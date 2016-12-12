# This is a script to load all the stuff necessary to run the Shiny R app 
# to serve a dashboard of site metrics.

# This script can be run from app.R with force_refresh == FALSE to
# initialize the Shiny R app, loading data from caches or APIs as needed.

# This script can also be run from another script with a force_refresh of TRUE
# to refresh all the caches in the background, avoiding the long startup time
# that some users would otherwise experience.

# prelude.R could eventually be broken down into separate caching/loading and 
# data munging/processing scripts.

options(stringsAsFactors = FALSE)

library(hash)
library(lubridate)
library(shiny)
library(shinyjs)
library(jsonlite)
library(googlesheets)
# Note that googlesheets depends on the R library openssl being installed,
# and on a Mac, "install.packages("openssl") fails unless openssl has 
# already been installed on the computer (e.g., by running
#    > brew install openssl
# ).

# Great documentation for googlesheets:
# https://rawgit.com/jennybc/googlesheets/master/vignettes/basic-usage.html
library(readxl)

#sparklines:
#http://bart6114.github.io/sparklines/
#library(sparklines) # Replaced by htmlwidgets/sparkline.
library(data.table)
library(DT)
library(htmlwidgets)
library(sparkline)
library(lattice)

#library(plyr) # It is reportedly better to load plyr before dplyr, 
# though I observe no difference.
library(dplyr) # data_frame comes from dplyr.
library(plyr) # Loaded to use the rename function (and override dplyr::rename).
# Otherwise, it's necessary to replace all instances of rename with
# plyr::rename.
library(httr)
#library(RGoogleAnalytics)
# Some online documentation for how to use RGoogleAnalytics is out of date.
# Consult demo(package="RGoogleAnalytics") instead.

library(RGA)
# RGA documentation is here:
# https://cran.r-project.org/web/packages/RGA/vignettes/authorize.html


class_key <- function(x,y) {
  # A simple helper function to concatenate together two strings,
  # separated by a semicolon.
  key <- paste(x,y,sep=";")
  return(key)
}

categorize_class_uses <- function(df) {
  # Return a data frame that counts number of uses of the Data Center
  # by semester and school. 
  
  # This function initially just computed the hash (uses_by_term)
  # and then reformatted it as a data frame to make it easier to
  # output a table. It's possible that this could now be refactored
  # to simplify the code.
  
  # The more conventionally R approach is 
  # to use a vectorized/functional approach, like:
  #    apply(c_uses_ws,1,categorize)
  # but I'm using a for loop for now.
  
  uses_by_term <- hash()
  insts <- c()
  ts <- c()
  for(i in 1:nrow(df)) {
    row <- df[i,]
    institution <- row$Institution
    term <- row$Semester
    if(!is.na(term) & !is.na(institution)) {
      if(!(grepl("CMU|Pitt",institution) > 0))
        institution <- "Other"
      if(!(institution %in% insts)) # Construct insts and ts as ordered unique
        insts <- append(insts,institution)   # lists of institutions and terms.
      if(!(term %in% ts))
        ts <- append(ts,term)
      key <- class_key(term,institution)
      if(!(key %in% keys(uses_by_term))){
        uses_by_term[[key]] <- 0
      }
      uses_by_term[[key]] <- uses_by_term[[key]]+1
    }
  }
  
  counts <- c()
  term_column <- c()
  inst_column <- c()
  for(i in 1:length(ts)) {
    for(j in 1:length(insts)) {
      term_column <- append(term_column,ts[i])
      inst_column <- append(inst_column,insts[j])
      u <- uses_by_term[[class_key(ts[i],insts[j])]]
      if(is.null(u)) 
        counts <- append(counts,0)
      else
        counts <- append(counts,u)
    }
  }
  
  df_out <- data.frame(term = term_column, institution = inst_column, count = counts)
  return(df_out)
}


reduce_to_category <- function(df,category) {
  # Takes the result from get_API_requests, filters down to only the resource
  # downloads (or the API calls, depending on the category-name string passed
  # to the function), and eliminates the then-unneeded "eventCategory" column.
  df <- df[df$eventCategory==category,]
  df <- df[,!(names(df) %in% c("eventCategory"))]
  return(df)
}

name_datasets <- function(df) {
  # Pull from the Data Center's CKAN API a list of all resources and use this to
  # obtain resource names, organizations, package names, and ID values to label
  # the Google Analytics statistics (which are listed by resource ID).
  json_file <- "https://data.wprdc.org/api/3/action/current_package_list_with_resources?limit=9999"
  json_data <- fromJSON(json_file)
  
  cached_resource_map_file <- "cached_resource_map.csv"
  
  result_df <- json_data$result
  
  if(exists("json_data")) {
    
    for(j in 1:nrow(result_df)) {
      package <- result_df[j,]
      #    if(!is.na(package$num_resources)) {
      #      count <- count + as.numeric(package$num_resources)
      #    } # ==> count = 607
      
      for(k in 1:length(package$resources[[1]]$id)) {
        resources <- package$resources[[1]]
        if((length(resources$name) > 0)) {
          if(is.na(resources$name[[k]])) {
            resource <- "Unnamed resource"
          } else {
            resource <- resources$name[[k]]
          }
          package_url_path <- c(paste("/dataset/",package$name,sep=""))
          if((length(resources$id) > 0)) {
            if(is.na(resources$id[[k]]) | !exists("package_url_path")) {
              resource_url_path <- "No resource path"
            } else {
              resource_url_path <- c(paste(package_url_path,"/resource/",
                                           resources$id[[k]],sep=""))
            }
          }
          if(j*k == 1) {
            resource_map <- data_frame(Package=c(package$title),
                                       Resource=c(resource),
                                       Organization=c(package$organization$title),
                                       id=c(resources$id[[k]]),
                                       package_id=c(resources$package_id[[k]]),
                                       package_path=c(package_url_path),
                                       resource_path=c(resource_url_path))
          } else {
            resource_map <- rbind(resource_map, 
                                  c(package$title,
                                    resource,
                                    package$organization$title,
                                    resources$id[[k]],
                                    resources$package_id[[k]],
                                    package_url_path,
                                    resource_url_path))
          }
        }
      }
    }
    
    # The next part could probably more simply be done with the merge function.
    resource_map$"30-day downloads" <- 0
    resource_map$"30-day unique downloads" <- 0
    #if(include_API_calls) {
    #  resource_map$"30-day API calls" <- 0
    #}
    resource_map$"All-time downloads" <- 0
    resource_map$"All-time unique downloads" <- 0
    if(include_API_calls) {
      resource_map$"All-time API calls" <- 0
    }
    for(i in 1:nrow(resource_map)) {
      if(resource_map$id[[i]] %in% df$eventLabel){
        matched_row <- df[df$eventLabel == resource_map$id[[i]],]
        resource_map$"30-day downloads"[[i]] <- matched_row$totalEvents.x
        resource_map$"30-day unique downloads"[[i]] <- matched_row$uniqueEvents.x
        #if(include_API_calls) {
        #  resource_map$"30-day API calls"[[i]] <- matched_row$"Month of API calls"  
        #}
        resource_map$"All-time downloads"[[i]] <- matched_row$totalEvents.y
        resource_map$"All-time unique downloads"[[i]] <- matched_row$uniqueEvents.y
        if(include_API_calls) {
          resource_map$"All-time API calls"[[i]] <- matched_row$"All API calls"  
        }
      } else {
        resource_map$"30-day downloads"[[i]] <- 0
        resource_map$"30-day unique downloads"[[i]] <- 0
        #if(include_API_calls) { # This is for the 30-day API calls.
        #  resource_map$"30-day API calls"[[i]] <- 0  
        #}
        resource_map$"All-time downloads"[[i]] <- 0
        resource_map$"All-time unique downloads"[[i]] <- 0
        if(include_API_calls) {
          resource_map$"All-time API calls"[[i]] <- 0  
        }
      }
    }
    write.csv(resource_map, cached_resource_map_file)
  } else if(file.exists(cached_resource_map_file)) {
    resource_map <- read.csv(cached_resource_map_file)
  } else {
    resource_map <- data.frame()
  }
  return(resource_map)
}

group_by_package <- function(df) {
  # Eliminate the resources and sum download counts over all resources
  # in a given package.
  
  # Again, there's surely a better way to do this. 
  # One option is to use the dplyr package:
  # http://www.statsblogs.com/2014/02/10/how-dplyr-replaced-my-most-common-r-idioms/
  package_list <- unique(df$Package)
  for(k in 1:length(package_list)) {
    matched_rows <- df[df$Package == package_list[[k]],]
    dl_1 <- sum(matched_rows$"30-day downloads")
    dl_1_unique <- sum(matched_rows$"30-day unique downloads")
    dl_all <- sum(matched_rows$"All-time downloads")
    dl_all_unique <- sum(matched_rows$"All-time unique downloads")
    if(k == 1) {
      grouped <- data_frame(Package=c(matched_rows$Package[[1]]),
                            Organization=c(matched_rows$Organization[[1]]),
                            "30-day downloads"=as.numeric(c(dl_1)),
                            "30-day unique downloads"=c(dl_1_unique),
                            "All-time downloads"=c(dl_all),
                            "All-time unique downloads"=c(dl_all_unique),
                            Resources=c(nrow(matched_rows)),
                            package_path=c(matched_rows$package_path[[1]]),
                            "Package ID"=c(matched_rows$package_id[[1]]))
    } else {
      grouped <- rbind(grouped, 
                       c(matched_rows$Package[[1]],
                         matched_rows$Organization[[1]],
                         dl_1,
                         dl_1_unique,
                         dl_all,
                         dl_all_unique,
                         nrow(matched_rows),
                         matched_rows$package_path[[1]],
                         matched_rows$package_id[[1]]))
    }
  }
  grouped$`30-day downloads` <- as.numeric(grouped$`30-day downloads`)
  grouped$`30-day unique downloads` <- as.numeric(grouped$`30-day unique downloads`)
  grouped$`All-time downloads` <- as.numeric(grouped$`All-time downloads`)
  grouped$`All-time unique downloads` <- as.numeric(grouped$`All-time unique downloads`)
  return(grouped)
}

generate_wide_dd <- function(monthly_df,id_field_name){
  mdd <- monthly_df
  names(mdd)[names(mdd)=="Year+month"] <- "ym" # Maybe define rename_dataframe_field(df,oldname,newname)
  names(mdd)[names(mdd)=="Year.month"] <- "ym"
  names(mdd)[names(mdd)==id_field_name] <- "id"
  names(mdd)[names(mdd)==gsub(" ", ".", id_field_name)] <- "id"
  wide_mdd <- dcast(mdd,id ~ ym,value.var="Downloads") # Excludes "Unique downloads"
  #as.list(as.data.frame(wdd))
  return(wide_mdd)
}
    
generate_history_frame <- function(wide_mdd){
  ids <- wide_mdd[[1]]
  
  number_of_months <- length(colnames(wide_mdd))
  x <- wide_mdd[c(2:number_of_months)]
  history_rows <- t(x)
  
  wide_mdd_length <- length(row.names(wide_mdd))
  #d0$Spark[1] <- list(history_rows[,1])
  
  for(k in 1:wide_mdd_length) {
    if(k == 1) {
      history_frame <- data_frame(id=ids[[k]],dl_history=0)
      #dl_history=as.vector(history_rows[,k]))
    } else {
      history_frame <- rbind(history_frame, 
                             c(ids[[k]],0))
      #as.vector(history_rows[,k])))
    } # I spent a huge amount of time trying to find a more R-ish way to do this, and 
    history_frame$dl_history[k] <- list(history_rows[,k]) # in the end, the only approach
  } # that I found that would work was creating a data frame with integer columns and 
  # only then substituting a vector of integers for the download history field.
  return(history_frame)
}

merge_history_with_df <- function(df,history_f,id_field_name,number_of_months) {
  # Add sparkline data
  df_with_sparks <- merge(df,history_f,
                          by.x=id_field_name,by.y="id",all.x = TRUE)
  df_with_sparks <- rename(df_with_sparks,
                           c("dl_history"="Monthly downloads"))
  
  df_with_sparks[is.na(df_with_sparks)] <- list(rep(0,number_of_months-1))
  return(df_with_sparks)  
}

reformat <- function(x) {paste(as.vector(x),collapse="|")}

downloadable_version <- function(df_sparks){
  # Set aside downloadable version of the dataframe
  download_df <- df_sparks[order(-df_sparks$"30-day downloads"),]
  download_df$`Monthly downloads` <- as.character(lapply(download_df$`Monthly downloads`,
                                                                 reformat))
  return(download_df)
}

make_datasparks_table <- function(df,fields,sparks_column){
  # Create jQuery version of dataframe with embedded sparklines
  d0 <- df[order(-df$"30-day downloads"),]
  
  d0 <- d0[,!(names(d0) %in% c("Downloads per pageview"))]
  d0 <- d0[fields]
  
  columnDefs = list(list(
    targets = c(sparks_column), # The column to convert from a vector/list/whatever into a sparkline.
    # Don't put the sparkline in the last column because otherwise many of the tooltips
    # will be outside the browser window (and therefore hidden).
    render = JS("function(data, type, full){
                return '<span class=spark>' + data + '</span>'           
}")
))
  
  fnDrawCallback = JS("function (oSettings, json) {
                      $('.spark:not(:has(canvas))').sparkline('html', {
                      type: 'bar',
                      highlightColor: 'orange'
                      });
                      }")

  d1 <- datatable(d0, options = list(
    columnDefs = columnDefs,
    fnDrawCallback = fnDrawCallback
  ), rownames= FALSE)
  d1$dependencies <- append(d1$dependencies, htmlwidgets:::getDependency('sparkline'))
  return(d1)
}

make_sparks_table_and_bare_df <- function(df_downloads_and_pageviews,
                                          monthly_downloads,fields,
                                          id_field_name, sparks_column) {
  # Take the base dataset and the one with the lists to be embedded in
  # individual rows (the histories of downloads by month) and return
  # 1) the jQuery table thing that has embedded sparklines and 
  # 2) a version that just has a pipe-delimited list in the 
  # corresponding history row (for downloading purposes).
  wide_mdd <- generate_wide_dd(monthly_downloads,id_field_name)
  number_of_months <- length(colnames(wide_mdd))
  history_frame <- generate_history_frame(wide_mdd)
  
  df_sparks <- merge_history_with_df(df_downloads_and_pageviews,
                                    history_frame,id_field_name,
                                    number_of_months)
  download_df <- downloadable_version(df_sparks)
  
  df_downloads_and_pageviews <- df_sparks[,!(names(df_sparks) %in% c(id_field_name))]
  
  d1 <- make_datasparks_table(df_downloads_and_pageviews,fields,sparks_column)
  return(list(d1,download_df))
}

prepend_Month <- function(df) {
  # Since there are a bunch of Google Sheets sheets with date ranges like
  # "2015 Oct 15 - Nov 14", which are unsortable, the solution we've come up
  # with is to convert the rownames (which are dumbly string-versions of integers
  # by default) into integers to represent the month.
  Month <- as.integer(rownames(df))
  if(df$Date[1] == "10/15 launch") {
    Month <- Month - 1 
  }
  return(cbind(Month,df))
}

reverse_sort_by_Month <- function(df) {
  return(df[order(df$"Month",decreasing=TRUE),])
}
stacked_barplot_matrix <- function(v) {
  data <- matrix(c(v[seq(1,length(v)-1)],0*v,v[c(length(v))]),nrow=2,byrow=T)
  return(data)
}

adjust_axis_and_plot <- function(mtrx,y_label,month_list,hues) {
  # R's barplot function has a weird tendency to make the y-axis too short
  # (if you want every point in the plot to be within the y-axis range).
  # If you try to make the scale (0,4500), R rounds it down to (0,4000),
  # presumably because 4500 = 9*500 (9*y_units) and R prefers multiples of
  # like 4, 5, or 6, so the tick marks can be distributed with nice round
  # spacings. It likes 1000 as a maximum, since 1000 = 5*200.
  # It doesn't like 24000 as a maximum for a value of 20730 (rounding it
  # down to 20000), but it does like 25000.
  # Going with 10^(floor(log10(y_max)))/2 works OK so far, but for 4258, it 
  # only results in 4000 being used as the maximum.
  
  # I eventually "solved" this by switching from 
  #       y_units_1 <- 10^(floor(log10(y_max)))
  # to
  #       y_units_1 <- 10^(round(log10(y_max)))
  # though this does result in a maximum of 5000 for a value of 4258.
  y_max <- max(mtrx)
  #y_units_1 <- 10^(floor(log10(y_max)))
  y_units_1 <- 10^(round(log10(y_max)))
  y_units_2 <- y_units_1/2
  y_lim_1 <- y_units_1*ceiling(y_max/y_units_1)
  y_lim_2 <- y_units_2*ceiling(y_max/y_units_2)
  y_lim <- y_lim_2
  barplot(mtrx,names.arg=month_list,ylab=y_label,xlab="",
          col=hues,cex.names=1.5,cex.lab=1.5,ylim=c(0,y_lim))
}


within_n_days_of <- function(df,n,last_date) {
  difference_in_days <- today-as.Date(df$Date,"%m/%d/%Y")
  df <- df[difference_in_days <= n,]
  return(df)
}

eliminate_empty_fields <- function(df,field_list) {
  # Return a dataframe containing only the rows where the fields
  # in field_list are not NA values.
  return(df[complete.cases(df[,field_list]),])
}

################# MOSTLY FUNCTIONS ABOVE THIS LINE ###################

cached_mode <- FALSE
include_API_calls <- TRUE 

if(!cached_mode) {
  source("get_data.R") # Load all the functions that get 
  # Google Analytics data and the sheet_key use to access
  # the Google Docs spreadsheet.
}

today <- Sys.Date()
yesterday <- today - days(x=1)
if(!production) {
    setwd("/Users/drw/WPRDC/Dashboards/Shiny/WPRDC-dashboard")
} else {
  source("setwd_to_file_location.R")
}

cached_metrics_file = "cached_metrics_sheet.xlsx"
refresh_google_sheets_data <- force_refresh | refresh_boolean(cached_metrics_file,59.11,cached_mode)

if((!cached_mode) & (refresh_google_sheets_data)) {
  if(production) {
    googlesheets::gs_auth(token = "shiny_app_token.rds", cache = FALSE)
  }
  if(exists("metrics")) {
    rm(metrics)
  }
  metrics <- gs_key(sheet_key) # Access Performance Management spreadsheet
  if(exists("metrics")) {
    metrics %>% gs_download(to = cached_metrics_file, overwrite = TRUE)
  }
}

site_stats_cache_file = "cached_site_stats.csv"
refresh_site_stats <- refresh_boolean(site_stats_cache_file,24*60,cached_mode)
refresh_site_stats <- refresh_site_stats | force_refresh | (hour(Sys.time()) == 6)
refresh_site_stats <- refresh_site_stats | !production

site_stats <- refresh_it(get_site_stats,
                         refresh_site_stats,
                         site_stats_cache_file)

if(is.null(site_stats)) {
  site_stats <- read_excel(cached_metrics_file, sheet = "(dashboard | site stats)")
  site_stats$`average session duration (minutes)` <- site_stats$`average session duration (seconds)`/60
  site_stats$`average session duration (minutes)` <- round(x=site_stats$`average session duration (minutes)`, digits = 2)
  site_stats <- site_stats[,!(names(site_stats) %in% c("average session duration (seconds)"))]
  site_stats$`pageviews per session` <- round(x=site_stats$`pageviews per session`, digits = 2)
  site_stats <- site_stats[,!(names(site_stats) %in% c("year_month"))]
}
month_list <- paste(month.abb[site_stats$month],site_stats$year,sep=" ")
numerical_month_list <- paste(site_stats$year,sprintf("%02d", site_stats$month),sep="/")
year_month <- numerical_month_list
site_stats <- cbind(year_month,site_stats[,!(names(site_stats) %in% c("X","year","month","Year+month","Year.month"))])
site_stats <- rename(site_stats, c("year_month"="year/month"))
site_stats <- rename(site_stats, 
                     c("pageviews.per.session"="pageviews per session", 
                       "average.session.duration..minutes."="average session duration (minutes)"))

site_stats_reversed <- site_stats[order(site_stats$"year/month",decreasing=TRUE),]

#site_stats$year_month <- numerical_month_list
year_months <- substr(seq.Date(as.Date("2015-10-01"),today,by="1 month"),1,7)
# produces a list like "2015-10" "2015-11" "2015-12" ...
# Add these to the spreadsheet as a "year_month" column to search for.


monthly_downloads_cache <- "monthly_resource_downloads.csv"
refresh_md <- refresh_boolean(monthly_downloads_cache,24*60,cached_mode)
refresh_md <- refresh_md | force_refresh | (hour(Sys.time()) == 6)
refresh_md <- refresh_md | !production
monthly_resource_downloads <- refresh_it(get_monthly_resource_downloads,
                                        refresh_md,
                                        monthly_downloads_cache)

monthly_package_downloads_cache <- "monthly_package_downloads.csv"
refresh_mpd <- refresh_boolean(monthly_package_downloads_cache,24*60,cached_mode)
refresh_mpd <- refresh_mpd | force_refresh | (hour(Sys.time()) == 6)
refresh_mpd <- refresh_mpd | !production
monthly_package_downloads <- refresh_it(get_monthly_package_downloads,
                                        refresh_mpd,
                                        monthly_package_downloads_cache)

resource_d_and_p_file <- "df_downloads_and_pageviews.csv"
package_d_and_p_file <- "package_downloads_and_pageviews.csv"
refresh_resource_info <- refresh_boolean(resource_d_and_p_file,30,cached_mode)
refresh_package_info <- refresh_boolean(package_d_and_p_file,30,cached_mode)

refresh_download_data <- refresh_resource_info | refresh_package_info | force_refresh

if(refresh_download_data) {
  API_requests_month <- get_API_requests_gar(today-days(x=30),yesterday,p_Id,production)
  #  API_requests_month <- get_API_requests("30daysAgo","yesterday",p_Id,client_id,client_secret,production)
  #API_requests_month <- get_API_requests_r_goo(today-days(x=30),yesterday,p_Id,client_id,client_secret,production)
  downloads_per_month <- reduce_to_category(API_requests_month,"CKAN Resource Download Request") #This reduces to downloads
  
#  if(include_API_calls) { # This is for the 30-day API calls.
#    API_calls_per_month <- reduce_to_category(API_requests_month,"CKAN API Request")
#    API_calls_per_month <- rename(API_calls_per_month,
#                                  c("totalEvents"="Month of API calls","uniqueEvents"="Month of unique API calls"))
#  }
  all_API_requests <- get_API_requests_gar("2015-10-15",yesterday,p_Id,production)
#  all_API_requests <- get_API_requests("2015-10-15","yesterday",p_Id,client_id,client_secret,production)
  #all_API_requests <- get_API_requests_r_goo("2015-10-15",yesterday,p_Id,client_id,client_secret,production)
  all_downloads <- reduce_to_category(all_API_requests,"CKAN Resource Download Request")

  if(include_API_calls) {
    all_API_calls <- reduce_to_category(all_API_requests,"CKAN API Request")
    all_API_calls <- rename(all_API_calls,
                            c("totalEvents"="All API calls","uniqueEvents"="All unique API calls"))
  }  
    
  df_downloads <- merge(downloads_per_month,all_downloads,by="eventLabel",all=TRUE)
  # Merging with all=TRUE makes this an outer join. Missing values are filled in by NAs.
  df_downloads[is.na(df_downloads)] <- 0 # NAs can be switched to zeros using this line.
  if(include_API_calls) {
    #df_downloads <- merge(df_downloads,API_calls_per_month,by="eventLabel",all=TRUE)
    df_downloads <- merge(df_downloads,all_API_calls,by="eventLabel",all.x=TRUE)
    df_downloads[is.na(df_downloads)] <- 0
  }
  df_downloads <- name_datasets(df_downloads)
#  df_downloads <- df_downloads[,!(names(df_downloads) %in% c("id","package_id"))]
  df_downloads <- df_downloads[,!(names(df_downloads) %in% c("id"))]
  df_pageviews_month <- get_pageviews_gar(today-days(x=30),yesterday,p_Id,production)
  df_pageviews_all <- get_pageviews_gar("2015-10-15",yesterday,p_Id,production)
  df_downloads_and_pageviews <- merge(df_downloads,df_pageviews_month,
                                      by.x="resource_path",by.y="pagePath",all.x=TRUE)
  # Making the above merge an all.x=TRUE merge eliminates a lot of paths that 
  # are not resources.
  df_downloads_and_pageviews[is.na(df_downloads_and_pageviews)] <- 0
  df_downloads_and_pageviews <- rename(df_downloads_and_pageviews,
                         c("pageviews"="30-day pageviews"))
  df_downloads_and_pageviews <- merge(df_downloads_and_pageviews,df_pageviews_all,
                                      by.x="resource_path",by.y="pagePath",all.x=TRUE)
  df_downloads_and_pageviews[is.na(df_downloads_and_pageviews)] <- 0
  df_downloads_and_pageviews <- rename(df_downloads_and_pageviews,
                                       c("pageviews"="All-time pageviews"))
  #df_downloads_and_pageviews$`Downloads per pageview` <- format(df_downloads_and_pageviews$`All-time unique downloads`/df_downloads_and_pageviews$`All-time pageviews`,digits=2)
  #The "downloads per pageview" metric problem: Downloads have not been 
  #tracked as long as pageviews have, which skews this metric based on the age of the dataset.
  #df_downloads_and_pageviews$`Downloads per pageview` <- format(df_downloads_and_pageviews$`30-day unique downloads`/df_downloads_and_pageviews$`30-day pageviews`,digits=1)
  
  resource_ids <- as.vector(mapply(function(x) gsub(".*\\/", "",x),df_downloads_and_pageviews$resource_path))
  df_downloads_and_pageviews$`Resource ID` <- resource_ids
  df_downloads_and_pageviews <- df_downloads_and_pageviews[c("Package","Resource",
                                                             "Organization",
                                                             "30-day downloads",
                                                             "30-day unique downloads",
                                                             "30-day pageviews",
                                                             "All-time downloads",
                                                             "All-time unique downloads",
                                                             "All-time pageviews",
                                                             "All-time API calls",
                                                             #"Downloads per pageview",
                                                             "Resource ID")]
  
  write.csv(df_downloads_and_pageviews, resource_d_and_p_file, row.names=FALSE)
  
  downloads_by_package <- group_by_package(df_downloads)
  package_downloads_and_pageviews <- merge(downloads_by_package,df_pageviews_month,
                                      by.x="package_path",by.y="pagePath",all.x=TRUE)
  package_downloads_and_pageviews[is.na(package_downloads_and_pageviews)] <- 0
  package_downloads_and_pageviews <- rename(package_downloads_and_pageviews,
                                       c("pageviews"="30-day pageviews"))
  package_downloads_and_pageviews <- merge(package_downloads_and_pageviews,df_pageviews_all,
                                           by.x="package_path",by.y="pagePath",all.x=TRUE)
  package_downloads_and_pageviews[is.na(package_downloads_and_pageviews)] <- 0
  package_downloads_and_pageviews <- rename(package_downloads_and_pageviews,
                                            c("pageviews"="All-time pageviews"))
  
  package_downloads_and_pageviews <- package_downloads_and_pageviews[c("Package",
                                                             "Organization",
                                                             "30-day downloads",
                                                             "30-day unique downloads",
                                                             "30-day pageviews",
                                                             "All-time downloads",
                                                             "All-time unique downloads",
                                                             "All-time pageviews",
#                                                             "All-time API calls",
                                                             "Resources",
                                                             "Package ID")]
  
  
  write.csv(package_downloads_and_pageviews, package_d_and_p_file, row.names=FALSE)

} else {
  df_downloads_and_pageviews <- read.csv(resource_d_and_p_file)
  df_downloads_and_pageviews <- rename(df_downloads_and_pageviews, 
                                 c("X30.day.downloads"="30-day downloads", 
                                   "X30.day.unique.downloads"="30-day unique downloads", 
                                   "X30.day.pageviews"="30-day pageviews", 
                                   "All.time.downloads"="All-time downloads", 
                                   "All.time.unique.downloads"="All-time unique downloads",
                                   "All.time.pageviews"="All-time pageviews",
                                   "All.time.API.calls"="All-time API calls",
                                   #"Downloads.per.pageview"="Downloads per pageview",
                                   "Resource.ID"="Resource ID"))
  
  package_downloads_and_pageviews <- read.csv(package_d_and_p_file)
  package_downloads_and_pageviews <- rename(package_downloads_and_pageviews, 
                                 c("X30.day.downloads"="30-day downloads", 
                                   "X30.day.unique.downloads"="30-day unique downloads", 
                                   "X30.day.pageviews"="30-day pageviews", 
                                   "All.time.downloads"="All-time downloads", 
                                   "All.time.unique.downloads"="All-time unique downloads",
                                   "All.time.pageviews"="All-time pageviews",
                                   "Package.ID"="Package ID"))#,
                                   #"All.time.API.calls"="All-time API calls"))
}

#### Fancy manipulations to enable embedding of sparklines depicting dataset download histories.
# How to add a column containing lists in R (which R makes difficult):
# df$c <- list(c(0),c(1,4,9)) 

# How to change a column that's numeric, so that one entry is a list:
# df$g[1] <- list(c(5.4,3.2))

#d0$Spark <- 0                                   # This
#d0$Spark[1] <- list(c(-3,2,-1,1,0,1,1,2,3,5,8)) # works!

##### Generate sparkline-embedded table for the dashboard and modified 
##### dataframe for downloading.
id_field_name <- "Resource ID"
sparks_column <- 3
fields <- c("Package","Resource",
            "Organization",
            "Monthly downloads",
            "30-day downloads",
            "30-day unique downloads",
            "30-day pageviews",
            "All-time downloads",
            "All-time unique downloads",
            "All-time pageviews",
            "All-time API calls")

returned_list <- make_sparks_table_and_bare_df(df_downloads_and_pageviews,
                                               monthly_resource_downloads, 
                                               fields,
                                               id_field_name, 
                                               sparks_column)

d1 <- returned_list[[1]]
resource_download_df <- returned_list[[2]]

# Relabel some columns
d1 <- rename(d1,c("Monthly downloads"="Monthly downloads*","All-time API calls"="All-time API calls**"))

the_downloads_table <- d1
##### OUTPUTS USED BY app.R: the_downloads_table, resource_download_df

# Likewise for packages:
id_field_name <- "Package ID"
sparks_column <- 2
fields <- c("Package",
            "Organization",
            "Monthly downloads",
            "30-day downloads",
            "30-day unique downloads",
            "30-day pageviews",
            "All-time downloads",
            "All-time unique downloads",
            "All-time pageviews",
            "Resources")

returned_list <- make_sparks_table_and_bare_df(package_downloads_and_pageviews,
                                               monthly_package_downloads, 
                                               fields,
                                               id_field_name, 
                                               sparks_column)

d2 <- returned_list[[1]]
package_download_df <- returned_list[[2]]

# Relabel some columns
d2 <- rename(d2,c("Monthly downloads"="Monthly downloads*","All-time API calls"="All-time API calls**"))

package_downloads_table <- d2


# [ ] Figure out how to trigger a reloading of all data... Maybe reboot Shiny every 30 minutes?
# When RGA samples a metric every day (using the fetch.by = "day" option), 
# it is extremely slow since it breaks the query down into many queries that
# are then batched but still run at 10 samples/second since that is the Google
# API's limit per IP address.

# Batching Google Analytics API requests was just added in September 2015:
# https://developers.google.com/analytics/devguides/reporting/core/v3/batching

# Conclusion: It's possible to pull data directly from Google Analytics through
# R, but either 1) keeping the data in the Google Docs spreadsheet and pulling 
# it from there or else 2) using the Google Analytics Embed API (inserting 
# JavaScript) into the R Shiny dashboard) should be faster.

c_uses_ws <- read_excel(cached_metrics_file, sheet = "Classroom Uses")
#The column labelled "Average Session Duration (minutes)" gets screwed up
#for two reasons: 1) The colon separating minutes and seconds is misinterpreted
#when importing into R. 2) The parentheses from the column header are 
#silently dropped.
c_uses_ws <- eliminate_empty_fields(c_uses_ws,c("Semester"))

pitt_uses <- nrow(c_uses_ws[c_uses_ws$Institution == "Pitt",])
cmu_uses <- nrow(c_uses_ws[grep("CMU",c_uses_ws$Institution),])
classroom_uses <- nrow(c_uses_ws)

df_uses <- categorize_class_uses(c_uses_ws)
index <- as.integer(rownames(df_uses))
df_uses <- cbind(index,df_uses)
df_uses <- df_uses[order(df_uses$"index",decreasing=TRUE),]


other_web_stats <- read_excel(cached_metrics_file, sheet = "Other Web Stats")
other_web_stats <- other_web_stats[-c(4,5),] # Eliminating rows 3 and 4 (which 
# do not contain one month of data) and any rows that contain NA values.
publishers <- other_web_stats[,(names(other_web_stats) %in% 
                                  c("Date","Total Publishers","Academic Publishers",
                                    "Government Publishers","Non-Profit Publishers",
                                    "Other Publishers"))]
publishers <- reverse_sort_by_Month(prepend_Month(publishers))
publishers <- eliminate_empty_fields(publishers,c("Date","Total Publishers"))

etl_process_count <- other_web_stats[,(names(other_web_stats) %in% 
                                         c("Data with Automated ETL (Non GIS)"))]
etl_processes <- other_web_stats[,(names(other_web_stats) %in% 
                                     c("Date","Data with Automated ETL (Non GIS)",
                                       "Automated ETL list"))]
etl_processes <- rename(etl_processes, 
                     c("Data with Automated ETL (Non GIS)"="Datasets with Automated Imports (Non-GIS)", 
                       "Automated ETL list"="List of Datasets with Automated Import Processes"))
etl_processes <- etl_processes[-c(1),] # Elimate first row
etl_processes <- reverse_sort_by_Month(prepend_Month(etl_processes))
na.omit(etl_processes)

misc_other_stats <- other_web_stats[,(names(other_web_stats) %in% 
                                        c("Date","Discussion Posts",
                                          "CKAN data requests"))]
misc_other_stats <- misc_other_stats[-c(1),]
misc_other_stats[,2:3] <- sapply(misc_other_stats[, 2:3], as.integer)
misc_other_stats <- reverse_sort_by_Month(prepend_Month(misc_other_stats))
na.omit(misc_other_stats)

social_media <- read_excel(cached_metrics_file, sheet = "Social Media")
social_media <- social_media[-c(3,4),]
twitter_followers <- social_media[,(names(social_media) %in% c("Period","Twitter Followers, End of period"))]
Month <- as.integer(rownames(twitter_followers))
twitter_followers <- reverse_sort_by_Month(cbind(Month,twitter_followers))
na.omit(twitter_followers)

media <- read_excel(cached_metrics_file, sheet = "Media")
media_mentions <- nrow(media) - 1
outreach_events_table <- read_excel(cached_metrics_file, sheet = "Project Outreach & Events")
outreach_and_events <- nrow(outreach_events_table) - 1
outreach_fields <- outreach_events_table[1,]
colnames(outreach_events_table) <- as.list(outreach_events_table[1,])
outreach_events_table <- outreach_events_table[-c(1),]
proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))) # inline function
outreach_events_table$Type <- proper(outreach_events_table$Type)
oet <- outreach_events_table
outreach_events_table <- eliminate_empty_fields(outreach_events_table,c("Date","Type"))
#df <- outreach_events_table
#field_list <- c("Date","Type")
#outreach_events_table <- df[complete.cases(df[,field_list]),]

# Due to a known deficiency in R and readxl (and really Excel 
# for being weird about forcing dates to become datetimes), the 
# first column of the Project Outreach & Events database, which 
# should look like
#   10/15/15
#   10/15/15
#   10/16/15
# instead looks like
#   42292.0
#   42292.0
#   42293.0
# where that number is the number of days since January 1, 1900.
# That is, October 15th is the 288th day of 2015, and
# (2015-1900)*365.25+288 = 42292, though it does seem to
# incorrectly assume that 1900 is a leap year, and there's one other
# day unaccounted for in there.

#outreach_events_table$Date <- as.Date(outreach_events_table$Date,as.Date("1899-12-30"))
#recent_events <- within_n_days_of(outreach_events_table,90,today)
outreach_events_table$Date <- as.numeric(outreach_events_table$Date)
day_number <- difftime(Sys.Date(), as.Date("1900-01-01"), units = c("days"))+2
recent_events <- outreach_events_table[(day_number-as.numeric(outreach_events_table$Date)) <= 90,]

# Prepare matrices for presenting users/sessions/pageviews plots.
sbm_users <- stacked_barplot_matrix(site_stats$users)
sbm_sessions <- stacked_barplot_matrix(site_stats$sessions)
sbm_pageviews <- stacked_barplot_matrix(site_stats$pageviews)

