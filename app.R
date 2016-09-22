# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

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

suppressMessages(library(dplyr))
library(plyr) # Loaded to use the rename function.
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
            dataset <- "Unnamed resource"
          } else {
            dataset <- resources$name[[k]]
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
                                       Dataset=c(dataset),
                                       Organization=c(package$organization$title),
                                       id=c(resources$id[[k]]),
                                       package_id=c(resources$package_id[[k]]),
                                       package_path=c(package_url_path),
                                       resource_path=c(resource_url_path))
          } else {
            resource_map <- rbind(resource_map, 
                                  c(package$title,
                                    dataset,
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
    resource_map$"1-month downloads" <- 0
    resource_map$"1-month unique downloads" <- 0
    if(include_API_calls) {
      resource_map$"1-month API calls" <- 0
    }
    resource_map$"All-time downloads" <- 0
    resource_map$"All-time unique downloads" <- 0
    if(include_API_calls) {
      resource_map$"All-time API calls" <- 0
    }
    for(i in 1:nrow(resource_map)) {
      if(resource_map$id[[i]] %in% df$eventLabel){
        matched_row <- df[df$eventLabel == resource_map$id[[i]],]
        resource_map$"1-month downloads"[[i]] <- matched_row$totalEvents.x
        resource_map$"1-month unique downloads"[[i]] <- matched_row$uniqueEvents.x
        if(include_API_calls) {
          resource_map$"1-month API calls"[[i]] <- matched_row$"Month of API calls"  
        }
        resource_map$"All-time downloads"[[i]] <- matched_row$totalEvents.y
        resource_map$"All-time unique downloads"[[i]] <- matched_row$uniqueEvents.y
        if(include_API_calls) {
          resource_map$"All-time API calls"[[i]] <- matched_row$"All API calls"  
        }
      } else {
        resource_map$"1-month downloads"[[i]] <- 0
        resource_map$"1-month unique downloads"[[i]] <- 0
        if(include_API_calls) {
          resource_map$"1-month API calls"[[i]] <- 0  
        }
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
  # Eliminate the datasets and sum download counts over all datasets
  # in a given package.
  
  # Again, there's surely a better way to do this. 
  # One option is to use the dplyr package:
  # http://www.statsblogs.com/2014/02/10/how-dplyr-replaced-my-most-common-r-idioms/
  package_list <- unique(df$Package)
  for(k in 1:length(package_list)) {
    matched_rows <- df[df$Package == package_list[[k]],]
    dl_1 <- sum(matched_rows$"1-month downloads")
    dl_1_unique <- sum(matched_rows$"1-month unique downloads")
    dl_all <- sum(matched_rows$"All-time downloads")
    dl_all_unique <- sum(matched_rows$"All-time unique downloads")
    if(k == 1) {
      grouped <- data_frame(Package=c(matched_rows$Package[[1]]),
                            Organization=c(matched_rows$Organization[[1]]),
                            "1-month downloads"=as.numeric(c(dl_1)),
                            "1-month unique downloads"=c(dl_1_unique),
                            "All-time downloads"=c(dl_all),
                            "All-time unique downloads"=c(dl_all_unique),
                            Resources=c(nrow(matched_rows)),
                            package_path=c(matched_rows$package_path[[1]]))
    } else {
      grouped <- rbind(grouped, 
                       c(matched_rows$Package[[1]],
                         matched_rows$Organization[[1]],
                         dl_1,
                         dl_1_unique,
                         dl_all,
                         dl_all_unique,
                         nrow(matched_rows),
                         matched_rows$package_path[[1]]))
    }
  }
  grouped$`1-month downloads` <- as.numeric(grouped$`1-month downloads`)
  grouped$`1-month unique downloads` <- as.numeric(grouped$`1-month unique downloads`)
  grouped$`All-time downloads` <- as.numeric(grouped$`All-time downloads`)
  grouped$`All-time unique downloads` <- as.numeric(grouped$`All-time unique downloads`)
  return(grouped)
}

within_n_days_of <- function(df,n,last_date) {
  difference_in_days <- today-as.Date(df$Date,"%m/%d/%Y")
  df <- df[difference_in_days <= n,]
  return(df)
}

cached_mode <- FALSE
include_API_calls <- FALSE # Switching this will conflict with a cached version of 
# the downloaded data, so eliminate this before deploying.

if(!cached_mode) {
  source("get_data.R") # Load all the functions that get 
  # Google Analytics data and the sheet_key use to access
  # the Google Docs spreadsheet.
}

today <- Sys.Date()
yesterday <- today - days(x=1)
if(!cached_mode) {
  if(production) {
    if(file.exists("/srv/shiny-server/metrics")) {
      setwd("/srv/shiny-server/metrics")
    }
  }
}
#df_analytics <- get_analytics(2015,10,p_Id,client_id,client_secret,production)

cached_metrics_file = "cached_metrics_sheet.xlsx"
if(!cached_mode) {
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

site_stats <- read_excel(cached_metrics_file, sheet = "(dashboard | site stats)")
site_stats$`average session duration (minutes)` <- site_stats$`average session duration (seconds)`/60
site_stats$`average session duration (minutes)` <- round(x=site_stats$`average session duration (minutes)`, digits = 2)
site_stats <- site_stats[,!(names(site_stats) %in% c("average session duration (seconds)"))]
site_stats$`pageviews per session` <- round(x=site_stats$`pageviews per session`, digits = 2)
month_list <- paste(month.abb[site_stats$month],site_stats$year,sep=" ")
numerical_month_list <- paste(site_stats$year,site_stats$month,sep="-")
site_stats$year_month <- numerical_month_list
year_months <- substr(seq.Date(as.Date("2015-10-01"),today,by="1 month"),1,7)
# produces a list like "2015-10" "2015-11" "2015-12" ...
# Add these to the spreadsheet as a "year_month" column to search for.

refresh_download_data <- FALSE
if(!file.exists("df_downloads_and_pageviews.csv")) {
  refresh_download_data <- TRUE
} else if (!file.exists("package_downloads_and_pageviews.csv")) {
  refresh_download_data <- TRUE
} else if(!cached_mode) {
  if(Sys.time()-dminutes(15) > c(file.info("df_downloads_and_pageviews.csv")$mtime)) {
    refresh_download_data <- TRUE
  }
  if(Sys.time()-dminutes(15) > c(file.info("package_downloads_and_pageviews.csv")$mtime)) {
    refresh_download_data <- TRUE
  }
}

if(refresh_download_data) {
  API_requests_month <- get_API_requests_gar(today-days(x=30),yesterday,p_Id,production)
  #  API_requests_month <- get_API_requests("30daysAgo","yesterday",p_Id,client_id,client_secret,production)
  #API_requests_month <- get_API_requests_r_goo(today-days(x=30),yesterday,p_Id,client_id,client_secret,production)
  downloads_per_month <- reduce_to_category(API_requests_month,"CKAN Resource Download Request") #This reduces to downloads
  
  if(include_API_calls) {
    API_calls_per_month <- reduce_to_category(API_requests_month,"CKAN API Request")
    API_calls_per_month <- rename(API_calls_per_month,
                                  c("totalEvents"="Month of API calls","uniqueEvents"="Month of unique API calls"))
  }
  all_API_requests <- get_API_requests_gar("2015-10-15",yesterday,p_Id,production)
#  all_API_requests <- get_API_requests("2015-10-15","yesterday",p_Id,client_id,client_secret,production)
  #all_API_requests <- get_API_requests_r_goo("2015-10-15",yesterday,p_Id,client_id,client_secret,production)
  all_downloads <- reduce_to_category(all_API_requests,"CKAN Resource Download Request")

  if(include_API_calls) {
    all_API_calls <- reduce_to_category(all_API_requests,"CKAN API Request")
    all_API_calls <- rename(all_API_calls,
                            c("totalEvents"="All API calls","uniqueEvents"="All unique API calls"))
  }  
    
  df_downloads <- merge(downloads_per_month,all_downloads,by="eventLabel")
  if(include_API_calls) {
    df_downloads <- merge(df_downloads,API_calls_per_month,by="eventLabel")
    df_downloads <- merge(df_downloads,all_API_calls,by="eventLabel")
  }
  df_downloads <- name_datasets(df_downloads)
  df_downloads <- df_downloads[,!(names(df_downloads) %in% c("id","package_id"))]
  df_pageviews_month <- get_pageviews_gar(today-days(x=30),yesterday,p_Id,production)
#  df_pageviews_month <- get_pageviews("30daysAgo","yesterday",p_Id,client_id,client_secret,production)
  df_pageviews_all <- get_pageviews_gar("2015-10-15",yesterday,p_Id,production)
#  df_pageviews_all <- get_pageviews("2015-10-15","yesterday",p_Id,client_id,client_secret,production)
  df_downloads_and_pageviews <- merge(df_downloads,df_pageviews_month,
                                      by.x="resource_path",by.y="pagePath")
  df_downloads_and_pageviews <- rename(df_downloads_and_pageviews,
                         c("pageviews"="1-month pageviews"))
  df_downloads_and_pageviews <- merge(df_downloads_and_pageviews,df_pageviews_all,
                                      by.x="resource_path",by.y="pagePath")
  df_downloads_and_pageviews <- rename(df_downloads_and_pageviews,
                                       c("pageviews"="All-time pageviews"))
  df_downloads_and_pageviews <- df_downloads_and_pageviews[c("Package","Dataset",
                                                             "Organization",
                                                             "1-month downloads",
                                                             "1-month unique downloads",
                                                             "1-month pageviews",
                                                             "All-time downloads",
                                                             "All-time unique downloads",
                                                             "All-time pageviews")]
  
#  write.csv(df_downloads, "df_downloads.csv", row.names=FALSE)
  write.csv(df_downloads_and_pageviews, "df_downloads_and_pageviews.csv", row.names=FALSE)
  
  downloads_by_package <- group_by_package(df_downloads)
  package_downloads_and_pageviews <- merge(downloads_by_package,df_pageviews_month,
                                      by.x="package_path",by.y="pagePath")
  package_downloads_and_pageviews <- rename(package_downloads_and_pageviews,
                                       c("pageviews"="1-month pageviews"))
  package_downloads_and_pageviews <- merge(package_downloads_and_pageviews,df_pageviews_all,
                                           by.x="package_path",by.y="pagePath")
  package_downloads_and_pageviews <- rename(package_downloads_and_pageviews,
                                            c("pageviews"="All-time pageviews"))
  
  package_downloads_and_pageviews <- package_downloads_and_pageviews[c("Package",
                                                             "Organization",
                                                             "1-month downloads",
                                                             "1-month unique downloads",
                                                             "1-month pageviews",
                                                             "All-time downloads",
                                                             "All-time unique downloads",
                                                             "All-time pageviews",
                                                             "Resources")]
  
  
#  write.csv(downloads_by_package, "downloads_by_package.csv", row.names=FALSE)
  write.csv(package_downloads_and_pageviews, "package_downloads_and_pageviews.csv", row.names=FALSE)
} else {
  df_downloads_and_pageviews <- read.csv("df_downloads_and_pageviews.csv")
  df_downloads_and_pageviews <- rename(df_downloads_and_pageviews, 
                                 c("X1.month.downloads"="1-month downloads", 
                                   "X1.month.unique.downloads"="1-month unique downloads", 
                                   "X1.month.pageviews"="1-month pageviews", 
                                   "All.time.downloads"="All-time downloads", 
                                   "All.time.unique.downloads"="All-time unique downloads",
                                   "All.time.pageviews"="All-time pageviews"))
  
  package_downloads_and_pageviews <- read.csv("package_downloads_and_pageviews.csv")
  package_downloads_and_pageviews <- rename(package_downloads_and_pageviews, 
                                 c("X1.month.downloads"="1-month downloads", 
                                   "X1.month.unique.downloads"="1-month unique downloads", 
                                   "X1.month.pageviews"="1-month pageviews", 
                                   "All.time.downloads"="All-time downloads", 
                                   "All.time.unique.downloads"="All-time unique downloads",
                                   "All.time.pageviews"="All-time pageviews"))
}

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
gadp <- read_excel(cached_metrics_file, sheet = "Google analytics Data Portal")
gadp$`Pages/Session` <- as.numeric(as.character(gadp$`Pages/Session`))
#The column labelled "Average Session Duration (minutes)" gets screwed up
#for two reasons: 1) The colon separating minutes and seconds is misinterpreted
#when importing into R. 2) The parentheses from the column header are 
#silently dropped.

pitt_uses <- nrow(c_uses_ws[c_uses_ws$Institution == "Pitt",])
cmu_uses <- nrow(c_uses_ws[grep("CMU",c_uses_ws$Institution),])
classroom_uses <- nrow(c_uses_ws)

df_uses <- categorize_class_uses(c_uses_ws)

other_web_stats <- read_excel(cached_metrics_file, sheet = "Other Web Stats")
other_web_stats <- other_web_stats[-c(4,5),] # Eliminating rows 3 and 4 (which 
# do not contain one month of data) and any rows that contain NA values.
publishers <- other_web_stats[,(names(other_web_stats) %in% 
                                  c("Date","Total Publishers","Academic Publishers",
                                    "Government Publishers","Non-Profit Publishers",
                                    "Other Publishers"))]

etl_process_count <- other_web_stats[,(names(other_web_stats) %in% 
                                         c("Data with Automated ETL (Non GIS)"))]
etl_processes <- other_web_stats[,(names(other_web_stats) %in% 
                                     c("Date","Data with Automated ETL (Non GIS)",
                                       "Automated ETL list"))]
etl_processes <- etl_processes[-c(1),]

misc_other_stats <- other_web_stats[,(names(other_web_stats) %in% 
                                        c("Date","Discussion Posts",
                                          "CKAN data requests"))]
misc_other_stats <- misc_other_stats[-c(1),]
misc_other_stats[,2:3] <- sapply(misc_other_stats[, 2:3], as.integer)

social_media <- read_excel(cached_metrics_file, sheet = "Social Media")
social_media <- social_media[-c(3,4),]
twitter_followers <- social_media[,(names(social_media) %in% c("Period","Twitter Followers, End of period"))]

media <- read_excel(cached_metrics_file, sheet = "Media")
media_mentions <- nrow(media) - 1
outreach_events_table <- read_excel(cached_metrics_file, sheet = "Project Outreach & Events")
outreach_and_events <- nrow(outreach_events_table) - 1
outreach_fields <- outreach_events_table[1,]
colnames(outreach_events_table) <- as.list(outreach_events_table[1,])
outreach_events_table <- outreach_events_table[-c(1),]
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
day_number <- difftime(Sys.Date() , as.Date("1900-01-01"), units = c("days"))+2
recent_events <- outreach_events_table[(day_number-as.numeric(outreach_events_table$Date)) <= 90,]


ga_site_stats <- gadp[-c(3,4),] # Eliminating rows 3 and 4 (which 
# do not contain one month of data).
new_percentage <- ga_site_stats$'New Users'/100
users <- ga_site_stats$Users

users <- site_stats$users

ui <- shinyUI(fluidPage(
  tags$head(tags$style(
    HTML('
         #sidebar {
         background-color: #000;
         color: white;
         }
         
         body, label, input, button, select { 
         font-family: Optima,"Lucida Grande",Tahoma;
         }')
  )),
  # Application title
  titlePanel("",windowTitle = "Data Center Metrics"),
  
  fluidRow(
    HTML("<div style='background-color:white;float:left;color:black;
         width:100%;margin:0;padding:0;left:0;
         font-size:200%'>Data Center Metrics"),
    HTML("<span style='float:right'>"),
    img(src="images/small-WPRDC-logo-inverted.png"),
    HTML("</span>"),
    # Shiny R wants all images and JavaScript and stuff to be nested in a
    # folder called "www" which is in the same directory as app.R.
    # So images would be in dashboard_folder/www/images/, but when 
    # you call them, you drop the "www", e.g., img(src="images/z.png")
    HTML("</div>"),
    
    #            p(HTML("<a href=\"javascript:history.go(0)\">Reset this page</a>"))
    
    tabsetPanel(
      tabPanel("Web stats",
               fluidRow(column(8, align="right",plotOutput("plot1")),
                        column(width = 2, wellPanel(
                          radioButtons("plot_type", "Plot",
                                       c("Users", "Sessions", "Pageviews")
                          )))
                        ),
               dataTableOutput('analytics_table')), 
      # Show a plot of the generated distribution
      #         h2("Cumulative statistics"),
      #h2('Download stats'),
      tabPanel("Other web stats",h3("Publishers"),dataTableOutput('publishers'),
               h3("ETL Processes"),dataTableOutput('etl'),
               h3("Discussion Posts & Data Requests"),dataTableOutput('misc')
      ),
      tabPanel("Dataset stats",dataTableOutput('downloads_table'),
               downloadButton('downloadDatasetData', 'Download')),
      tabPanel("Package stats",dataTableOutput('by_package'),
               downloadButton('downloadPackageData', 'Download')),
      tabPanel("Classroom uses", 
               dataTableOutput('uses_table'),
               HTML(sprintf("Total classroom uses: %d (Pitt: %d, CMU: %d)", 
                            classroom_uses, pitt_uses, cmu_uses))),
      tabPanel("Outreach",
               h4("Media mentions: ", media_mentions),
               dataTableOutput('twitter_followers'),
               HTML("<hr>"),
               h4("Total outreach instances & events: ", outreach_and_events),
               HTML("<center style='font-size:140%'>Breakdown of outreach/events<br><span style='font-size:75%'>(Last 90 days in blue)</span></center>"),
               fluidRow(
                 splitLayout(cellWidths = c("70%", "30%"), plotOutput("event_types_plot"), plotOutput("recent_event_types_plot"))
               )
      )
    )
    )
  )
  )

# Define server logic required to render the output
server <- shinyServer(function(input, output) {
  
  #   output$userPlot <- renderPlot({
  #     barplot(users,names.arg=1:nrow(ga_site_stats),ylab="Users",xlab="Month",
  #             col=c("#0066cc"),cex.names=1.5,cex.lab=1.5)
  #   })
  output$plot1 <- renderPlot({
    if (input$plot_type == "Users") {
      barplot(users,names.arg=month_list,ylab="Users by Month",xlab="",
              col=c("#0066cc"),cex.names=1.5,cex.lab=1.5)
    } else if (input$plot_type == "Sessions") {
      barplot(site_stats$sessions,names.arg=month_list,ylab="Sessions by Month",xlab="",
              col=c("#0066cc"),cex.names=1.5,cex.lab=1.5)
    } else if (input$plot_type == "Pageviews") {
      barplot(site_stats$pageviews,names.arg=month_list,ylab="Pageviews by Month",xlab="",
              col=c("#0066cc"),cex.names=1.5,cex.lab=1.5)
    }
  })
  output$userPlot <- renderPlot({

  })
  output$analytics_table = renderDataTable({
    site_stats[,!(names(site_stats) %in% c("year","month"))] #df_analytics
  })
  output$uses_table = renderDataTable({
    df_uses
  })
  output$downloads_table = renderDataTable({
    df_downloads_and_pageviews[order(-df_downloads_and_pageviews$"1-month downloads"),] 
  })
  output$downloadDatasetData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = "dataset_downloads_and_pageviews.csv",
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- ","
      
      # Write to a file specified by the 'file' argument
      write.table(df_downloads_and_pageviews[order(-df_downloads_and_pageviews$"1-month downloads"),], 
                  file, sep = sep,
                  row.names = FALSE)
    }
  )
  output$by_package = renderDataTable({
    package_downloads_and_pageviews[order(-package_downloads_and_pageviews$"1-month downloads"),] 
  },options = list(lengthMenu = c(10, 25, 50), pageLength = 10))
  output$downloadPackageData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = "package_downloads_and_pageviews.csv",
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- ","
      
      # Write to a file specified by the 'file' argument
      write.table(package_downloads_and_pageviews[order(-package_downloads_and_pageviews$"1-month downloads"),], 
                  file, sep = sep,
                  row.names = FALSE)
    }
  )
  output$publishers = renderDataTable({
    publishers
  })
  output$etl = renderDataTable({
    etl_processes
  })
  output$misc = renderDataTable({
    misc_other_stats
  })
  output$twitter_followers = renderDataTable({
    twitter_followers
  })
  output$event_types_plot = renderPlot({
    dotchart(sort(table(outreach_events_table$Type),decreasing=FALSE),
             las=1,xlab="Count",col=c("#ff3300"))
  })
  output$recent_event_types_plot = renderPlot({
    dotchart(sort(table(recent_events$Type),decreasing=FALSE),
             las=1,xlab="Count",col=c("#0033ff"))
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

