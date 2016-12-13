library(RGoogleAnalytics)
library(lubridate)
library(googleAnalyticsR)
library(googleAuthR)
library(jsonlite)
library(httr)

authorize_analytics <- function(p_Id,client_id,client_secret,production) {
  if(!production) {
    authorize()
    ga_data <- get_ga(profileId = p_Id)
  } else {
    ga_token <- authorize(client.id = client_id, client.secret = client_secret,
                          cache="/srv/shiny-server/WPRDC/shiny_app_token.rds")
    ga_data <- get_ga(profileId = p_Id, token = ga_token)
  }
}

get_month_stats <- function(year,month,p_Id) {
  # Given a particular month, year, and Google Analytics profile ID,
  # pull the users, sessions, and page views for the specified month.
  start_date_string <- paste(toString(year),toString(month),"01",sep="-")
  end_date <- as.Date(start_date_string)
  day(end_date) <- days_in_month(end_date)
  
  month_stats <- get_ga(profileId = p_Id, start.date = start_date_string,
                        end.date = end_date, metrics = c("ga:users", "ga:sessions",
                                                         "ga:pageviews", 
                                                         "ga:pageviewsPerSession",
                                                         "ga:avgSessionDuration"), fetch.by = "month")
  return(month_stats)
}

get_analytics <- function(year,month,p_Id,client_id,client_secret,production) {
  authorize_analytics(p_Id,client_id,client_secret,production)  
  month_stats <- get_month_stats(year,month,p_Id)
  return(month_stats)
}

get_API_requests <- function(start_date="30daysAgo",end_date="yesterday",
                             p_Id,client_id=NULL,client_secret=NULL,production=FALSE) {
  
  authorize_analytics(p_Id,client_id,client_secret,production)
  # This requires creating a Google project in the Google Developers Console.
  # This is to allow Google sign-in for the project (in this case, the Shiny 
  # dashboard). 
  # https://developers.google.com/identity/sign-in/web/devconsole-project
    
  # See
  # https://cran.r-project.org/web/packages/RGA/vignettes/authorize.html
  # about creating and hiding these credentials.
  
  API_requests <- get_ga(profileId = p_Id, start.date = start_date,
                         end.date = end_date, 
                         metrics = c("ga:totalEvents", "ga:uniqueEvents"), 
                         dimensions = c("ga:eventCategory", "ga:eventLabel"),
                         sort = c("-ga:totalEvents"))
  # The Query Explorer
  #   https://ga-dev-tools.appspot.com/query-explorer/
  # is useful for constructing such queries.
  return(API_requests)
}
get_pageviews <- function(start_date="30daysAgo",end_date="yesterday",
                             p_Id,client_id=NULL,client_secret=NULL,production=FALSE) {
  
  authorize_analytics(p_Id,client_id,client_secret,production)
  pageviews <- get_ga(profileId = p_Id, start.date = start_date,
                         end.date = end_date, 
                         metrics = c("ga:pageviews"), 
                         dimensions = c("ga:pagePath"),
                         sort = c("-ga:pageviews"))
  # The Query Explorer
  #   https://ga-dev-tools.appspot.com/query-explorer/
  # is useful for constructing such queries.
  return(pageviews)
}
######################### RGoogleAnalytics helper functions
create_credentials <- function(client_id,client_secret,token_file,production) {
  if(!production) {
    oauth_token <- Auth(client.id = client_id, client.secret = client_secret)
    oauth_token$init_credentials()
    save(oauth_token, file = token_file)
    return(oauth_token)
  }
  return(NULL)
}

authorize_analytics_r_goo <- function(p_Id,client_id,client_secret,production) {
  # This method only works under two conditions. If the token file exists and is
  # valid, it will work. In non-production mode, the token can be recreated and
  # saved using manual web authentication. Otherwise, this approach seems to fail
  token_file <- "rgoogleanalytics_oauth_token"
  if(file.exists(token_file)) {
    load(token_file)
    ValidateToken(oauth_token)
    # This function checks whether the Access Token is expired. 
    # If yes, it generates a new Access Token and updates the token object
##  save(oauth_token, file = token_file)
#    if(!ValidateToken(oauth_token)) {
#      create_credentials(client_id,client_secret,token_file)
      # As documented on this page
      #   http://wearecoder.com/questions/k80x2/refresh-oauth-token-in-r-using-rgoogle-analytics
      # this is not quite enough, as running ValidateToken() may produce "Error: Refresh token not available"
      
      # So you have to do this once:
      # oauth_token$init_credentials()
      # This pulls up the web page that allows you to choose a Google login.
#    }
  } else {
    oauth_token <- create_credentials(client_id,client_secret,token_file,production)
  }
  return(oauth_token)
}


get_API_requests_r_goo <- function(start_date,end_date,
                             p_Id,client_id=NULL,client_secret=NULL,
                             production=FALSE) {
  #A start date must be specified of the form YYYY-MM-DD

  
  oauth_token <- authorize_analytics_r_goo(p_Id,client_id,client_secret,production)
  q.events <- Init(start.date = as.character(start_date),
                 end.date = as.character(end_date),
                 dimensions = c("ga:eventCategory", "ga:eventLabel"),
                 metrics = c("ga:totalEvents", "ga:uniqueEvents"),
                 sort = c("-ga:totalEvents"),
                 max.results = 99999,
                 table.id = p_Id)
  GA_events <- QueryBuilder(q.events)
  if(is.null(oauth_token)) {
    return(NULL)
  }
  API_requests <- GetReportData(GA_events, oauth_token, paginate_query = FALSE)

  return(API_requests)
}
###########################################################
source("authentication.R") # Look up the p_Id, client ID, and client
# secret to access the Google Analytics account. Also determine the
# value of the production variable, which sets whether this is a
# production or development environment.

authorize_json_request <- function(url,API_key) { # This function dies with an error
  # when there is no Internet connection.
  req <- httr::GET(url, httr::add_headers(Authorization = API_key))
  json <- httr::content(req, as = "text")
  json_data <- fromJSON(json)
  return(json_data)
}

######################### GoogleAnalyticsR helper functions

#"If for some reason you need authentication without access to 
#a browser (for example when using Shiny Server), then you can 
#authenticate locally and upload the .httr-oauth file to the 
#folder of your script."

authorize_analytics_gar <- function(production) {
  if(!production) {
    json_file <- "/Users/drw/WPRDC/Dashboards/Shiny/WPRDC-dashboard/google-auth.json"
  } else {
    json_file <- "google-auth.json"
  }
  if(file.exists(json_file)) {
    service_token <- gar_auth_service(json_file=json_file)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

get_API_requests_gar <- function(start_date,end_date,
                                   p_Id,production=FALSE) {
  #A start date must be specified of the form YYYY-MM-DD
  authorized <- authorize_analytics_gar(production)
  if(authorized) {
    API_requests <- google_analytics(id = p_Id, 
                             start = as.character(start_date), 
                             end = as.character(end_date), 
                             metrics = c("ga:totalEvents", "ga:uniqueEvents"),
                             dimensions = c("ga:eventCategory", "ga:eventLabel"),
                             sort = c("-ga:totalEvents"),
                             max_results = 99999
                             )
  return(API_requests)
  } else {
    return(NULL) # The backup option should really be better than returning NULL.
    # Maybe we should fall back to the .httr-whatehver authentication method, then
    # fall back to an old cached CSV before finally giving up.
  }
}

get_pageviews_gar <- function(start_date,end_date,
                          p_Id,production=FALSE) {
  
  #A start date must be specified of the form YYYY-MM-DD
  authorized <- authorize_analytics_gar(production)
  if(authorized) {
    pageviews <- google_analytics(id = p_Id, 
                                     start = as.character(start_date), 
                                     end = as.character(end_date), 
                                     metrics = c("ga:pageviews"), 
                                     dimensions = c("ga:pagePath"),
                                     sort = c("-ga:pageviews"),
                                     max_results = 99999
    )
    # This call to google_analytics seems to be the source of the 
    # following warning which I just noticed appearing on the console
    # when running the dashboard app from RStudio:
    #     Warning in FUN(X[[i]], ...) : 400 type error in response
    # It's not clear what this means, but introducing a delay of 1 second
    # before calling this does not eliminate this warning. This error
    # does not appear to be causing any problems, as the pageviews are
    # all subsequently downloaded.
    return(pageviews)
  } else {
    return(NULL) # The backup option should really be better than returning NULL.
    # Maybe we should fall back to the .httr-whatever authentication method, then
    # fall back to an old cached CSV before finally giving up.
  }
}

refresh_boolean <- function(datafile,refresh_period,cached_mode) {
  # If the file does not exist, definitely refresh it.
  # Otherwise, as long as we are not in cached mode, check whether
  # the file has outlived its freshness date and decide whether to 
  # refresh based on that.
  # "cached_mode" being true can be interpreted as "Don't attempt
  # to refresh the data unless you have no choice."
  refresh_data <- FALSE
  if(!file.exists(datafile)) {
    refresh_data <- TRUE
  } else if(!cached_mode) {
    if(Sys.time()-dminutes(refresh_period) > c(file.info(datafile)$mtime)) {
      refresh_data <- TRUE
    }
  }
  return(refresh_data)
}
# Consider replacing refresh_boolean with !refresh_boolean, renaming it 
# use_cache, and writing it like this:
#  use_cache <- TRUE
#  if(!file.exists(datafile) | (Sys.time()-dminutes(refresh_period) > c(file.info(datafile)$mtime))) {
#    use_cache <- FALSE
#  } # Can we really get away without using cached_mode here?

refresh_it <- function(data_getter,get_that_data,cache_file) {
  desired_data <- NULL
  if(get_that_data) {
    desired_data <- data_getter()
    if(!is.null(desired_data)) {
      write.csv(desired_data, cache_file, row.names=FALSE)
    } else if(file.exists(cache_file)) {
      desired_data <- read.csv(cache_file)
    }
  } else if(file.exists(cache_file)) {
    desired_data <- read.csv(cache_file)
  }
  return(desired_data)
}

get_site_stats <- function() {
  # Pull WPRDC stats from a dedicated data repository on wprdc.org.
  json_file <- "https://data.wprdc.org/api/action/datastore_search?resource_id=865441c9-498a-4a3f-8f52-3a865c1c421a&limit=9999"
  json_data <- authorize_json_request(json_file,CKAN_API_key)
    
  if(exists("json_data")) {
    site_stats <- json_data$result$records
    site_stats$`average session duration (minutes)` <- as.numeric(site_stats$`Average session duration (seconds)`)/60
    site_stats$`average session duration (minutes)` <- round(x=site_stats$`average session duration (minutes)`, digits = 2)
    site_stats$`pageviews per session` <- as.numeric(site_stats$`Pageviews per session`)
    site_stats$`pageviews per session` <- round(x=site_stats$`pageviews per session`, digits = 2)
    site_stats$users <- as.integer(site_stats$Users)
    site_stats$pageviews <- as.integer(site_stats$Pageviews)
    site_stats$sessions <- as.integer(site_stats$Sessions)
    site_stats <- site_stats[c("Year+month","users","sessions","pageviews","pageviews per session","average session duration (minutes)")]
#    site_stats$`year/month` <- paste(substring(site_stats$`Year+month`,1,4),substring(site_stats$`Year+month`,5,6),sep='/')
    site_stats$year <- as.integer(substring(site_stats$`Year+month`,1,4))
    site_stats$month <- as.integer(substring(site_stats$`Year+month`,5,6))
    
  } else {
    site_stats <- NULL
  }
  
  return(site_stats) 
}

get_monthly_downloads_stats <- function(resource_id,field_name) {
  # Pull WPRDC monthly downloads stats from a dedicated data repository on wprdc.org.
  json_file <- paste("https://data.wprdc.org/api/action/datastore_search?resource_id=",resource_id,"&limit=9999",sep="")
  json_data <- authorize_json_request(json_file,CKAN_API_key)

  if(exists("json_data")) {
    site_stats <- json_data$result$records
    site_stats$`Unique downloads` <- as.integer(site_stats$`Unique downloads`)
    site_stats$Downloads <- as.integer(site_stats$Downloads)
    site_stats[,as.character(field_name)] <- as.character(site_stats[,as.character(field_name)])
    site_stats <- site_stats[c("Year+month",field_name,"Downloads","Unique downloads")]
    site_stats$year <- as.integer(substring(site_stats$`Year+month`,1,4))
    site_stats$month <- as.integer(substring(site_stats$`Year+month`,5,6))
  } else {
    site_stats <- NULL
  }
  return(site_stats) 
}

get_monthly_resource_downloads <- function() {
  return(get_monthly_downloads_stats("e8889e36-e4b1-4343-bb51-fb687eb9a2ff","Resource ID"))
}

get_monthly_package_downloads <- function() {
  # Pull WPRDC monthly downloads stats by package from a dedicated data repository on wprdc.org.
  return(get_monthly_downloads_stats("d72725b1-f163-4378-9771-14ce9dad3002","Package ID"))
}
