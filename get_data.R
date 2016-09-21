library(RGoogleAnalytics)
library(lubridate)
library(googleAnalyticsR)
library(googleAuthR)

source("authentication.R") # Look up the p_Id, client ID, and client
# secret to access the Google Analytics account. Also determine the
# value of the production variable, which sets whether this is a
# production or development environment.

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

######################### GoogleAnalyticsR helper functions

#"If for some reason you need authentication without access to 
#a browser (for example when using Shiny Server), then you can 
#authenticate locally and upload the .httr-oauth file to the 
#folder of your script."


# Something simple that worked:
#service_token <- gar_auth_service(json_file="WPRDC/Dashboards/Shiny/wprdc_io/Internal dashboard-e6fc0911ead5.json")
#account_list <- google_analytics_account_list() #shows list of accessible WPRDC Google Analytics accounts


# Some documentation
#gadata <- google_analytics(id = ga_id, 
#             start="2016-08-01", end="2016-08-31", 
#             metrics = c("sessions", "bounceRate"), 
#             dimensions = c("date"))
authorize_analytics_gar <- function(production) {
  if(!production) {
    json_file <- "/Users/drw/WPRDC/Dashboards/Shiny/wprdc_io/Internal dashboard-e6fc0911ead5.json"
  } else {
    json_file <- "Internal dashboard-e6fc0911ead5.json"
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
    return(pageviews)
  } else {
    return(NULL) # The backup option should really be better than returning NULL.
    # Maybe we should fall back to the .httr-whatehver authentication method, then
    # fall back to an old cached CSV before finally giving up.
  }
}



authorize_analytics_gar_did_it_ever_work <- function(p_Id,client_id,client_secret,production) {
  # This method only works under two conditions. If the token file exists and is
  # valid, it will work. In non-production mode, the token can be recreated and
  # saved using manual web authentication. Otherwise, this approach seems to fail
  token_file <- "original-Google-service-account-credentials.json"
  if(file.exists(token_file)) {
    
    #https://cran.r-project.org/web/packages/googleAnalyticsR/vignettes/googleAnalyticsR.html
    #Make sure to add the service email to the users of the Google project, and then 
    #download the JSON file and authenticate via:
    #googleAuthR::gar_auth_service("gwt-download-XXXX.json")
    
    
    options(googleAuthR.client_id = client_id)
    
    options(googleAuthR.client_secret = client_secret)
    options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics"))
    options(googleAuthR.webapp.client_id = "798439325891-g9u0fjsfij4plnq700r0hq8r671mfhrk.apps.googleusercontent.com")
    options(googleAuthR.webapp.client_secret = "PjUX7m11E-0-yXZkG3l1RBdL")
    
    options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics"))
    googleAuthR::gar_auth_service(json_file = token_file)
#    googleAuthR::gar_auth_service(json_file = token_file,
#                                  scope = "https://www.googleapis.com/auth/analytics")
    # In some cases, it is necessary to transform the character sequence "\n" in
    # that JSON file into actual carriage returns, as described here:
    # http://stackoverflow.com/questions/31351344/openssl-crypto-error-in-signedjwtassertioncredentials-attempt
    # However, this destroys the file's authenticity from the standpoint of gar_auth_service().
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

