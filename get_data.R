source("authentication.R") # Look up the p_Id, client ID, and client
# secret to access the Google Analytics account. Also determine the
# value of the production variable, which sets whether this is a
# production or development environment.

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

  
