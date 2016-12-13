This repository contains the code for a Shiny-based dashboard, customized for tracking performance metrics for the Western Pennsylvania Regional Data Center (WPRDC). The live dashboard is at http://tools.wprdc.org/analytics/wprdc-dashboard/

Data is pulled from three sources: 1) a Google Analytics account used to track site usage and downloads (recorded as Events under Google Analytics), 2) a Google Sheet with a bunch of subsheets used to document many other metrics associated with WPRDC activity, and 3) the data.json file from the CKAN instance used to host the WPRDC site and its open data repositories.

Shiny is the R-based web framework which provides the front end for this dashboard (in app.R). Other R scripts are used for pulling all of the above data and metrics together and munging it into a human-readable form. (For instance, Google Analytics tracks pageviews by the URL of the page, and the data.json file is used to look up URLs and CKAN resource IDs for individual WPRDC-hosted data files and obtain the names of the resources (individual data files, often CSV files), datasets (groups of data files, like "Allegheny County Crash Data"), and publishing organizations.)

The get_data.R script can pull data from all of the above sources using various APIs. Generating a CSV file that has month-by-month download histories for each of the hundreds of WPRDC resources is performed by a separate Python script, and a function within get_data.R then pulls this data from a special CKAN repository. Basic site stats are also obtained in the same fashion.

All data critical to the running of the dashboard is cached in CSV files (or as an Excel file in the case of the Google Sheets data). The dashboard checks the freshness of the cache files against allowed lifetimes for each file and only pulls new data when necessary.

Used alone, this approach would occasionally result in users encountering long load times during data refreshes, so all of the data-loading and -transforming operations were pulled out of app.R into a separate file (prelude.R). The prelude.R script is periodically run from a separate refresh_now.R script that overrides all the cache-checking and forces all data to be redownloaded in the background. Long load times for users are eliminated by running refresh_now.R more frequently than any cache lifetime, via a cron job that looks like this:

```
1,16,31,46 * * * * cd /srv/shiny-server/wprdc-dashboard && /usr/bin/R CMD BATCH /srv/shiny-server/wprdc-dashboard/refresh_now.R
```

To make this dashboard work, it's necessary to set up all the Google authentication (by setting up a service account and locating the JSON file containing authentication credentials in the same directory as the R scripts) and provide an R script (called "authentication.R") that sets 1) a Boolean variable called "production", the Google Sheets key (a string called "sheet_key"), the CKAN API key (called "CKAN_API_key") [though this is only necessary if you are pulling data from repositories on CKAN that are still private], and the profile ID for the Google Analytics data (under a string called "p_Id").

An excellent guide to issues that may be encountered when installing R and Shiny server and deploying Shiny apps to Linux servers (including the tricky navigation of permissions that may be necessary) can be found here: http://deanattali.com/2015/05/09/setup-rstudio-shiny-server-digital-ocean/
