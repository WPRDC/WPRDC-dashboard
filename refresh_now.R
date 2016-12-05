# This script forces prelude.R to reobtain all data files, even if 
# valid caches exist.

# Define the production variable by running authentication.R.
source("authentication.R")
force_refresh <- TRUE

source("prelude.R", local=TRUE)