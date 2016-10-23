# R is really bad at providing a way to identify a script's actual directory
# that works across operating systems and script-usage modalities. The script
# below, from 
# http://stackoverflow.com/questions/8835426/get-filename-and-path-of-sourced-file/18009281#18009281
# seems to accomplish this, and then it just sets the working directory 
# to that directory.

whereFrom=sys.calls()[[1]]
# This should be an expression that looks something like
# source("pathname/myfilename.R")
whereFrom=as.character(whereFrom[2]) # get the pathname/filename
whereFrom=paste(getwd(),whereFrom,sep="/") # prefix it with the current working directory
pathnameIndex=gregexpr(".*/",whereFrom) # we want the string up to the final '/'
pathnameLength=attr(pathnameIndex[[1]],"match.length")
whereFrom=substr(whereFrom,1,pathnameLength-1)
#print(whereFrom) # or "setwd(whereFrom)" to set the working directory
setwd(whereFrom)