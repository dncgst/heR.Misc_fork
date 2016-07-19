frame2series <- function (data, start.var, end.var, date.var="Date", 
			date.format="%m/%d/%Y", time.format="%H:%M:%S", by=NULL) 
{

#  Convert data events formated as a series of time frames with beginning
#		and ending time variables to a continuous time series of events
#		with a single time variable.

#  First we duplicate each row, one for beginning time and one for 
#		ending time -- adding NA's to event variables for 2nd time and
#		creating a single time variable from beg/end times.

#		We order the final records by the new time variable and 
#			any specified index variables.

#		Don't do this:  We then delete any rows with duplicate times.

#   TODO:  Add checks so that there are no overlapping time frames and to
#		see if the time frames are adjacent, e.g., are contiguous in time
#		with no time in between.    For now the user will have to delete
#		any rows with duplicate times themselves if the time segments
#		are contiguous.

if (missing(start.var) || missing(end.var))
	stop("`start.var' and `end.var' must contain names of variables contining the starting and ending times for each time segment")

if (!start.var %in% names(data) || !end.var %in% names(data))
	stop("`start.var' and `end.var' must be variables names in `data' containing the event starting and ending times, respectively.")

start.idx <- which(names(data) == start.var)[1]
end.idx <- which(names(data) == end.var)[1]

#  check validity of time format by converting to POSIX time

time1 <- as.POSIXct(strptime(paste(data[[date.var]],data[[start.var]]), 
			format=paste(date.format,time.format)))
time2 <- as.POSIXct(strptime(paste(data[[date.var]],data[[end.var]]),
			format=paste(date.format, time.format)))

if (any(is.na(time1)) || any(is.na(time2))) 
	stop("Some times are missing or converted incorrectly, check format spec.")

data[[start.var]] <- format(time1, format=time.format)
data[[end.var]] <- format(time2, format=time.format)

# Sort by standard start time and index variables (if any)
data <- data[do.call("order", data[c(by, start.var)]),]

data2 <- rbind(data,data)	#  duplicate the rows
#data2[] <- NA  #  set all values initially to NA

#  Add 2nd row for each ending time with NA's for all event vars
for (i in 1:NROW(data)) {
	data2[i+i-1,] <- data[i,]
	data2[i+i,] <- NA
	#  only non-NA variables in 2nd row are date,time and indices..
	if (!is.null(by)) data2[i+i,by] <- data[i,by]
	data2[i+i,date.var] <- data[i, date.var]
	data2[i+i,start.var] <- data[i,end.var]
}


data2

}