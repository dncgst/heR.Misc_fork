timeplot <- function(time, values, events=NULL, groups=NULL, tz="",
			format="%m/%d-%a", 
			plot.interval="6 hours", plot.format="%I%p",
			round=TRUE, relation="sliced",
			pch=16, cex=0.55, type="l", lwd=2,
			auto.key=TRUE, debug=FALSE, n=100, ...)
{

#  UPDATE 8/12/2011.  Fixed bug that dropped group/event levels (using "c" function)
#						now we use indexing of the input factors to retain levels
#  UPDATE 8/12/2011.  Fixed bug in data sorting/tfactor issue.
#  UPDATE 8/9/2011. Fixed bug in doing non-day plots (e.g., by hour)

# This function uses the lattice package to create a time series plot
#	nicely displayed as panels broken down by minute,hour,day, week,
#			month, year, etc.

#  Conditions on any unique time factor defined by a unit format,
#		defaulting to unique daily format: month/day-dayofweek 
#	But can define any way that strptime allows...
#   See strptime for more time-formatting syntax.

#   Internally always uses (if round==TRUE) trunc.units <- days to truncs 
#	beginning by specified time unit and 
#   	adds 1 unit to ending time and truncates that also....
#   could set to "secs", "mins", "hours", "days", but not needed because we
#	just want nice round beginning and ending time to calculate
#	axis labels..

#    If there are multiple time values within combination of time.factor
#		levels and factor levels defined by "groups" (
#		or everything in a time level if no groups defined),
#	 then the plot will be strange -- so we disallow this case.

#  make a panel function???? e.g., panel.hourplot, panel.dayplot, panel.weekplot,
#		panel.monthplot, panel.yearplot  
#	OR just panel.timeplot with appropriate arguments...

require(lattice)
require(heR.Activities)

# Possible time factor format string tokens in categories with highest
#		resolution included time component:
years <- c("%y", "%Y")
months <- c("%b", "%B", "%d", "%m")
weeks <- c("%U", "%W")
days <- c("%a","%A", "%j", "%w", "%x")
hours <- c("%H", "%I", "%p")
minutes <- c("%M")
seconds <- c("%c", "%S", "%X")
all <- c(years, months, weeks, days, hours, minutes, seconds)
tokens <- c()
for (i in all) if (grepl(i,format)) tokens <- c(tokens, i)
cat("Time-Date tokens found:\n")
print(tokens)	

#  Calculate upper offset in seconds....
if (any(seconds %in% tokens)) {
	trunc.units <- "secs"
	offset <- 1
} else if (any(minutes %in% tokens)) {
	trunc.units <- "mins"
	offset <- 60
} else if (any(hours %in% tokens)) {
	trunc.units <- "hours"
	offset <- 3600
} else if (any(days %in% tokens)) {
	trunc.units <- "days"
	offset <- 86400
} else if (any(weeks %in% tokens)) {
	trunc.units <- "days"
	offset <- 86400
} else if (any(months %in% tokens)) {
	trunc.units <- "days"
	offset <- 86400
} else if (any(years %in% tokens)) {
	trunc.units <- "days"
	offset <- 86400
}

time <- as.POSIXct(time)
time.factor <- format(time, format)

min.time <- min(as.POSIXlt(time, tz=tz))
min.time2 <- min.time
if (round) min.time2 <- trunc(min.time2, trunc.units)
min.year <- min.time2$year + 1900
min.mon <- min.time2$mon + 1
min.mday <- min.time2$mday
min.hour <- min.time2$hour
min.min <- min.time2$min
min.sec <- min.time2$sec

max.time <- max(as.POSIXlt(time, tz=tz))
max.time2 <- max.time
if (round) max.time2 <- trunc(max.time2 + offset, trunc.units)
max.year <- max.time2$year + 1900
max.mon <- max.time2$mon + 1
max.mday <- max.time2$mday
max.hour <- max.time2$hour
max.min <- max.time2$min
max.sec <- max.time2$sec

cat("\nRaw Data Beginning Time:\t",format(min(time),"%Y-%m-%d %H:%M:%S"))
cat("\nRaw Data Ending Time:\t",format(max(time),"%Y-%m-%d %H:%M:%S"),"\n")

cat("\nAdjusted (rounded) Data Beginning Time:\t",format(min.time2,"%Y-%m-%d %H:%M:%S"))
cat("\nAdjusted (rounded) Data Ending Time:\t",format(max.time2,"%Y-%m-%d %H:%M:%S"),"\n")

start <- ISOdatetime(min.year,min.mon,min.mday,min.hour,min.min,min.sec,tz=tz)
end <- ISOdatetime(max.year,max.mon,max.mday,max.hour,max.min,max.sec,tz=tz)

cat("\nPlot Start Time:\t",format(start,"%Y-%m-%d %H:%M:%S"))
cat("\nPlot End Time:\t",format(end,"%Y-%m-%d %H:%M:%S"),"\n")

time.seq <- seq.POSIXt(start,end,by=plot.interval)

cat("\nTime Domain:\n")
print(format(time.seq,"%Y-%m-%d %H:%M:%S"))

# Don't need to add points, just use the "sliced" relation value
#	to make X domain the same for all panels....
#  Well... doesn't always seem to work ..let's add the points anyway...
#  No let's not...seems like can screw things up sometimes..
#  not let's..because something weird is going on with scales in xyplot when we
#		have aggregated data...
#	ALSO NOTE:  Weird things happen unless the aggregated data start on an even hour
#		e.g., 12:00, 12:05, 12:10, etc. instead of 12:03,12:08, 12:13, etc.
if (min.time > as.POSIXct(start)) {
    cat("Adding low value to data:\n"); print(format(start,"%Y-%m-%d %H:%M:%S"))
	time <- c(time.seq[1], time)
	values <- c(NA, values)
	time.factor <- c(time.factor[1], time.factor)
	if (!is.null(groups)) groups <- groups[c(1,1:length(groups))]
	if (!is.null(events)) events <- events[c(1,1:length(events))]
}
if (max.time < as.POSIXct(end)) {
  cat("Adding hi value to data:\n"); print(format(end,"%Y-%m-%d %H:%M:%S"))
	time <- c(time, time.seq[length(time.seq)])
	values <- c(values, NA)
	time.factor <- c(time.factor, time.factor[length(time.factor)])
	if (!is.null(groups)) groups <- groups[c(1:length(groups),length(groups))]
	if (!is.null(events)) events <- events[c(1:length(events),length(groups))]
}

data <- data.frame(time=as.POSIXct(time), values=values, tfactor=time.factor)
if (!is.null(groups)) { 
	cat("Group levels:\n"); print(levels(groups))
	data <- cbind(data, groups=groups)
}
if (!is.null(events)) {
	cat("Event levels:\n"); print(levels(events))
	data <- cbind(data, events=events)
}

#  Sort the data frame by time
data <- data[order(time),]


if (debug) print(head(data, n=n))


#  Check for multiple time.factor values in a given panel and group breakdown
if (!is.null(groups)) {
	#print(table(data[c("time","groups","tfactor")]))
	if (any(as.vector(table(data[c("time","groups","tfactor")])) > 1))
		  stop("Found multiple time values in a given group and time factor breakdown.")
} else {
	#print(table(data[c("time","tfactor")]))
	if (any(as.vector(table(data[c("time","tfactor")])) > 1))
		  stop("Found multiple time values in a given time factor breakdown.")
}
	
if (is.null(events))
  print(xyplot(values ~ time | tfactor, groups=groups,
	pch=pch, cex=cex, type=type, lwd=lwd, auto.key=auto.key,
	data=data, 
	scales=list(alternating=1, 
			x=list(axs="i",relation=relation,format=plot.format,
					at=time.seq)),
	as.table=TRUE, ...))

else
  print(xytact(values ~ time | tfactor, groups=groups, tact.var="events",
	pch=pch, cex=cex, type=type, lwd=lwd, auto.key=auto.key,
	data=data,
	scales=list(alternating=1, 
			x=list(axs="i",relation=relation,format=plot.format,
					at=time.seq)),
	as.table=TRUE, ...))

invisible(data)
	
}

	