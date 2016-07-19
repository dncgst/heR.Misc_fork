plot.monitors <- 
function (x, data=alldata,
		groups=NULL, codes=NULL, tact.var=NULL,
		show=TRUE, name="timeSeries", make.pdf=FALSE, 
		make.svg=FALSE, make.bitmap=FALSE, type.bitmap="png16m", units="in",
		make.postscript=FALSE, make.metafile=FALSE, onefile=TRUE, width=11, height=8.5,
		tz="", interval="30 min",
		format="%H:%M", tod=TRUE,
		ylab="Concentration", xlab="Time of Day", 
		main=NULL,
		type="l", pch=16, lty="solid", lwd=2, layout=NULL, cex=1,
		col=NULL, codes.colors=NULL, col.border="gray70", 
		origin.area=0, fill.area="gray50", border.area="black",alpha.area=0.6,
		as.table=TRUE, 
		scales.x=list(relation="free", alternating=TRUE, rot=0, log=FALSE, axs="i"),
		scales.y=list(relation="same", alternating=TRUE, rot=0, log=FALSE),
		scales=NULL,
		outerStrips=FALSE, clean=FALSE,
		strips.bg=trellis.par.get("strip.background")$col,
		strips.left.bg=trellis.par.get("strip.background")$col,
		strips=strip.custom(bg=strips.bg), 
		strips.left=strip.custom(horizontal=FALSE, bg=strips.left.bg),
		strip.lines=1, strip.left.lines=1, remove.nonincreasing=FALSE, 
		debug=FALSE, abline=NULL, ...)

{
#  Update:  separate scales for x and y axis that are used to build up a scales argument 
#				if scales argument is NULL.  11June2013

#  Update:  Drawing the show=TRUE to current device last seems to have fixed weird drawing issues
#		in RStudio.     We assign plot to p <- now and update it or print it as needed.
#		Also:  We now have a layers argument where we can add arbitrary latticeExtra layers to the plot
#		using a function argument.   NK  11June2013

#  Update:  Fix using elapsed time as x-axis (i.e., not encoded POSIX times).  Now, if tod=FALSE
#	we don't do any custom formating of the x-axis.  User takes all responsibility.  5June2013.

#  TODO IDEA:   Have option to sort records by time for a given panel + group in the plot. 

#  TODO:    Assign plots to a variable p, and then do print(p) and return plot object
#   or use print(p) to plot to a given additional graphics device... NK 5June2013
#	Above is done.  show=TRUE always plots to current graphics device...  Done.

# TO DO:   Deal with time zones, and time zone changes in mid-series... try using Date package.

# UPDATE:  Now have `scales' to match xyplot args, but with defaults suited to plotting
#		time series with panels split by a time factor over continuous time values. NK19Dec2012

# UPDATE:  No defaults for x.  If missing we throw an error. NK19Dec2012

# UPDATE:  added debug arg.  NK 17Dec2012

# UPDATE:   make tact.var=NULL, and modified xytact to allow for tact.var=NULL in which
#				case time-activities are not drawn.    NK    1/26/2012

# TODO:   Allow for elapsed time (e.g., minutes) plots in addition to datetime formated
#			strings on the x-axis....OK added tod=TRUE argument, if set to FALSE then
#			we plot elapsed time (using interval) rather than time of day...NK 1/19/2011

# Note 12/14/2011 -- This function has now been expanded and moved to the heR.Misc packages and
#			named "plot.monitors".  It (or it's descendants) will be used to
#			plot the results of the process.monitors function....  Neil Klepeis (NK)

#  To do for xytact (or right here):  add option (clean) to remove all data that has NA for the specified events..

#  To do for xytact:  option to not need a group variable.... (fix bug actually)

#  Make Two-Column plots to fit all on one page but still get some height to the peaks..

#x <- as.formula(x, env=environment())   # set frame as function environment
# if x is missing, we throw an error
#if (missing(x)) x <- formula(Value ~ Time.POSIX | SensorID)

if (debug) print(deparse(substitute(groups)))
group.var <- all.vars(formula(c("~",deparse(substitute(groups)))))
if (debug) print(group.var)
vars <- c(all.vars(x), tact.var, group.var)
yvar <- vars[1]
xvar <- vars[2]
cat("Variables being used: ", paste(vars,collapse="|"),"\n")

#  Added by NK 17Dec2012,  check if groups variable is for real...if quotes used group will
#		be NULL and cause confusion because above check will be FALSE
if (!is.null(substitute(groups)))
  if (!all(deparse(substitute(groups)) %in% names(data)))
    stop("Grouping variable(s) is(are) not in the data. Check group expression (do not use quotes around `groups' expression).")

if (!all(vars %in% names(data)))
	stop("Some specified variable names not in data.")

if (clean) {
	cat("Cleaning data of rows with NA values...\n")
	data <- na.omit(data[vars])
	if (NROW(data) == 0) stop("No non-NA rows left after `clean'-ing.  Try setting `clean' to FALSE.")
}

cat("Independent (time) variable [",xvar,"] :\n")
if (tod) print(head(as.POSIXlt(data[[xvar]],tz=tz)))
else print(head(data[xvar]))

groups <- eval(substitute(groups), data, environment(x))
cat("Group factor levels:\n")
print(unique(groups))

#  Function to create event-annotated plots of the lab data

require(heR.Misc)	
require(heR.Activities)
require(lattice)
if (outerStrips || any(type == "A")) require(latticeExtra)

# Should we get the time zone from the data itself??

# can specify scales for x and y individually or as a big scales argument...
if (is.null(scales)) {
	scales <- list()
	scales$x <- scales.x
	scales$y <- scales.y
}	

if (tod) {

	data[[xvar]] <- as.POSIXct(data[[xvar]], tz=tz)

	min.time <- min(as.POSIXlt(data[[xvar]], tz=tz))
	max.time <- max(as.POSIXlt(data[[xvar]], tz=tz))
	min.year <- min.time$year + 1900
	min.mon <- min.time$mon + 1
	min.mday <- min.time$mday
	max.year <- max.time$year + 1900
	max.mon <- max.time$mon + 1
	max.mday <- max.time$mday

	time.seq <- seq.POSIXt(ISOdatetime(min.year,min.mon,min.mday,0,0,0,tz=tz), 
                        ISOdatetime(max.year,max.mon,max.mday,23,59,59,tz=tz),
                        by=interval)

#cat("Plot x-axis time sequence:\n")
#print(time.seq)						
	scales$x$at <- time.seq
	scales$x$format <- format					
	#scales <- list(x=list(at=time.seq, format=format,
	#	relation=relation.x, rot=rot.x, alternating=alternating.x),
	#	y=list(relation=relation.y, rot=rot.y, log=log, 
	#	alternating=alternating.y))
} 
#else {						
#						
#	scales <- list(x=list(relation=relation.x, rot=rot.x, alternating=alternating.x),
#		y=list(relation=relation.y, rot=rot.y, log=log, 
#		alternating=alternating.y))						
#}						

if (debug) {
	cat("Scales spec:\n")
	print(scales)
}


#make.plots <- function() {
	if (outerStrips) {
		p <- useOuterStrips(xytact(x,
			groups=groups, data=data,
			tact.var=tact.var, codes=codes,
			as.table=as.table,
			type=type, pch=pch, lty=lty, lwd=lwd, cex=cex,
			col=col, codes.colors=codes.colors, col.border=col.border, 
			origin.area=origin.area, 
			fill.area=fill.area, alpha.area=alpha.area, border.area=border.area,
			ylab=ylab, xlab=xlab, main=main, 
			remove.nonincreasing=remove.nonincreasing,
			debug=debug, abline=abline, ...),
		strip = strips,
		strip.left = strips.left,
		strip.lines = strip.lines,
		strip.left.lines = strip.left.lines
			)

		p <- update(p, layout=layout)	
		p <- update(p, scales=scales)			
		
	} else {
		p <- xytact(x,
			groups=groups, data=data,
			tact.var=tact.var, codes=codes,
			layout=layout, as.table=as.table,
			type=type, pch=pch, lty=lty, lwd=lwd, cex=cex,
			col=col, codes.colors=codes.colors, col.border=col.border, 
			origin.area=origin.area, 
			fill.area=fill.area, alpha.area=alpha.area, border.area=border.area,
			scales=scales,
			ylab=ylab, xlab=xlab, main=main, 
			remove.nonincreasing=remove.nonincreasing,
			debug=debug, abline=abline, ...)
	}
	
#}

#if (!is.null(layers))
	#if (!is.list(layers))
#		stop("`layers' must be a list of layers for adding to the plot")
#	else
#		for (i in length(layers)) p <- p + layers[[i]] 

		
if (make.pdf) {
 pdf(file=paste(name,".pdf",sep=""), width=width, height=height, onefile=onefile)
 #make.plots()
 print(p)
 dev.off()
}

if (make.postscript) {
 postscript(file=paste(name,".ps",sep=""), width=height, height=height, onefile=onefile)
 #make.plots()
 print(p)
 dev.off()
}

if (make.svg) {
 #require(RSvgDevice)
 require(grDevices)
 #devSVG(file=paste(name,".svg",sep=""), width=width, height=height, onefile=onefile)
 svg(file=paste(name,".svg",sep=""), width=width, height=height, onefile=onefile) 
 #make.plots()
 print(p)
 dev.off()
}

if (make.bitmap) {
 bitmap(file=paste(name,".",type.bitmap,sep=""), width=width, height=height,
		units=units, type=type.bitmap, onefile=onefile)
 #make.plots()
 print(p)
 dev.off()
}

if (make.metafile) {
 win.metafile(filename = paste(name,".emf",sep=""), width = width, height = height,
				pointsize = 12, restoreConsole = TRUE)
 #make.plots()
 print(p)
 dev.off()
}

# show on current device?
if (show) print(p)
#if (show) make.plots()

invisible(p)

}
