time.factor.POSIXt <-
function (times, interval = 1, unit="mins", origin = min(times), integer.levels = FALSE, 
    tolerance = 1e-05, relax.increasing = FALSE, tz="", debug=FALSE) 
{

# Function:   same as time.factor.2, except 'times' is in POSIXt format
#        instead of numeric format.  We use cut.POSIXt, after we get
#        the proper breaks to use.  We can't use cut.POSIXt directly with 
#        1 min or 5 min break designation, since we want the origin to be
#        anywhere and we have to create the proper breaks first anyway.
#                           We also  NK 3-June-2009
# ----------------------------

#  UPDATE:  Fixed reversal of "lower" if only lower is present.   Also made by=interval pos/neg
#			for seq.POSIXt in "seconds" to make things simpler.  
#					8/23/2011 NK.

#  UPDATE:   Some weird time zone problems arose when origin was not at the beginning
#         of the data, which means we used c() to combine the break data (both lower and 
#        upper are defined).    The man page says that time zone is changed to current
#        machine time zone in this case, so we must convert back... NK 5-Dec-2009

#  Update 11/17/09  -- fix bug where interval.secs was undefined if units=="secs" | "sec"

#    Gives factors as the beginning POSIX time of each interval.  Or returns
#       integer levels like time.factor does...

    times <- as.POSIXlt(times, tz=tz)
	#cat("\nFirst instance of times:\n")
	#print(times[1])

    if (!relax.increasing & any(diff(times) <= 0)) 
        stop("`times' is typically a strictly increasing set of times; this check can be disabled with a `relax.increasing=TRUE' argument.")
    if (any(is.na(times))) 
        stop("`times' must not contain any missing values.")

    interval <- as.numeric(interval)[1]
    if (interval <= 0) 
        stop("`interval' must be a positive number.")
    if (length(unit) != 1 || !unit %in% c("min","mins","sec","secs","hour","hours"))
      stop("`unit' must be one of `min(s)', `sec(s)', or `hour(s)'.")

    #  Convert interval into proper number of seconds so we
    #     can add it to POSIXt times.
	
    if (unit == "min" || unit == "mins")
		interval.secs <- interval*60
    else if (unit == "hour" || unit == "hours")
		interval.secs <- interval*3600
    else
		interval.secs <- interval


    #  Old by=interval was a string with interval+units...
    #interval.pos <- paste(interval," ",unit,sep="")
    #interval.neg <- paste("-",interval," ",unit,sep="")
	
	# get by=interval in secs... (simpler).  fixed  8/23/2011. NK
    interval.pos <- interval.secs
    interval.neg <- -interval.secs


    origin <- as.POSIXct(origin, tz=tz)[1]
	#cat("Origin:\n")
	#print(origin)
	
    n <- length(times)
    if (tolerance >= interval.secs) 
        stop("The `tolerance' for placing values in the next highest bin must be less than the interval width.")

    #f <- vector(length = n, mode = "integer")  what is this for?
    #lower <- as.POSIXct(c())
    #lidx <- c()
    if (origin > min(as.POSIXct(times))) {
        lower <- seq(origin, min(as.POSIXct(times)) - interval.secs, by = interval.neg)
        lidx <- -1:-(length(lower) - 1)
	  #cat("Lower:\n")
	  #print(lower)
    }
    
    #upper <- c()
    #uidx <- c()
    if (origin <= max(as.POSIXct(times))) {
        upper <- seq(origin, max(as.POSIXct(times)) + 2 * interval.secs, by = interval.pos)
        uidx <- 1:(length(upper) - 1)
	  #cat("Upper:\n")
	  #print(upper)
	}


    if (exists("lower") && exists("upper")) {
         lower <- lower[-1]
         breaks <- c(rev(lower), upper)
         int.levels <- c(rev(lidx), uidx)
    } else if (exists("lower")) {
         breaks <- rev(lower)		# fixed. added rev.  8/23/11
         int.levels <- rev(lidx)	# fixed. added rev. 8/23/11
    } else {
         breaks <- upper
         int.levels <- uidx
    }
	
	#   Some strange thing causes tz to go to current machine time zone
	#     instead of what is passed as tz, when the rev() command is used
	#   Here we try to put back the right tz.   as.POSIXct doesn't work, use
	#        as.POSIXlt instead.
	breaks <- as.POSIXct(as.POSIXlt(breaks, tz=tz))
    
    if (debug) {
		cat("\n\nBreaks:\n")
		print(breaks)
	}
	
    if (integer.levels) labels <- int.levels
    else labels <- breaks[-length(breaks)]
    times <- times + tolerance
    cut(times, breaks = breaks, labels = labels)
}

