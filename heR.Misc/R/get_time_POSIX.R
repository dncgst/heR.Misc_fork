get.time.POSIX <-
function (date.time, format="%d/%m/%y %H:%M:%S", units="mins", interval=1,
            index=1, start=NULL,   tolerance=0.00001, relax.increasing=FALSE,
            names=c("Time.POSIX",paste("Elapsed",units,sep="."),"Time.Factor"),
			tz="")
{

#  Function to return a data frame containing the POSIX
#   object of a date.time character or character factor vector,
#   including the elapsed time in 'units'.
#  Elapsed numeric times are calculated starting from the `index', which 
#  is assumed to be the first value in the vector, UNLESS a `start'
#  is specified in the same format as the date.time's, in which case we
#    use that as the reference point for elapsed numeric time units.

#  Also returns a time factor showing which records belong to a given
#   time interval (defined by units),
#   starting at the origin specified by `start' or `index',

#  UPDATE:  Added fatal error if all processed times are NA indicating a bad
#            format spec.  NK 9 May 2014
  
# UPDATE:   If times are not increasing and relax.increasing=FALSE, then we pring out elements
#             where the time is not increasing.  NK  9-March-2010

#  UPDATE:   Added interval argument, defaulting to 1 unit, so that we can
#        create time factors for multiples of mins or secs or hours, etc.
#                    12-June-2008 NK.

#   UPDATE:   Added tolerance argument, which defaults to the same as the
#      default for time.factor.2 .   NK 25-Jan-2006

#  UPDATE:  Check for all NA's in times and allow for a Date/Time object
#         version of `start' to be passed instead of a char string.  
#                NK 25-Jan-2006
  

time.POSIX <- as.POSIXct(strptime(as.character(date.time), format=format, tz=tz))

if (all(is.na(time.POSIX)))
  stop("All processed time values are NA.  Check argument `format' so it matches the character format of the input time data.  See `strptime' function.")

tidx <- diff(time.POSIX) <=0
if (any(tidx) & !relax.increasing) {
	cat("Times not increasing at:\n")
	print(data.frame(Index=which(tidx), Time.POSIX=time.POSIX[which(tidx)]))
}
	

if (any(is.na(time.POSIX)))
  stop("Please check your `format' string.")

if (index > NROW(time.POSIX) | index < 1) 
  stop("`index' value was out of range.")

# Start is assigned to `index' time value or converted from
#   character or translated to POSIXlt if a character string using the
#    same `format' or coerced to POSIXct otherwise
if (is.null(start))
  start <- time.POSIX[as.integer(index)]
else if (is.character(start))
  start <- strptime(as.character(start), format=format, tz=tz)
else
  start <- as.POSIXct(start, tz=tz)

if (is.na(start))
  stop("Check the format of `start' -- strings must be in the same format as `date.time'.")

elapsed <- as.numeric(difftime(time.POSIX, start, units=units))

time.factor <- time.factor.2(elapsed, interval=interval, origin=0,
                             integer.levels=TRUE, tolerance=tolerance,
							 relax.increasing=relax.increasing)

ret <- data.frame(Time.POSIX=time.POSIX, Elapsed=elapsed, Time.Factor=time.factor)
names(ret) <- names
ret

}