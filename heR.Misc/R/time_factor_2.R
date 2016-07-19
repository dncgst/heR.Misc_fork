time.factor.2 <-
function (times, interval=1, origin=min(times), integer.levels=FALSE,
           tolerance=0.00001, relax.increasing=FALSE, na.incomplete=FALSE) 
{

#  THIS IS "time.fact.2", which uses the `cut' function instead of 
#     trying to do it ourselves.  Here, we really just set up the
#     proper breaks and labels and pass it along to `cut'.  30Oct04
# ---------------------------------------------------------------
  
# UPDATE:  21-Apr-2010.  Option to assign NA to incomplete intervals in
#				beginning or end ..  Not done yet!  maybe really don't need this
#				since we may have incomplete data in other intervals also.....nk
# UPDATE:  Make sure interval is greater than 0

# OK, everything (finally) seems to be working OK.  But we should really
#     just be setting up the cutpoints here and using `cut' for the
#     final factor generation.  See time.factor.2.  NK 30-Oct-04

# UPDATE:  Fixed the use of the tolerance, it was operating on the wrong
#           bin (towards negative from lower bin instead of towards positive
#                 from higher bin).  Also I set a limit on tolerance; it must
#                 be less than the interval.  [Did I ever use the
#                 tolerance feature for anything? Did it work????]
#                       --NK 30-Oct04

# UPDATE:  Add ability to relax the requirement of increasing 
#           values of times, i.e., not check whether the
#           times are increasing or not...

#  OK, like the cut.POSIXt routine, lets use the `cut' function.
#    But we are a bit more flexible than cut.POSIXt because
#    we can specify any interval and the original and a tolerance.
#     Actually, cut.POSIXt _CAN_ cut by any number of secs, mins, etc.
#     but it uses the first value as the origin and can't go below
#     1 sec intervals.  NOT DONE YET.  cut.POSIXt works well enough....


#  IDEA:  It would seem better to use the cut or the cut.POSIXt
#          functions, instead of reinventing the wheel, and faster
#          too?   At least vectorize the process, for god's sake....

#  UPDATE:  OK, now we can specify any origin we want, even if it
#           is outside of the range of times.   In this case, 
#           the integer.levels may not start or end at 1

#  UPDATE:  Added a tolerance for limits of bins that
#          farthest from origin so that times that are
#          within the tolerance will be placest in the next
#          farthest bin.  This is to help sometimes when 
#          times are fractions that don't quite make it to the
#          next highest bin but should really be there anyway.

#  Function to calculate an appropriate time series 
#    grouping factor with fixed-length groups.  The factor
#    can be used to produce a new grouped time
#    series consisting of consecutive averages, or some other statistic,
#    over a fixed time interval.  The beginning of the factor cycle (origin) is
#    at the first value of times, by default.
#    This new time series can be combined with
#    others having a consistent time format.
#    The returned factor consists of consecutive fixed-length time groups
#    designated by consecutive integers, i.e., -2,-1,1,2,3,...,
#    if integer.levels==TRUE,
#    otherwise the factor levels are equal to the left most limit of
#    the interval.

#   This function was originally written to convert a time series
#     of 2s - 15s particle and CO measurements to a time series
#     of 1 minute averages.

#   times = a numeric vector of increasing times
#   interval = the time interval within which elements of `times' will be
#              grouped in the returned factor
#   origin = the time at which grouping will begin

#  Note that times MUST be increasing,because we set up a sequence of 
#     increasing values at even intervals and compare each time to this
#     sequence, assigning them to proper "bins". 

times <- as.numeric(times)

if (any(is.na(times)))
  stop("`times' must not contain any missing values.   Index of NA's:", which(is.na(times)))

if (length(times) < 1)
	stop("`times' is empty.", times)

if (!relax.increasing & any(diff(times) <=0))
  stop("`times' is typically a strictly increasing set of times; this check can be disabled with a `relax.increasing=TRUE' argument.")

interval <- as.numeric(interval)[1]
if (interval<=0) stop("`interval' must be a positive number.")

origin <- as.numeric(origin)[1]
n <- length(times)

if (tolerance >= interval)
  stop("The `tolerance' for placing values in the next highest bin must be less than the interval width.")

#if (origin < times[1] | origin > times[n])
#  stop("`origin' must fall within the range of `times'.")

f <- vector(length=n, mode="integer")  

lower <- c()
lidx <- c()
if (origin > min(times)) {
  lower <- seq(origin, min(times)-interval, by=-interval)
  lidx <- -1:-(length(lower)-1)
}  
upper <- c()
uidx <- c()
if (origin <= max(times)) {
  upper <- seq(origin, max(times)+2*interval, by=interval)
  uidx <- 1:(length(upper)-1)
}  
if (length(lower) & length(upper))
  lower <- lower[-1]

breaks <- c(rev(lower), upper)

if (integer.levels)
  labels <- c(rev(lidx), uidx)
else
  labels <- breaks[-length(breaks)]

times <- times + tolerance

#print(breaks)
#print(labels)

cut(times, breaks=breaks, labels=labels)

}