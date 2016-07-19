match.time.segments <- 
function(times, segment.limits, labels, format, add.integers=FALSE,
         sep="-", collapse=TRUE, debug=FALSE)
{

#  Function to take a vector of increasing times and to create a factor, equal
#     in length this vector, which identifies a set of time segments, e.g.,
#     for different events (action, locations, etc.).    You must
#     specify a set of (increasing) limits for the time segements, and,
#     optionally, labels for each time segment. 
#
#     We sort the segment limits, so they don't technically have to be in order
#     as specified as arguments.
#
#    Tries to convert times to POSIXct using strptime  if `format' is given.
#
#    Includes the lowest limit in assigning the factor level for each interval.
#
#  UPDATE:  Key off of `labels', if missing then we use integers....removed return.index=TRUE.
#						argument.   NK 25-April-2010
#   UPDATE:  Fixed bug when segment.limits was empty.    NK 24-March-2010
#    UPDATE:   Now we allow NA's in times, and return NA's
#               as appropriate, so that the length of the result is equal the
#               length of times...Also return a string of NA's if 
#				segment.limits is empty.   Any NA's in segment.limits are
#               stripped away.   NK 3-Feb-2010
#
#    UPDATE:   Change name to "match.time.segments" to show it's similarity to the
#              general match function.    3-Feb-2010
#    UPDATE:  Have new logical argument, return.index, which, if true, makes the
#              function return numeric indices for 'times' corresponding to
#              elements of 'segment.limits', i.e., it is like the `match' function that
#             returns the index of the first argument in the second argument.
#            This is the new default behavior, and labels and add.integers are ignored.
#            If return.index is FALSE (and labels are given), then the sep,# 
#              add.integers arguments are used to return factor values for each match.
#             With no labels, numeric values are also return, a factor is only
#            returned if labels are given and return.index is FALSE.   NK   3-Feb-2010
#
#    UPDATE: Now a fatal error is returned if segment.limits are not
#            strictly increasing.   The times do not have to be.
#
#    UPDATE:  Add collapsing by default of the segments if the same factor
#               value is given in sequential segments. 
#
#    UPDATE:  Now we, by default, add sequential integers to the time segment
#            labels so that earlier segments can be distinguished from
#            later ones.   If add.integers=FALSE, then it is possible for
#            later segments to have the same label as earlier ones and may
#            confuse plotting and analysis.   11-June-08 NK
# --------------------------------------------------------------            

if (debug) {
  cat("match.time.segments: Passed times: \n")
  print(head(times))
  cat("match.time.segments: Passed segment.limits: \n")
  print(head(segment.limits))
  cat("match.time.segments: length segment.limits: \n")  
  print(length(segment.limits))
  if (length(segment.limits) < 1) {
    cat("match.time.segments: returned NA: \n")    
    print(head(rep(NA, length.out=length(times))))
  }
  
}

#  If segment.limits is empty then just return string of NA's
if (is.null(segment.limits) || length(segment.limits) < 1)
		return(rep(NA, length.out=length(times)))

if (debug) {
  cat("match.time.segments: valid segment limits passed \n")
}


if (!missing(format)) {
  times <- as.POSIXct(strptime(times, format=format))
  segment.limits <- as.POSIXct(strptime(segment.limits, format=format))
} else {
  times <- as.POSIXct(times)
  segment.limits <- as.POSIXct(segment.limits)
}

if (any(is.na(segment.limits)))
   warning("NA's in segment limits, check time format.")
if (any(is.na(times)))
   warning("NA's in times.")

#if (any(is.na(segment.limits)))
#	stop("Error converting `segment.limits' to POSIX times, check `format' string.")

if (missing(labels)) {
  
   labels <- 1:(length(segment.limits) - 1)
   result <- as.numeric(rep(NA,length(times)))
  
} else {

   if (length(labels) != length(segment.limits) - 1)
     stop("`labels' must have length 1 element smaller than length of `segment.limits'.")

   # collapse limits and labels if specified
   if (collapse) {
     #cat("Collapse...\n")
     #print(labels)
     #print(segment.limits)
     #print(collapse(labels))
     newi <- collapse(labels)
     labels <- labels[newi]
     segment.limits <- 
          segment.limits[c(newi, length(segment.limits))]
   }

   if (add.integers)
       labels <- paste(labels, 1:length(labels), sep=sep)
	   
   result <- factor(rep(NA,length(times)),levels=unique(labels))		   
}


# remove any NA's inside segment.limits, with corresponding removal for labels
labels <- labels[which(!is.na(segment.limits))]
segment.limits <- segment.limits[which(!is.na(segment.limits))]
if (any(diff(segment.limits) < 0 ))
  stop("`segment.limits' must be strictly increasing.")

#cat("Collapsed...\n")
#print(labels)
#print(segment.limits)

for (i in 2:length(segment.limits)) {
	idx <- times >= segment.limits[i-1] & times < segment.limits[i]
	result[idx] <- labels[i-1]
}

result

}
