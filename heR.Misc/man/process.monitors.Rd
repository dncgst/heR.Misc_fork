\name{process.monitors}
\alias{process.monitors}
\title{Process data streams from real-time monitors}
\description{
This function provides in-place processing of any number of ASCII files that contain real-time monitoring data.  It automatically generates structured long-format and wide-format master data frames, merged lookup and time-activity information, time-aggregated data frames, diagnostic plots, statistical summaries, editable SVG plots, etc. 

This function was originally designed as a full-featured engine for automatically processing environmental data, such as contaminant levels, temperature, air flow, etc.   If you have readings from dozens or hundreds of sensors that need to be processed to a usable format for statistical analysis or plotting, and especially if they must be merged with auxiliary or time-activity information, then this function can save you many hours of labor!
}
\usage{
process.monitors(
		# An overall name for the data-processing run
		name, 
		
		# Starting-time arguments
		start = "auto-first", trunc.start = "none",
		trunc.bykey.start = "none", start.expression = NULL,

		#  Data-file processing arguments
		prefix = "Sidepak", extension = "csv", csv = TRUE, skip = 0,
 		comment.char = "", id.names = c("LoggerType", "LoggerID"),
		splitchar = "_", merge.id.names = NULL,
		value.prefix = 	"Concentration",
		colClasses=NA,
		
		#  Data-file date-time processing arguments
		format = "\%d/\%m/\%y \%H:\%M:\%S", tz = "",
		date.names = c("Date", "date", "DATE"),
		time.names = c("Time", "time", "TIME"), datetime = FALSE,
		relax.increasing = FALSE, remove.nonincreasing=FALSE
		adjust.nonincreasing.times = FALSE,
		adjust.nonincreasing.secs = 1e-06, datetime.offset.sec = NULL, 
		
		# Arguments specifying variable names for final data base
		datetime.name = "Time.POSIX",
		value.name = "Value", value.id.name = "Response",
		
		# Arguments for different ways to process the data
		split.name = NULL, subset.pre = NULL, subset.post = NULL, 
		pre.process = NULL, value.process = NULL, post.process = NULL,
		computed.key.names = NULL, computed.response.names = NULL, 
		elapsed.bykey.names = NULL, convert2numeric = TRUE,
		
		# Aggregation arguments
		make.agg = FALSE, make.wide = FALSE, interval = c(1, 5, 60),
		unit = "mins", fun.aggregate = mean, reshape.formula = NULL, 
		reshape.function = mean,
		
		# Arguments for including variables from data files (besides time/date/responses)
		includes = FALSE, include.id.names = c("Location"),
		includes.as.events = TRUE, 
		
		# Arguments for merging event variables, based on time and key vars
		merge.events = FALSE, event.key = value.id.name,
		datetime.events = FALSE, events.prefix = "Events",
		events.extension = "csv", events.format = format, 
		event.value.prefix = c("Loc", "Act", "Event"),
		event.numbers = FALSE, comment.char.event = "",
		date.names.events = date.names, time.names.events = time.names, 
		
		# Arguments for merging extra lookup variables, based on key vars
		merge.lookup = FALSE, lookup.key = value.id.name,
		lookup.prefix = "Lookup", lookup.extension = "csv", 
		lookup.value.prefix = c("Pos"), comment.char.lookup = "",
		
		# Arguments for plotting the data
		plot = FALSE, make.svg = FALSE, make.ps = FALSE,
		make.bitmap = FALSE, type.bitmap = "png16m", make.pdf = TRUE, 
		plot.formula = NULL, plot.group.key = value.id.name, 
		plot.event.names = event.value.names, plot.conditioning = NULL, 
		plot.conditioning.events = NULL, layout = c(1, 1), 
		width = 11, height = 8.5, cex = 0.4, 
		rot.tod = 0, cex.lab = 1.7, cex.axis = 1, cex.main = 1.9,
		type = "l", main = paste(name, "response plots"), plot.tod = TRUE, 
		plot.format = "\%a \%I:\%M\%p", plot.interval = "hour",
		relation.x = "free", relation.y = "free", log.y = FALSE,
		log.x = FALSE, ylab = "Response Value", as.table = TRUE,
		xlab = ifelse(plot.tod, "Time of Day", paste("Elapsed", unit)), 
		col.lines = c("blue", "red", "black", "green", "orange", "magenta", 
		"cyan", rainbow(12, alpha = 0.4)),
		col.areas = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", 
		"#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2",
		rainbow(12, alpha = 0.4)),
		
		# Arguments for generating statistics
		make.stats = FALSE, na.rm = TRUE,
		
		#  Miscelleneous control arguments
		save.csv = FALSE, debug = FALSE, ...)
}
\arguments{
  \item{name}{
Name used to identify generated files for a given run. This is REQUIRED.
}
  \item{start}{
The time at which the elapsed time variable will be set to 0. Can be "auto-first", "auto-byfile", or a time value in appropriate time \code{format}
}
  \item{trunc.start}{
How to truncate overall starting time for a given processing run, can be "none" (default), "secs", "mins", "hours", "days"}
  \item{trunc.bykey.start}{
How to truncate starting time at beginning of data segments (combination of key variables), can be "none" (default), "secs", "mins", "hours", "days".  See \code{elapsed.bykey.names}.
}
  \item{start.expression}{
Expression to use to assign a starting time for a given data file, takes precedance over \code{start}
}
  \item{prefix}{
Prefix used to match the ASCII files that will be processed
}
  \item{extension}{
Extension used to match the ASCII files that will be processed
}
  \item{csv}{
Whether ASCII data files are in CSV format or space-delimited format
}
  \item{skip}{
Number of lines to skip at start of ASCII files before reading data records
}
  \item{comment.char}{
The character in the ASCII files that designates a comment that will not be processed
}
  \item{id.names}{
A vector of variable names to assign to the parsed variables in each ASCII data file name, number must match the actual variables embedded in each file name
}
  \item{splitchar}{
The character expected to separate variables in the input ASCII data file names
}
  \item{merge.id.names}{
Variables names whose values will be merged into the response variable \code{value.id.name} values
}
  \item{value.prefix}{
The expected prefix for all value, i.e., response, variables in the ASCII data files
}
  \item{colClasses}{
A list of class vectors, with elements such as 'character' or 'numeric', for each value in a file specifying
the class for each file variable.  One vector in the list per file prefix.
}
  \item{format}{
The expected format for the combined date/time variable in the ASCII data files 
}
  \item{tz}{
The time zone for times in processed files
}
  \item{date.names}{
Reserved names for the date variable in the ASCII files
}
  \item{time.names}{
Reserved names for the time variable in the ASCII files
}
  \item{datetime}{
Whether or not there is a single date/time variable or separate variables for date and time
}
  \item{relax.increasing}{
Whether to allow records that do not have increasing times to "pass through"
}
  \item{remove.nonincreasing}{
Whether to remove nonincreasing records, regardless of relax.increasing value
}
  \item{adjust.nonincreasing.times}{
Whether to adjust non-increasing times, making them increasing by adding very small values; regardless of relax.increasing value
}
  \item{adjust.nonincreasing.secs}{
Number of seconds to use in adjusting non-increasing time values to make them increasing
}
  \item{datetime.offset.sec}{
Number(s) of seconds to use in offsetting (adjusting) all time values, one per file prefix
}
  \item{datetime.name}{
The name to use in the generated data frames for the standardized POSIX date/time variable
}
  \item{value.name}{
The name to use for the value (response) variable in the generated data frames, default is "Value"
}
  \item{value.id.name}{
The name to use for the value identification variable in the generated data frames, default is "Sensor"
}
  \item{split.name}{
Variable name to use in splitting data files with a respective prefix that contains multiple responses indicated by a single grouping variable, one per file prefix.
}
  \item{subset.pre}{
Expression(s) in quotes for subsetting the raw data file (i.e., including/excluding raw, unprocessed records according to a logical rule), one per prefix
}
  \item{subset.post}{
Expression(s) in quotes for subsetting data after post-processing, one per prefix
}
  \item{pre.process}{
Expression(s) in quotes for pre-processing raw parsed data (before any time parsing, merging or other processing), one per file prefix;
Processing occurs _before_ any event/lookup merging or time processing. 
Allows for new variables to be created for use in event/lookup merging or date-time processing.
New variables be used as merge keys for events and lookup data.
}
  \item{value.process}{
Expression(s) in quotes for processing the values of response variables, one per *value* prefix;
This special processing occurs _after_ event/lookup merge during processing of a given response value.
Any type of processing can occur but this is designed to be used conveniently for custom transformation of each unique type of response value.  Note that each element is applied to one of the matched type of value prefixes (not the file prefix as with pre/post processing).
}
  \item{post.process}{
Expression in quotes for post-processing the final assembled long-format data base, one value per file prefix,
to be evaluted on a per prefix basis after all other assembly, merging, and processing is finished.
}
  \item{computed.key.names}{
Declared names for computed key variables, i.e., referenced in pre-processing above, needed if variables are to be referenced in later processing/plotting
}
  \item{computed.response.names}{ 
Declared names for computed response variables, i.e., referenced in pre-processing above, needed if variable are to be referenced in later processing/plotting
}
  \item{elapsed.bykey.names}{
Variable names to use in calculating segmented elapsed time, if null, then no segmented time variables will be generated
}
  \item{convert2numeric}{
Whether to explicitly convert also responses to numeric format
}
  \item{make.agg}{
Whether to generate (long format) aggregated data files
}
  \item{make.wide}{
Whether to generate wide format aggregated files
}
  \item{interval}{
Vector containing all intervals to be used in generating time factor variables and aggregrated the data values
}
  \item{unit}{
The unit of time to use for generated time-factor variables, defaults to "min", i.e., minutes
}
  \item{fun.aggregate}{
Custom function(s) to use in aggregating data, one per prefix
}
  \item{reshape.formula}{
Custom formula to use in reshaping data
}
  \item{reshape.function}{
Custom function to use in reshaping data
}
  \item{includes}{
Whether extra variables from each raw data file (in addition to time and response variables) should be included in final data base
}
  \item{include.id.names}{
Vector of variable names in each raw data file to include in final data base
}
  \item{includes.as.events}{
Whether to use included variables as event variables when plotting
}
  \item{merge.events}{
Whether to merge time-dependent event variables in the final data base from one or more
ASCII files.     Events are
merged based on a date and time variables (or combined date-time variable) and matched
to raw data based records based on one or more key variables.
}
  \item{event.key}{
One or more variables to use as key(s) in matching events to responses.  
Within each key or key combination the corresponding times must be increasing.
}
  \item{datetime.events}{
Whether a variable in the events input file contains both date and time
}
  \item{events.prefix}{
Prefix for the ASCII events input file(s).   Multiple files with event data can be
merged into a single file or will be merged automatically if more than one file
is matched with the prefix.  Event data will be sorted by event merge key(s) and times.
}
  \item{events.extension}{
Extension for the ASCII events input file
}
  \item{events.format}{
The data/time format for the ASCII events input file
}
  \item{event.value.prefix}{
The expected prefix for all variables in the event file that will be processed
}
  \item{event.numbers}{
Whether to create duplicate event variables that have an increasing integer appended to subsequent
occurrences of a given event value, allowing for disaggregated statistical analysis of individual occurrences 
of a given event value in addition to across _all_ occurrences (aggregated).  Note:  This
functionality can also be accomplished by manually adding numeric indices to one or more
event variable values in the ASCII events input file.
}
  \item{comment.char.event}{
Character to designate a comment line in the events input file
}
  \item{date.names.events}{
Reserved variables names that can be used for date variables in events input file
}
  \item{time.names.events}{
Reserved variables names that can be used for time variables in events input file
}
  \item{merge.lookup}{
Whether to merge variables from a one or more lookup table files into the final data base.
Lookup variables are simply extra factors that are to be linked to a given response value
or other variable value in the data base.   Lookup variables are merged according to
matched values of one or more key variables.
Important Note:   Merging of lookup variables occurs _after_ merging of time-based events to allow
reference to event variables as merge keys for lookup data.
}
  \item{lookup.key}{
One or more variables to use as the key(s) to match records from the lookup table, defaults to value.id.name
}
  \item{lookup.prefix}{
Prefix for one or more ASCII lookup files
}
  \item{lookup.extension}{
Extension for the ASCII lookup files
}
  \item{lookup.value.prefix}{
The expected prefix for all variables in the lookup file that will be processed
}
  \item{comment.char.lookup}{
Character designating a comment line in the lookup input file
}
  \item{plot}{
Whether to generate graphics files containing diagnostic lattice plots 
}
  \item{make.svg}{
Whether to create plots in SVG format
}
  \item{make.ps}{
Whether to create plots in Postscript format
}
  \item{make.bitmap}{
Whether to create plots in bitmap format
}
  \item{type.bitmap}{
The type of bitmap file to generate
}
  \item{make.pdf}{
Whether to create plots in Adobe Portable Document Format (PDF) format
}
  \item{onefile}{
Whether to create one graphical files with all plots of given type or to 
create multiple files, one plot per page
}
  \item{plot.formula}{
Formula to use in custom plots
}
  \item{plot.group.key}{
Variable that defines groups (series) of data plotted in same panel with \code{xyplot} plots
}
  \item{plot.event.names}{
Event variable names to use in overlaying activity information on plots
}
  \item{plot.conditioning}{
Extra conditioning variables to use in plots
}
  \item{plot.conditioning.events}{
Extra conditioning variables to use in plots that contain overlaid activity (event) information
}
  \item{layout}{
Layout specification for plots, see \code{xyplot}
}
  \item{width}{
Width of PDF, Postscript, Bitmap, and SVG plots in inches, defaults to 11 inches
}
  \item{height}{
Height of PDF, Postscript, Bitmap, and SVG plots in inches, defaults to 8.5 inches
}
  \item{cex}{
Magnification level for plot symbols
}
  \item{rot.tod}{
Degrees to rotate axis labels
}
  \item{cex.lab}{
Magnification level for plot axis labels
}
  \item{cex.axis}{
Magnification level for numbers on axis
}
  \item{cex.main}{
Magnification level for plot title
}
  \item{type}{
Type of plotting symbols to use, defaults to "l" (lines)
}
  \item{main}{
The main title to use for the plots
}
  \item{plot.tod}{
Whether to plot actual clock times in time series plots or to use the elapsed time
}
  \item{plot.format}{
Format specification for plotting clock times on the horizinal axis
}
  \item{plot.interval}{
Interval between plotted clock times on the horizontal axis
}
  \item{relation.x}{
Option to plot time series with the same or free X-scale, can be "free", "same", or "slice".  See lattice \code{xyplot} function.
}
  \item{relation.y}{
Option to plot time series with the same or free Y-scale, can be "free", "same", or "slice".  See lattice \code{xyplot} function.
}
  \item{log.y}{
Whether to plot Y axis values in log scale
}
  \item{log.x}{
Whether to plot X axis values in log scale
}
  \item{ylab}{
Y axis label to use in time series plots
}
  \item{as.table}{
Plot panels in tabular format
}
  \item{xlab}{
X axis label to use in time series plots, if using clock times
}
  \item{col.lines}{
Colors to use for plotting lines
}
  \item{col.areas}{
Colors to use for plotting areas
}
  \item{make.stats}{
Whether to generate statistics
}
  \item{na.rm}{
Whether to remove missing values in statistics or aggregation
}
  \item{save.csv}{
Whether to save all output files in CSV format in addition to RData format, defaults to FALSE
}
  \item{debug}{
Whether to include debugging messages in the output
}
  \item{\dots}{
Additional arguments to plotting functions
}
}
\details{
Scientists and engineers in the environmental field are increasingly using
real-time environmental sensing devices to measure such things as
temperature, humidity, particulate matter, carbon monoxide, air flow, etc.  These devices generate large streams of time-stamped records, which require substanttial processing to be of use.  

This function is highly configurable to deal with any type of logged ASCII data file.  It provides the following features (among others):

\enumerate{
\item Read ASCI data files directly from the working directory
\item Match files based on prefix and extension
\item Generate a master long-format data file with data from ALL MATCHED ASCII files
\item Parse embedded variables in the matched files names and merge into the master data frame
\item Read ASCII files in CSV or space-delimited format
\item Read header line in ASCII files and assign variable names
\item Read multiple response variables in each file using a matched prefix.
\item Process date and time variables to generate a standard POSIX time variable
\item Calculate elapsed-time and time-factor variables across user-specified time intervals to be used in time-aggregation
\item Optionally merge time-stamped events into the master file
\item Optionally merge lookup information about sensors into the master file
\item Optionally generate several kinds of wide-format data
\item Optionally time-aggregate the data by specified time factors
\item Optionally generate diagnostic time-series lattice plots split by all factor variables
\item Optionally generate editable SVG plots of all time series
}

This function has been used to process data from the following types of sensors and logging devices:

\itemize{
\item Langan Databear data logger
\item Onset HOBO data logger
\item TSI Sidepak aerosol monitor
\item TSI IAQ Calc
\item Dylos DC1700 particle counter
\item EME Systems OWL data logger
}

Admittedly, this function can be a bit hard to use at first.   But once you get the hang of it, it is a total breeze and I promise it will save you hours or days of work.   I will be posting detailed tutorials on the ExposureScience.Org website in the near future.  In the meantime, please shoot me an email if you are confused, and I can get you started quickly.
}
\section{Basic Instructions to Use This Function}{
First, start R in a working directory that contains a number of ASCII files to be processed.  The files can be in CSV or space-delimited format. They must have a header record that gives the names of the variables in each column.   The files can have embedded information in their names.   Each file must contain columns of data for 1 or more sensors (i.e., response value).   Date and time variables (or a contained date-time variable) are also required. 

Any kind of sensor-specific information can optionally be matched with the approprieate sensor records, and the time of its readings, by specifying lookup and event files.

Next, run the R script with appropriate argument values, and processed data frame objects and corresponding CSV files will automatically be generated, which contain the concatenated and structured data in long-format.  This format is suitable for plotting with Lattice and conducting statistical analysis and modeling.   

Optionally, the script can generate time-aggregated files (using the reshape package) for 1 or more time groups, as specified by the \code{interval} argument.   Also optionally, the script will generate many diagnostic Lattice time-series plots split by the different factors either embedded in the file names or included in the lookup and event files.   The plots can be generated in Postscript (default) or in editable SVG format if the RSvgDevice package is available.
}
\value{
  This function does not return a value.  It saves various data and graphic files to the current directory.
}
\references{
http://exposurescience.org/heR/
}
\author{
Neil Klepeis
}
\keyword{misc}
