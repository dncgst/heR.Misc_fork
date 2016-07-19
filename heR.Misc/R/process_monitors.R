process.monitors <-
function (#  An overall name for the data-processing run
		  name,   

		  # Starting-time arguments
		  start="auto-first", trunc.start="none",
		  trunc.bykey.start="none", start.expression=NULL,
          
		  #  Data-file processing arguments
		  prefix="Sidepak", extension="csv", csv=TRUE, skip=0,
		  comment.char="", id.names=c("LoggerType","LoggerID"),
		  splitchar="_",
		  merge.id.names=NULL, value.prefix="Concentration",
		  colClasses=NA,
		  new.header=NULL,   #  Specify vector of new header strings to add to raw file (one per file prefix)
		  
		  #  Data-file date-time processing arguments
		  format="%d/%m/%y %H:%M:%S", tz="",
		  date.names=c("Date","date","DATE"),
          	  time.names=c("Time","time","TIME"), 
		  datetime=FALSE, relax.increasing=FALSE, remove.nonincreasing=FALSE,
		  adjust.nonincreasing.times=relax.increasing,
		  adjust.nonincreasing.secs=0.000001,
		  datetime.offset.sec=0,
		  
		  # Arguments specifying variable names for final data base
		  datetime.name="Time.POSIX", 
          	  value.name="Value", value.id.name="Response",

		  # Arguments for different ways to process the data
		  split.name=NA, subset.pre=NULL,   subset.post=NULL,
		  pre.process=NULL,     # process individual sensor data file before assembling
		  value.process=NULL,   # process sensor values (or other vars) after initial file processing
		  post.process=NULL,    # process assembled long data of all data files
		  computed.key.names=NULL,   # declared new variables for use as key vars
		  computed.response.names=NULL, elapsed.bykey.names=NULL,   # declared new variables for use as response vars
		  convert2numeric=TRUE,   # coerce response values to numeric

		  # Aggregation arguments
  		  make.agg=FALSE, make.wide=FALSE,
	          interval=c(1,5,60), unit="mins", fun.aggregate=mean,
  		  reshape.formula=NULL, reshape.function=mean,
		  
		  # Arguments for including variables from data files (besides time/date/responses)
		  includes=FALSE, include.id.names=c("Location"),
		  #merge.includes=FALSE, include.value.prefix=c("Location"),		  
		  includes.as.events=TRUE,   # for plotting purposes only
		  
		  # Arguments for merging event variables, based on time and key vars
		  merge.events=FALSE, event.key=value.id.name,
		  datetime.events=FALSE,
	          events.prefix="Events", events.extension="csv", events.format=format,
		  event.value.prefix=c("Loc","Act","Event"), event.numbers=FALSE,
		  comment.char.event="",
		  date.names.events=date.names, time.names.events=time.names,
		  
		  # Arguments for merging extra lookup variables, based on key vars
		  merge.lookup=FALSE, lookup.key=value.id.name,
	          lookup.prefix="Lookup", lookup.extension="csv",
  		  lookup.value.prefix=c("Pos"),
		  comment.char.lookup="",
          
		  # Arguments for plotting the data
		  plot=FALSE, make.svg=FALSE, make.ps=FALSE, make.bitmap=FALSE,
		  type.bitmap="png16m", make.pdf=TRUE, onefile=TRUE,
		  plot.formula=NULL, plot.group.key=value.id.name,
		  plot.event.names=NULL,
		  plot.conditioning=NULL, plot.conditioning.events=NULL,
		  layout=c(1,1),
	          width=11, height=8.5, cex=0.4, rot.tod=0,
	          cex.lab=1.7, cex.axis=1, cex.main=1.9, type="l",
		  main=paste(name,"response plots"),
	          plot.tod=TRUE, plot.format="%a %I:%M%p", plot.interval="hour", 
		  relation.x = "free", relation.y = "free", log.y=FALSE, log.x=FALSE,
		  ylab="Response Value", as.table=TRUE,
 	          xlab=ifelse(plot.tod, "Time of Day", paste("Elapsed",unit)),          
		  col.lines=c("blue","red","black","green","orange","magenta","cyan",
						rainbow(12, alpha=0.4)),
	          col.areas=c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC",
					"#E5D8BD","#FDDAEC", "#F2F2F2", rainbow(12, alpha=0.4)),

		  # Arguments for generating statistics
	          make.stats=FALSE,  na.rm=TRUE,
		  
		  #  Miscelleneous control arguments
		  save.csv=FALSE, debug=FALSE, warn=0, ...) {

#  NEW, MORE GENERAL DESIGN:    Parse 1 or more response variables per file
#     and create a new index variable in the master file using their names. 
#     This new index is used to plot diagnostics and make a WIDE format file.   
#     The indices embedded in the file name are meant to apply to ALL the
#     responses present in a given file and are also stored in the master file.
#     ALSO:  We now parse for key PREFIX words in the date, time, or response
#      columns instead of making user specify the column number.  The user
#      simply has to make sure that all the responses in a given file start
#      with the specified keyword (e.g., CO or Aerosol) and 
#      the date and time variables also (e.g., Date and Time).a
#                               --- NK  27-April-2008
#      update:  Can specify multiple value.prefix values and we will
#       select all that match.  now working.  NK 3-June-2008

# Function to read in a number of real-time monitor data files,
#   create a standard time variable, elapsed time, time factor, 
#   id variable, and create a combined file.   Elapsed time keys from
#   time at start of first file if "start" is NULL. Also creates an 
#   aggregate data file, averaging over specified time interval.
#   Optionally create diagnostic plots.
#   Concentration column must be specified as "value.field".
#   "name" must be a character string that is used to name the combined files.
#            Neil Klepeis,  13-Sept-2007
#
#  ----
#
#  INPUT ARGS
#
#  name - base name of resulting file, e.g., "CO" to give "CO.RData" or "CO.csv"
#  start - optional time to start elapsed time variable, default is first time
#           of first file
#  prefix - search for files that begin with this prefix, e.g., "CO"
#  extension - search for files that have this extension, e.g., ".txt"
#  format - format for date-time concatenated variable
#  datetime - whether the date field contains both date & time info (TRUE) or
#      the datetime variable is the concatenation of separate date/time fields
#      (FALSE)
#  date.prefix - the prefix for columns of the DATE or DATE-TIME in the file
#  time.prefix - the prefix of the TIME in the file, if NULL assume date field
#                contains date-time information
#  value.prefix - the prefix of the measured VALUE(S) (response(s)) in the file
#               CAN GIVE MULTIPLE DIFFERENT PREFIXES AND WE SELECT THEM ALL
#  datetime.name - name to assign to the date-time variable in the final file
#  value.name - the name of the column containing the response values 
#                  "Value" by default
#  value.id.name - the name to use for the value ID column in the final file
#                   "Sensor" by default
#  interval - time interval to use in aggregating/calculating elapsed time
#  unit - unit of time used for the interval, e.g., "mins" or "secs"
#  csv - whether or not the input file is in CSV format or space-delim format
#  splitchar - the character used to split the filenames to extract information
#  id.names - the names to use for index values embedded in each file name;
#    length must match the actual number in each filename or an error is
#    returned
#  plot - whether to create a diagnostic plot of the data or not
#  plot.id -  NOT USED!!
#             the index of the id.names to use as a conditional variable in
#             the diagnostic plot
#  main,xlab,ylab - the title and axis labels for the diagnostic plot
#  make.wide - whether to additionally create WIDE format data files with
#             responses in separate columns and a common time and index 
#             variables (TRUE) or to just make the LONG format version of
#             the data where the data are stacked on top of each other (FALSE).
#  XXX align.round - NOT USED !!
#                 number of digits to round the Elapsed variable to before
#                aligning files into the WIDE format, defaults to 0, Note:
#                units of the Elapsed variable are given in "unit" arg. which
#                defaults to "min".    DON'T NEED THIS IF WE ONLY "WIDEN"
#                 THE AGGREGATED FILE....
#  wide.id  -   NOT USED !!
#                the index to use for making the WIDE filing using
#                the reshape "cast" function.
#  ----

# UPDATE:  Internally changed from using taken name "data" to "rdata" for raw data just read in... NK 29Jan2016
# UPDATE:  New "new.header" argument for specifying a header to be added to a raw data file by prefix.  NK 28jan2016
# UPDATE:  Fix to include all events names in aggregration. (events.value.names.list).  NK 26Jan2016
# UPDATE:  Won't fail if a file prefix is not found.. just skip... NK 22Jan2016
# UPDATE:  datetime.offset.secs is now 0 by default (instead of NULL) - 20April2014
# UPDATE:   Fixed bug when event.numbers=TRUE for merge events that
#             forced error when there are no Events records to merge. NK 20April2014
# UPDATE:   Added remove.increasing argument to optionall remove all records with non-increasing times..  6/26/2013
# UPDATE:   Fixed grep matching of date and tmie names to be AN EXACT match now.  26June2013
# UPDATE:   added subset.pre and subset.post for subsetting raw data file or subsetting
#				data after any post-processing (e.g,. by datetime or any original or computed variable).
#							26June2013
# UPDATE:   Make split.name replicated for each file prefix.   6/19/2013.
# UPDATE:   New argument to set warning level (warn=2 turns warnings into errors).  6/19/2013
# UPDATE:   Not done yet!!  Allow for different include vars matches per file prefix. 6/13/2013.
# UPDATE:   colClasses is now specified per file prefix.  6/11/2013.
# UPDATE:   Added colClasses argument for inputing data files, to specify custom
#				variable types if necessary.    3/8/2013.
# UPDATE:   Added ability to have multiple event files.  Processing and merging each one 
#				individually.  Also must specify separate event.key(s) for each file.
#									NK 3/8/2013
# UPDATE:   Added a warning if there are no event key value matches for a given file
#				and value name.   NK  3/3/2012
# UPDATE:   Added onefile logical argument to determine whether graphics files should be
#			saved to a single file or multiple files (one graph per page).  NK 14-Dec-2011
# UPDATE:   Reactivated the "event.numbers" argument where we, if TRUE, now create
#				duplicate event variablest have sequential numbers added to event
#				values so disaggregated analysis can occur by individual event value
#				occurrences.  NK 13-Dec-2011
# UPDATE:   Reorganized order of argumentns to make orde and grouping more logical.
#			Updated documentation Rd file.    NK 12-11-2011
# UPDATE:   Make date/time prefixes to date/time "names" instead.  These are now considered
#				reserved words for choosing date and time variables and therefore cannot
#				be used for other names.   Before we used them as a prefix and the actual
#				names were not identified until run time.   NK   12-10-2011
# UPDATE:    add `trunc.bykey.start' argument giving units to truncate all of the
#				starting times for bykey time variables (restart to 0 at beginning of
#				each new value of the specified key variables).  
#				Can have values "none", "secs", "mins", "times", "days".  - 9-Aug-2010
# UPDATE:    Move pre.process _before_ subset so we can subset based on any new
#				variables.... NK  27-Jul-2010
# UPDATE:    Added save.csv=FALSE argument (by default FALSE) to control whether CSV
#				files are written.   The write.csv command is buggy in R 2.11.0 and can 
#				produce very large files... so I disabled it by default.. NK 26-Apr-2010
# UPDATE:    Added an option to calculate an extra variable for elapsed time starting
#				at "0" at each (merged) key variable juncture.  Useful for calculating
#				aggregated statistics that start evenly at beginning of time segments
#				defined by event or lookup vars (or any key).  Specify the key variables
#				to use in the elapsed.bykey.names argument... if NULL, then no extra
#				time factor variables are created...  Done.
# UPDATE:  put back the ... for plotting parameters
# UPDATE:   Added plot.conditioning.events argument, so we can specify conditioning
#			just for event plots, since these are the ones that are more likely to
#			need splitting and the other plots have instrinic conditioning already and we
#			may not want include the events-specific conditioning in them.  nk 12-April-10
# UPDATE:   Convert time factor variables (elapsed time) to numeric in AGG data... 
# UPDATE:    Added "log.x", "log.y" arguments for scales in plotting.  Can be "TRUE" for base 10
#					"e" for natural logarithm, or another number for another base.
#						  10-Apr-10
# UPDATE:    Set new defaults  start="auto-first" and trunc.start="none" so that by 
# 				default we have a fixed starting time (and thus consistent set of elapsed
#				time factors across all responses/files) that is not truncated.  User
#				can change the starting time/trunction as required, but these new defaults
#				are the most intuitive, I think..  NK 3-Apr-2010
# UPDATE:    Now use rbind.fill to make sure we fill in NA's if new computed names are
# 				not the same for different prefixes/responses...that is if we forget to
#				declare them with the computed.key.names or computed.response.names
#  				arguments (the former is only really necessary to let aggregation functions
#				know that a new key variable is available to use).  nk 2-Apr-2010
# UPDATE:   Fixed bugs in making WIDE files (merging files), multiple lookup files (merging
# 			  all lookup files, and merging id names (can be any taken key names now).
#						NK 26-Mar-2010
# UPDATE:   Allow for multiple lookup files
# UPDATE:    Fixed some of the AGG code to merge columns adding NA in rows even if there
#           are no column matches.   We also use Hadley Wickhams, rbind.fill command in
#             the plyr package to add missing columns when binding two data frame, e.g.,
#             when WIDE-AGG time-in-column reshaping gives different times for different responses
#             (problem arose because we are applying different aggregration functions to
#             responses that match different value.prefixes).  nk March-25-2010
# UPDATE:   Fixed some bugs in the aggregation to match beginning of strings and include
#  			all data in merged wide files.   24-Mar-2010
# UPDATE:  Removed "..." argument for plotting...prevented checking whether unknown
#            arguments were passed...
# UPDATE:  	Moved value.processing after events/lookup merge so we can refer to these
#				variables when doing processing specific to value prefix matches.
#					  23-March-2010 NK
# UPDATE:   Fixed bug that let .GlobalEnv mask saved objects. Now when saving we refer to
# 			env=sys.nframe()  [the current frame]  so that objects created in this function
#            are saved in preference to any identically named objects in parent frames.
#					NK 9-March-2010
# UPDATE:    For aggregrated data:  sort by POSIX time first now instead of Elapsed Time.
#               This makes it so the resulting data frame is sorted by increasing times
#               even if elapsed time is reset between raw data files.  NK 9-March-2010
# UPDATE:      put as.data.frame() wrapper around aggregated data (agg/merge/stat) to
#            clean residual 'reshape' (melt/cast) attributes that can interfere with any
#            further melting and casting.  NK 8-March-2010
# UPDATE:  		Fixed bug that didn't include events/lookup variable names in new data
# 				frame if only 1 variable was merged... NK 5-March-2010
# UPDATE:     Added option to adjust the times that are not increasing, adjust.nonincreasing.times=TRUE,
#             (set equal to relax.increasing by default) by adding sequential microseconds
#				(by default) to the final time variable, i.e., for each set of non-increasing times
#				in a given file, 1:N adjust.nonincreasing.secs are added (in order) where N is the
#				number of times, `adjust.nonincreasing.secs' is by default a microsecond.  !!! For times
#               significantly under a second, this is likely to show up only internally
#               to make it possible to make times series plots with overlaid events !!!
#              This adjustment of nonincreasing values is done after any time offset 
#             ajustment which is done to ALL the time values not just nonincreasing ones.
#					NK 3-March2010
# UPDATE:    Merging of events now comes _before_ merging of lookup variables so that
#            we may use time-based merged variables as keys in the lookup merge.  This
#            is useful if, e.g., we have more than 1 experiment's worth of data in a 
#            given file and we'd like to tag each one in time, and then assign other
#            data, e.g., spatial position, to the sensors based on the experiment. 
#                     NK  7-Feb-2010
# UPDATE:    Can now have different fun.aggregation functions (passed to cast) for
#             different matched value.prefix'es... fun.aggregation should be same
#             length as value.prefix.    NK 5-Feb-2010
# UPDATE:   Changed merge.id.names procesing, so now we can specify any
#             taken.key.name to merge into the response key name..  5-Feb-2010
# UPDATE:   Have new value.process commmand that is keyed on the value.prefix, allowing
#           for processing commands specific to the different matched value names.  In
#            contrast, the pre/post commands are specific to a file prefix.   4-Feb-2010
# UPDATE:   We now remove lookup and event rows that contain NA for any key values..  3-Feb-2010
# UPDATE:   Fix event merging in case events.key contains NA values and allow for
#              use of multiple keys...  Rewrote the event merge section.   NK, 2-Feb-2010  
# UPDATE:  If the custom start evaluates to NA, then we assign the auto-byfile value...
# UPDATE:   OK.  We can use the old 'within' function with within(x,eval(parse(text=""))
#                  so go back to that....
# UPDATE:   Actually the built-in `within' doesn't seem to work when using
#              parse(text="") as the expression argument.   So I wrote a new
#             within.data.frame.2 that accepts commands as text strings.  NK 4-Jan-2010
# UPDATE:    Now both PRE and POST processing used the builtin R commmand `within' to
#            evaluate expressions in terms of the raw or processed data file, respectively.
#              This allows us to avoid manually adding data$ or newdata$ strings to
#             matched variable names.  We can now have variable names that partially match
#              but not fully.   NK  1-Jan-2010
# UPDATE:   Add pre.process argument to specify expression(s) to evaluate specific to
#            each prefix in the context of each raw file -- allows manipulation of
#            response (or include variables) before they are put into LONG format.
#					NK.  1-Jan-10
# UPDATE:   misc fixes to error output.   NK 12/31/09
# UPDATE:    misc fixes: add relax.increasing argument, printout where time is not increasing
#                       12/15/2009
# UPDATE:   Changed method to check for duplicate names to use `charmatch' command, which
#            checks for partial or full matches and returns NA if neither found.  We
#			don't want to use a name that is part of another name, since it screws up
#           the parsing of commmands for post.processing and start.expression.  NK 12/5/09
# UPDATE:   Added new option "custom" for `start' with sister argument start.expression
#          with an expression to evaluate in the context of the current dataframe
#             (with all taken.names available) and return an index for where the
#           starting time should be defined.    Seemed to have workout the timezone
#            issues by coercing `start' to as.POSIXct(as.POSIXlt(x, tz=tz))
#         (need both bc only POSIXlt uses tz) ... NK 12/5/09
# UPDATE:   Added reshape.formula and reshape.function arguments to apply one or more custom reshaping
#			formulas to the final melted LONG format data file.   NK 12/4/09
# UPDATE:  Any new (computed) variables created during post.processing must be 
#          declared using the computed.key.names or computed.response.names arguments.
#						NK 11/9/09
# UPDATE:   Added a post.process argument for each element of "prefix" that contains
#     a string of arbitrary commands to evaluate in the context of each individual 
#       response component of the data base.   NK 11/9/09
# UPDATE:  New convert2numeric logical argument, defaulting to TRUE, specifying whether
#           response vectors will be converted to numeric, making non-numeric values
#            NA.   If FALSE, then factors and non-numeric vectors will produce a
#            fatal error.   9 Nov 2009--NK
# UPDATE:  Added a datetime.offset.sec argument that is replicated for each "prefix" and
#        if non-NULL is assumed to contain a number of seconds that are added to each
#        date-time variable.  NK 11/5/09
# UPDATE:   Better design for merging lookup variables, can now key on multiple 
#				variables (TODO: should also do for events...).  NK 10/7/09
# UPDATE:   Can now plot multiple types of graphics file during same run. 
#            Created functions for each type of plot..  NK 10/7/09.
# UPDATE:   Important: Reshape command "melt" adds things to a data frame -- so when columns are
#            deleted the data frame needs to be cleaned with a df <- as.data.frame(df). 
#             No change made in code of process.monitors, just keep in mind for future
#               analysis.   NK 8/26/09
# UPDATE:   Added a "subset" string argument with embedded logical arguments for pre-processing
# 				data files by removing unwanted rows -- with character elements specific to each
#					'prefix', which are evaluated in turn. 		NK - 16-Jul-2009
# UPDATE:   Added a "split.name" argument giving a column factor that will be used for
#                splitting the datafile before processing.  We then loop over each component
#                dataframe after the splitting.   Split.name values added as a factor to final data.
# UPDATE:  Add option "merge.id.names" containing file id key variables to merge with the response in
#           each file.  This allows for differentiating, e.g., Sidepaks where Aerosol is the native
#            column name but we add a sidepak number to the filename.
# UPDATE:   added '...' argumet for lattice plot extra variables, and as.table is a parameter now...
# UPDATE:   Get plot.tod=TRUE working to plot time of date (tod) info on x-axis instead of 
#                   just elapsed time...NK 11-July-2009
# UPDATE:  Added separate datetime logical for events table....
# UPDATE:   Revamped all AGG and WIDE data reshapings.  Now do time-only, raw, time+events, etc. versions of 
#             reshaped data.  LOTS of files produced!... more stats... NK 10-July-2009
# UPDATE:    pass scales parameter, which is passed directly to xyplot
# BIG UPDATE:   Allow for reading in different types of files by allowing parallel lists of certain
#           parameters, i.e., prefix, extension, csv, skip, comment.char, format, ylab, and value.factor.
#					and "datetime"!!
#  UPDATE.    Now if the event file does not contain the event.key var, we merge ALL of the events into
#               each data file, rather than merging according to event.key.  NK 9-July-2009..
#  UPDATE.    include "includes" in event plots by default.  NK 8-July-2009
#  UPDATE.    Added stats across all events+fileid combos...
#  UPDATE.    Fixed bug so start would not work if "auto-first"...
#  UPDATE.      We now plot nice lattice plots for all events.
#  UPDATE.   Add plot.event.names argument to only use a submit of available event variables.
#  UPDATE.   Add a "plot.conditioning" variable that species names of any additional conditioning
#			  variables to use in lattice plots in addition to the instrinsic ones used,
#			  e.g., for fileid variables or lookup variables.  May duplicate intrinsic ones
#             that are used automatically.   Defaults to NULL.
#  UPDATE.   Add a "layout" argument that gives the layout for any lattice plots.
#              defaults to 2x2 per page.
#  UPDATE.   Now use new xtact function to make lattice plots with time-activity
#              overlays.    16-June-2009
#  UPDATE.   Make numbering of events optionally, defaulting to FALSE.
#  UPDATE.    Fixed up the event merge stuff to now have a customizable key variable for
#              choosing events from the file (default to value.id.name).  Also gives warning/error
#              if duplicate times in either data or event file.  NK 3-June-2009
#  UPDATE.   Now create pdf as well as ps files when make.svg=FALSE.  3-June-2009
#  UPDATE.  Add variables with the POSIX time interval factors (in addition to those based
#            on elapsed numeric time), which are used as the time variable in the aggregated
#					files.  NK 3-June-2009
#  UPDATE.  Option to truncate start time to nearest day, hour, min, sec.
#              DEFAULTS TO "days" SO THAT THE ELAPSED TIME IS "elapsed time
#               after midnight".  Makes aggregated times more easily aligned
#              since even interval starting times will be generated rather than
#              just starting at the first time in each file.
#                   !!  Use "none" to disable truncation.  NK 2-June-2009
#  UPDATE.  Added plot.group.key, which gives the variable that will be used in
#            plots to differentiate groups on the same plot.   Defaults to
#            value.id.name, but can be any factor.   NK 1-June-2009.
#  UPDATE.  for plotting seq time removed mday + 1. Now just mday.
#  UPDATE.  Added option for start as "auto-byfile" and "auto-first" to
#           allow for automatic assignment of time origin as first record in each file
#           or the first record in the first file.  Otherwise, start is interpreted
#           as an actual time to use.   NK  1-June-2009
#  UPDATE.  Added a 'debug' argument for having the function print
#           out more intermediate results to facilitate finding
#            problems.
#  UPDATE.  Allow for factor variables to be "included" in each data file
#             must match specified prefix.   "includes" argument must
#             be TRUE and the include.id.names must contain exact word
#             matches for vars in each data file.   This way we can 
#             determine if each file has the same included variables and
#             return an error if not.    This option is actually most
#             useful when the analyst decides to put all the response data
#             from multiple sensors in 1 file along with the factor data
#             in another column.   --NK 30-Nov-2008
#  UPDATE.  Now we can key the lookup table on the value.id.name (by default)
#            Or any of the FILE ID variables.... NK 28-Aug-2008
#  UPDATE.  Add "scale.relation" can be "free" or "same" or "sliced"
#             See ?xyplot in lattice package.
#  UPDATE.  Now date.prefix and time.prefix can have multiple values
#            although only first match is used at the date or time variable.
#           Also, now specifying multiple values for 'interval' produces
#           an aggregated file across each of the specified intervals.
#  UPDATE.  Misc fixes.  Also, add 'plot.times' and 'plot.format' arguments
#             for specifying whether
#             actual times of day are plotted on x-axes (and their formatting)
#             or if the elapsed variable is used.  14-Aug-2008
#  UPDATE.  Fixed bug that didn't correctly add lookup/event factors to each
#             individual value variable.  NK Jul-24-2008
#  UPDATE.  Optional value.factor to multiply by all values.
#  UPDATE.  Now can calculate lots of statistics tables
#  UPDATE.  Replace the 'aggregate' function entirely with reshapes
#               melt and cast.   The built-in aggregate seems to bog
#               down when we use a lot of factors.. NK 22-Jul-2008

#  UPDATE.  Put make.wide stuff back in.   Added lookup file.  
#              Put in different lattice plots for each id name. 
#              Write postscript or SVG graphics files only.   22Jul08
#  UPDATE.  Add "skip" argument to skip lines before reading header/data in
#            each data file (not for event files).
#  UPDATE.  Add an activity-response areaplot for each "value" variable
#               if merge.events=TRUE
#  UPDATE.  Add merging of events into the data base from a separate CSV
#                file.  Done.
#  UPDATE.  Clean up code and make work for CO and Aerosol data files.
#  UPDATE.  Add option to creat AGG files. ....
#  UPDATE.  19-Apr-2008.  Add option to create WIDE format data files.
#                         using the reshape R package.. Only do for the
#                         aggregated file since otherwise aligning them
#                         may be difficult...
#  UPDATE.  27-April-2008.  Now read one or more response variables from
#                         each file and use the column names as a new
#                         index variable, also use this index to make the
#                         WIDE file.  Also use this index to make the
#                         diagnostic plot.
#  UPDATE.  21-May-2008    Now we put the POSIX time value at the beginning of
#                     the time interval into the aggregated file
#
# =====================================================================================
# =====================================================================================
#
# TODO:  Allow for specifying total scales argument or individual x/y arguments for
#				maximum flexibility....    
#
#      Add ability to have multiple lookup and event files with *different* lookup
#			key variables, for most flexibility.....
#    
#      Add different aggregation functions, beside just "mean", for each prefix.
#             --doesn't really seem possible since we aggregate at the very end with all
#				 of the data since we may be piecing together data from different files
#                and aggregating across all of it.  We'd have to somehow iterate over
#                prefix matches or response matches (better) at the aggregation stage...
#
#		Maybe add tolerance for time.factor 0.5*interval so that the
#        specific bin is actually the midpoint of the interval.
#       
#        Optionally have a lookup table (another CSV file) to fold information
#        on each sensor into the database.....Done.

#        Add aggregated files for splits by id.name, value.names, and
#             event.value  (and any other factors...).
#
#       !! +10 **!! Add capacity to specify what factors and factor combinations will be
#             used to plot/aggregate data and generate statistics.
#
#        Just have argument containing formulas to use for plots.  Or maybe just
#          the conditional portion, what key variables to use as conditioning
#          variables.    Program will plot all combinations and single vars.
#
#        Maybe have events be listed both with numbers and without them so we can
#          do both plotting and stats easily.  
#
#		**!!  Tie the value.factor to the different value.prefix, so we can adjust values
#         based on the sensor and not just the file prefix...

#            Also tie the ylab to each Sensor type -- maybe make a factor for the label that
#             gives the units of measurement

#       **!!   Make a series of WIDE files according to different factors.  Have 1 just
#                factored by time so that all responses can be compared regardless of the
#                value of the other ID variables.  done.
#
#        Have way to flag strings in a line of data and ignore it....pre-processing routines...
#         could do separately with a Perl script, but easier to embed into R.
#
#      Have option to translate (or add) the file id names to the response name if there is only a single
#         response value column in the data file.   Avoid having to edit by hand...Done.

#
#			Have better error checking for non-numeric response data.   Say which file and
#             variable is non-numeric.    Maybe only do check if we are multiplying the 
#              reponses by numeric adjustment factors...
#
#			Have way to specify more than one event or lookup key...which then merges them into
#             one variable to do the matching.....Done.

#	 Add  plot.formula argument to create one or more custom lattice plots
#            using the LONG raw data files as the source data.    

#     We really need to clean up the plotting code to make it more general
#        and easier for the user to customize at runtime... made new function plot.monitors
#
#    Do pre checking/filtering of input records to make sure every record that is
#     supposed to be numeric actually is, etc...
#
#	 Split up any parts that are possible into modules that can be run separately
#       e.g.,  output a veriables spec of KEY and RESPONSE variables that can be
#		read in by a plotting function to generate different plots after the raw
#		LONG data are generated..... also have a separate AGG module for computing
#		aggregated data and different stats.....

#    Prevent collision of "Date" or "Time" names used for date.prefix or time.prefix
#			 and simultaneously an id.name or something (hard bug to find)....Done

#	Have multiple event files.....
#
# -------------------------------------------------------------------------

cat("\n\n ** Welcome to `process.monitors', a processor for real-time monitoring data. **\n\n")

#  Set warn=0, print warnings at end, warn =1 print right away, warn >= 2 to turn warnings into errors to help in debugging....
options(warn=warn)

if (missing(name)) stop("Must specify a name for this batch of files.")

if (length(prefix) > 1)
  if (length(prefix) != length(extension) | length(prefix) != length(skip) |
		length(prefix) != length(csv) | length(prefix) != length(comment.char) |
		length(prefix) != length(format) | length(prefix) != length(datetime) |
		length(prefix) != length(datetime.offset.sec) |
		(!is.null(new.header) & length(prefix) != length(new.header)) |
		(!is.null(subset.pre) & length(prefix) != length(subset.pre)) |
		(!is.null(subset.post) & length(prefix) != length(subset.post)) |		
		(!all(is.na(split.name)) & length(prefix) != length(split.name)) |		
		(!is.null(post.process) & length(prefix) != length(post.process)) | 
		(!is.null(pre.process) & length(prefix) != length(pre.process)))
	warning("Length mismatch for one of extension, prefix, skip, csv, format, comment.char, new.header, format, subset, split.name, post.process, pre.process. Replicating to length of prefix.")	
	
extension <- rep(extension, length=length(prefix))
csv <- rep(csv, length=length(prefix))
skip <- rep(skip, length=length(prefix))
comment.char <- rep(comment.char, length=length(prefix))
format <- rep(format, length=length(prefix))	
datetime <- rep(datetime, length=length(prefix))
datetime.offset.sec <- rep(datetime.offset.sec, length=length(prefix))
# not yet!!   includes <- rep(includes, length=length(prefix))
new.header <- rep(new.header, length=length(prefix))  # new.  28Jan2016

if (!is.list(colClasses)) colClasses <- list(colClasses)
for (i in 1:length(prefix))
	if (length(colClasses) >= i) {
		if (all(is.na(colClasses[[i]]))) colClasses[[i]] <- NA
		else colClasses[[i]] <- rep(colClasses[[i]], length=length(prefix))
	} else colClasses[[i]] <- NA	
	
if (debug) {
	cat("\ncolClasses specification:\n")
	print(colClasses)
}

# split data keyed on the file id prefix
split.name <- rep(split.name, length=length(prefix))

if (!is.null(new.header))
  new.header <- rep(new.header, length=length(prefix))  # new.  28Jan2016
		
if (!is.null(post.process)) 
	post.process <- rep(post.process, length=length(prefix))
if (!is.null(subset.pre)) 
	subset.pre <- rep(subset.pre, length=length(prefix))
if (!is.null(subset.post)) 
	subset.post <- rep(subset.post, length=length(prefix))
if (!is.null(pre.process)) 
	pre.process <- rep(pre.process, length=length(prefix))	
if (make.agg && is.null(fun.aggregate))
	stop("Must include an aggregation function.")	

if (is.null(value.prefix) || is.na(value.prefix)) 
	stop("Must provide `value.prefix' strings to match against file variables.")
if (!is.null(value.process)) {
	if (length(value.process) != length(value.prefix) |
		length(fun.aggregate) != length(value.prefix))
		warning("Length mismatch between `value.prefix' and `value.process' or `fun.aggregate', replicating to make lengths consistent.")
  value.process <- rep(value.process, length=length(value.prefix))		
}
fun.aggregate <- rep(c(fun.aggregate), length=length(value.prefix))		


# No duplicate entries allowed in interval spec
interval <- unique(interval)
		
#require(heR.Misc)
#require(reshape)

time.factor.names <- paste("Time.Factor",interval, unit, sep=".")
time.factor.POSIX.names <- paste("Time.Factor.POSIX",interval, unit, sep=".")
if (!is.null(elapsed.bykey.names)) {
	time.factor.bykey.names <- paste(time.factor.names, "bykey", sep=".")
	time.factor.POSIX.bykey.names <- paste(time.factor.POSIX.names, "bykey", sep=".")	
}
elapsed.name <- paste("Elapsed", unit, sep=".")

#  CHECK FOR COLLISIONS IN BASIC VARIABLE NAMES

#  These are the reserved date/time names in the raw data files or events file 
#			that may not be used for other names.  NK 12-10-2011
raw.date.time.names <- 
	unique(c(date.names, time.names, date.names.events, time.names.events))

taken.names <- c(time.factor.names, time.factor.POSIX.names, datetime.name, elapsed.name,
                 id.names, value.name, value.id.name)
#  Names that can be used as a key variable to merge other vars.
taken.key.names <- c(id.names, value.id.name)

if (value.id.name %in% id.names) 
  stop("The `value.id.name' must be different from any of the `id.names' embedded in filenames.")
  
if (length(taken.names) != unique(length(taken.names)))
	stop("Some time or key names duplicated.  Please check optional variable names.")

if (any(raw.date.time.names %in% taken.names)) 
  stop("The reserved raw date and time names must not be used elsewhere:",
			paste(raw.date.time.names,collapse=" ,"))
  
taken.names <- c(taken.names, raw.date.time.names)


# -----------------------------------------------------------------------
# function to check x for any partial or full matches in y and vice versa
#    returns TRUE if any match found, otherwise FALSE
#  Now written to only find full matches by default.....NK 1-Jan-2010
nameMatch <- function (x, y, partial=FALSE) {
	aMatch <- FALSE
	conflicting <- c()
	if (partial) {
		for (i in 1:length(x)) {
			m <- grepl(x[i], y)
			if (any(m)) {
				aMatch <- TRUE
				conflicting <- c(conflicting, x[i], y[which(m)])
			}
		}
		for (i in 1:length(y)) {
			m <- grepl(y[i], x)
			if (any(m)) {
				aMatch <- TRUE
				conflicting <- c(conflicting, x[which(m)], y[i])
			}
		}
	} else {
		if (any(x %in% y)) {
			aMatch <- TRUE
			conflicting <- x[which(x %in% y)]
		}
	}
	list(aMatch, paste(conflicting,collapse=", "))
}
# ---------------------------------------------------------------------------


if (includes) 
  #if (any(include.id.names %in% taken.names) || 
  if (nameMatch(include.id.names, taken.names)[[1]]) {
    stop("Duplicate partial or full name(s) used for included names, please check include id names, value name, and id names. Duplicated name(s):  ",nameMatch(include.id.names, taken.names)[[2]]) 
  } else {
    taken.names <- c(taken.names, include.id.names)
    taken.key.names <- c(include.id.names, taken.key.names)
  }

for (i in 1:length(split.name))
    if (!is.na(split.name[i]))
		if (split.name[i] %in% taken.names) {
			stop("Duplicate name(s) used, please check split.name, value.name, and id.names. Duplicated name(s):  ",taken.names[which(taken.names %in% split.name[i])]) 
		} else {
			taken.names <- c(taken.names, split.name[i])
			taken.key.names <- c(split.name[i], taken.key.names)
		}

if (!is.null(computed.key.names)) {
  if (nameMatch(computed.key.names, taken.names)[[1]]) {
    stop("Duplicate partial or full name(s) used for `computed.key.names', please check include id names, value name, and id names. Duplicated name(s):  ",nameMatch(computed.key.names, taken.names)[[2]]) 
  } else {
    taken.names <- c(taken.names, computed.key.names)
    taken.key.names <- c(computed.key.names, taken.key.names)
  }
}

if (!is.null(computed.response.names)) {
  if (nameMatch(computed.response.names, taken.names)[[1]]) {  
    stop("Duplicate partial or full name(s) used for `computed.response.names', please check include id names, value name, and id names. Duplicated name(s):  ",nameMatch(computed.response.names, taken.names)[[2]]) 
  } else {
    taken.names <- c(taken.names, computed.response.names)
  }
}


if (debug)  {
	cat("\nInitial taken names:\n")
	print(taken.names)
}



#  colors used in EVENTS plots
#cols <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
#          "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

#  col argument is by default this:
# c("blue","red","black","green","orange","magenta","cyan")




# -------------------------------------------------

#  E V E N T   F I L E 

# -------------------------------------------------

if (merge.events) {

    # Make list objects containing all data for the found event files... will be merged individually....
	event.key.list <- list()
	event.value.names.list <- list()
	no.event.key <- c()
	edata.list <- list()
	edataI.list <- list()
	
	if (!is.list(event.key))
		event.key.list[[1]] <- event.key
	else event.key.list <- event.key

	# Load event file(s) for merging if available....
	ep <- paste("^(",events.prefix,").*[.](",events.extension,")$",sep="")
	efiles <- dir(pattern=ep)
	
	if (debug) print(efiles)

	if (length(efiles) == 0)
		stop("No event file found with prefix ",events.prefix," and extension ",events.extension,"\n")
		
	#if (length(efiles) > 1 )
	#	stop("Multiple event files found, please consolidate to a single file.\n")
     
	if (length(efiles) != length(event.key.list)) 
		warning("Length of `event.key' list does not match number of found event files. Assuming first value.\n")

    

	for (i in 1:length(efiles)) {

		   cat("\nProcessing Event File #",i,": ",efiles[i],"...\n", sep="")
		   
		   if (i > length(event.key.list))
				event.key.list[[i]] <- event.key.list[[1]]		   

		   #alledata <- data.frame()

		   #for (i in efiles) {
		   # always do CSV for event file....
		#    if (csv)   # comma separated file format?
			  edata <- read.csv(file=efiles[i], comment.char=comment.char.event)
		#    else       # else assume space delimited with a header
		#      edata <- read.table(file=efiles[1],header=TRUE, comment.char=comment.char)

			if (!all(event.key.list[[i]] %in% c(taken.key.names)))
				stop("The event key(s), currently ",event.key.list[[i]], ", must be one of the available factors name, ",paste(taken.key.names,collapse=" "))


			no.event.key[i] <- FALSE
			if (!all(event.key.list[[i]] %in% names(edata))) {
			  warning("The event key variable(s) ",event.key.list[[i]], ", is(are) not in the event file: ",paste(names(edata), collapse=","),".  Events will be matched to ALL records.")
			  no.event.key[i] <- TRUE
			} else if (!all(event.key.list[[i]] %in% c(taken.key.names))) {
			  stop("The event key(s), currently ",event.key.list[[i]], ", must be one of the available factor names, ",paste(taken.key.names,collapse=" "))
			} else {
				cat("\nUsing event.key: ", event.key.list[[i]],"\n\n")
				# Remove rows with an NA event key
				goodidx <- apply(edata[event.key.list[[i]]], 1, function(x) !any(is.na(x)))
				edata <- edata[goodidx, ]
				if (NROW(edata) < 1)
					stop("Event data empty after removing rows with one or more NA index values.")
			}

			print(head(edata))

			# Get Date/Time indices, matching 1st occurrence on specified prefix
			#   Now matches multiple possibilities, still choose only first match
			#date.index <- grep(paste("^(",date.prefix,")",sep=""), names(edata))[1]
			#date.index <- grep(paste("^(",date.prefix.events,")",sep="",collapse="|"), names(edata))[1]
			date.index <- grep(paste(date.names.events,sep="",collapse="|"), names(edata))[1]
			if (datetime.events || is.null(time.names.events))
			  time.index <- NULL
			else
			  #time.index <- grep(paste("^(",time.prefix,")",sep=""), names(edata))[1]
			  #time.index <- grep(paste("^(",time.prefix.events,")",sep="",collapse="|"), names(edata))[1]
			  time.index <- grep(paste(time.names.events, sep="",collapse="|"), names(edata))[1]

			if (is.na(date.index))
			  stop("Date columns not matched.")
			if (!is.null(time.index) && is.na(time.index))
			  stop("Date columns not matched.")
			  
			# Get the event index or indices, matching MULTIPLE occurrences
			#   on specified prefix
			event.value.index <- grep(paste("^(",event.value.prefix,")",sep="",collapse="|"), names(edata))
			event.value.names <- grep(paste("^(",event.value.prefix,")",sep="",collapse="|"), names(edata), value=TRUE)

			#  Store these for later use...
			event.value.names.list[[i]] <- event.value.names
			
			cat("\nEvent value names found: ", event.value.names,"\n")
			
			if (length(event.value.names) == 0)
			  stop("No event names found.")
			  
				
			# Check for duplicate names
			if (nameMatch(event.value.names, taken.names)[[1]])	
			  stop("Duplicate name(s) used for event names, please check event and id names. Duplication name:  ",nameMatch(event.value.names, taken.names)[[2]])
			
			taken.names <- c(taken.names, event.value.names)
			taken.key.names <- c(taken.key.names, event.value.names)

			# Make events into factors
			for (j in event.value.names) 
			   edata[[j]] <- factor(edata[[j]])

			# Create standard date format
			if (datetime.events)
			  dateraw <- edata[[date.index]]
			else {
				#print(head(edata[[date.index]]))
				#print(head(edata[[time.index]]))	  
				dateraw <- paste(edata[[date.index]], edata[[time.index]])
			}
			  
			#print(head(dateraw))
			  
			edata[datetime.name] <- as.POSIXct(strptime(dateraw, 
										format=events.format, tz=tz))

			if (any(is.na(edata[[datetime.name]])))
			  stop("Date/Time not processed correctly.  Please check the date-time format string.")
			  

			# Include the event.key index variable for different response values
			#   if it exists in the event file (default set to value.id.name)
			if (event.key %in% names(edata))
			  newnames <- c(event.key, datetime.name, event.value.names)
			else
			  newnames <- c(datetime.name, event.value.names)
			
			edata <- edata[newnames]
			#   newdata <- data[newnames]
			#    alledata <- rbind(alledata, newdata)
		   
			#}


			#  Sort by time -- and event.key (default "value.id.name") if it exists
			if (all(event.key %in% names(edata))) 
				o <- do.call(order, edata[c(datetime.name, event.key)])
			else
				o <- order(edata[[datetime.name]])

			edata <- edata[o,]

			#cat("\nProcessed Events File...\n")
			cat("\nProcessed Event File #",i,": ",efiles[i],"...\n", sep="")

			print(head(edata))
			
			edata.list[[i]] <- edata

			if (event.numbers) {
				edataI <- edata
				for (var in event.value.names)
					edataI[[var]] <- paste(edataI[[var]], 1:length(edataI), sep="-")
				cat("\nCreated Events Data with Added Sequential Integers...\n")		
				print(head(edataI))	
				edataI.list[[i]] <- edataI	
			}	
			
							
	}
	
	if (is.null(plot.event.names))
		plot.event.names <- unlist(event.value.names.list)
		
	#print(plot.event.names)
		
	if (!all(plot.event.names %in% unlist(event.value.names.list)))
		stop("'plot.event.names' must be a subset of the event names: ",paste(event.value.names,collapse="|"))

	cat("\nWill use event names for plots: ",plot.event.names,"\n\n")
	
}




# -------------------------------------------------

#  L O O K - U P   F I L E (S)

# -------------------------------------------------

if (merge.lookup) {

    if (!all(lookup.key %in% c(taken.key.names)))
      stop("The lookup key(s), currently ",lookup.key, ", must be one of the available factor names, ",paste(taken.key.names, collapse=" "))

    # Load lookup file(s) for merging if available....
    
    lp <- paste("^(",lookup.prefix,").*[.](",lookup.extension,")$",sep="")
    lfiles <- dir(pattern=lp)

    if (length(lfiles) == 0)
      stop("No look-up file(s) found with prefix ",lookup.prefix," and extension ",lookup.extension,"\n")

	lookup.value.names <- c()
	new.ldata <- data.frame()
	  
	for (i in 1:length(lfiles)) {
	  
		#if (length(lfiles) > 1 )
		#  stop("Multiple look-up files found, please consolidate to a single file.\n")

		cat("\nProcessing Look-up File ",lfiles[i],"...\n", sep="")

		# Always to CSV format for lookup table
	#    if (csv)   # comma separated file format?
		ldata <- read.csv(file=lfiles[i], comment.char=comment.char.lookup)
	#    else       # else assume space delimited with a header
	#      ldata <- read.table(file=lfiles[1],header=TRUE, comment.char=comment.char)

		if (!all(lookup.key %in% names(ldata)))
		  stop("The lookup key names(s), currently ",lookup.key, ", must be in the lookup file: ",paste(names(ldata), collapse=" "))
		  
		# Remove rows with an NA event key
		goodidx <- apply(ldata[lookup.key], 1, function(x) !any(is.na(x)))
		ldata <- ldata[goodidx, ]
		if (NROW(ldata) < 1)
			stop("Lookup data empty after removing rows with one or more NA index values.")	  

		print(head(ldata))

		# Get the event index or indices, matching MULTIPLE occurrences
		#   on specified prefix
		lookup.value.index <- grep(paste("^(",lookup.value.prefix,")",sep="",collapse="|"), names(ldata))
		current.lookup.value.names <- grep(paste("^(",lookup.value.prefix,")",sep="",collapse="|"), names(ldata), value=TRUE)

		if (length(current.lookup.value.names) == 0)
		  stop("No look-up names found.")
		
		if (any(current.lookup.value.names %in% lookup.value.names))
			stop("Duplicate lookup names across lookup files.")
		
		lookup.value.names <- unique(c(lookup.value.names, current.lookup.value.names))

		#if (!lookup.key %in% names(ldata))
		#  stop("'lookup.key' (", lookup.key,") not found in look-up file.")

		# Check for duplicate names
		if (nameMatch(taken.names, current.lookup.value.names)[[1]]) 	
		  stop("Duplicate partial or full name(s) used for lookup names, please check look-up, event and file id names. Duplicated name(s):  ",nameMatch(taken.names, lookup.value.names)[[2]])
		  
		taken.names <- c(taken.names, current.lookup.value.names)
		taken.key.names <- c(taken.key.names, current.lookup.value.names)	

		# Make lookup items into factors
		for (j in current.lookup.value.names) ldata[[j]] <- factor(ldata[[j]])

		# Select only matched id names and value.id.name 
		newnames <- c(lookup.key, current.lookup.value.names)
		ldata <- ldata[newnames]

		#o <- order(ldata[[lookup.key]])
		o <- do.call(order, ldata[lookup.key])	
		ldata <- ldata[o,]

		if (i == 1)
			new.ldata <- ldata
		else
			new.ldata <- merge(new.ldata, ldata, all=TRUE)
		
	}

	ldata <- new.ldata

	cat("\nProcessed all Look-up Files...\n")

	print(head(ldata))
	
}

# ----------------------------------------------------------


cat("\nTaken variables names: ", taken.names)
cat("\nTaken key variable names: ", taken.key.names)



# -------------------------------------

#    D A T A   F I L E S      

# -------------------------------------

require(plyr)

cat("\n\nProcessing '",name,"' data files....\n", sep="")

# Initialize combined data frame
alldata <- data.frame()

all.value.names <- c()

# Get files starting with "prefix" and ending with "extension"
#p <- paste("^(",prefix,").*[.](",extension,")$",sep="")
# Allow prefix and extension to be vectors with any possible
#    values, we will combine them and seach for the combinations....

#  Iterate over all elements of "prefix" for different types of data file....
# TODO:

if (is.na(start) || is.null(start))
   stop("`start' should be one of 'auto-byfile', 'auto-first', 'custom', or a date-time string.")
else
   start.type <- start

if (start == "auto-byfile" || start == "custom")
	warning("`start' time may vary for responses in different files, resulting in potentially unexpected results for time-aggregated WIDE files (time factor and POSIX time factors won't match).")
   
first <- TRUE   
   
for (m in 1:length(prefix)) {

	cat("\n\n=============================================================\n")
	cat("=============================================================\n")	
	cat("Processing files with prefix.extension = '",prefix[m],"[...].",extension[m],"'","...\n", sep="")
	cat("=============================================================\n")
	cat("=============================================================\n\n")	

	p <- paste("^(",prefix[m],").*[.](",extension[m],")$",sep="",collapse="|")

	files <- dir(pattern=p)

	#  Now just do warning.. if "files" vector is empty no files will be processed
	if (length(files) == 0)
	  warning("No data files found with prefix ",prefix[m]," and extension ",extension[m],"\n")
	  

	for (i in files) {
	
		cat("\n=============================================================\n")
		cat("Processing file '",i,"'","...\n", sep="")
		cat("============================================================\n\n")

		if (debug) {
			cat("\nFile processing specifications: \n\n")
			cat("Skip: ",skip[m], "\n")
			cat("Comment Char: ",comment.char[m], "\n")			
			cat("Column Classes: ",colClasses[[m]], "\n")			
			cat("Time Format: ",format[m], "\n")						
			cat("Datetime: ",datetime[m], "\n")
			cat("CSV: ",csv[m], "\n")			
			cat("Extension: ",extension[m], "\n\n")
		}
		
		# NEW:  Add a new header if specified --> AFTER file is read in...  28-Jan-2016

		rdata <- data.frame()
		
		if (csv[m]) {  # comma separated file format?
		  if (is.null(new.header) | is.na(new.header[m])) {
		    if (debug) cat ("\nCSV file: Use existing header.\n")
		    rdata <- read.csv(file=i, skip=skip[m], comment.char=comment.char[m], colClasses=colClasses[[m]])
		  } else {
		    if (debug) cat ("CSV file: assigning new header: ", new.header[m],"\n")
		    require(stringr)
	      rdata <- read.csv(file=i, header=FALSE, skip=skip[m], comment.char=comment.char[m], colClasses=colClasses[[m]])
		    new.names <- str_split(new.header[m], pattern=",")[[1]]
		    if (NCOL(rdata) == length(new.names)) {
		      names(rdata) <- new.names
		    } else {
		      warning("Header length does not match number of columns.")
		    }
		  }
		} else {       # else assume space delimited with a header
		  if (is.null(new.header) | is.na(new.header[m])) {
		    if (debug) cat ("\nDAT file: Use existing header.\n")		    
		    rdata <- read.table(file=i,header=TRUE,skip=skip[m],comment.char=comment.char[m], colClasses=colClasses[[m]])
		  } else {
		    if (debug) cat ("DAT file: assigning new header: ", new.header[m],"\n")		      		    
		    require(stringr)
		    rdata <- read.table(file=i,header=FALSE,skip=skip[m],comment.char=comment.char[m], colClasses=colClasses[[m]])		  
		    new.names <- str_split(new.header[m], pattern=",")[[1]]
		    if (NCOL(rdata) == length(new.names)) {		    
		      names(rdata) <- new.names
		    } else {
		      warning("Header length does not match number of columns.")
		    }
		  }
		}
		
		if (NROW(rdata) == 0) {
		   stop("Error reading in data file '",i,"' for prefix: ", prefix[m], "\n") 
		} else {
		  cat("Raw data successfully read in:\n")
		  print(head(rdata))
		}

		# Get number of original columns
		n <- NCOL(rdata)

		#  Now apply the new PRE-processing commands on the raw file (specific to
		#  each prefix).   This allows us to modify or make new response variables
		#   that will be matched by the prefix argument....  NK 1-Jan-2010
		#  Uses the "within" command to modify the current raw `data' data frame.....
		if (!is.null(pre.process)) {
			cmds <- pre.process[m]
			cat("\nEvaluating pre-processing command(s) in raw data frame: ",cmds,"\n")
			rdata <- within(rdata, eval(parse(text=cmds)))
		}
		
		#  NEW:  Do subset after pre-process so we can subset on any new
		#			variables.... NK 27-Jul-2010
		#  Apply the 'subset' argument if given to pre-process data, removing unwanted rows
		#   If an element for this prefix is "NA", we skip any preprocessing for that prefix
		#   Allows for the FILTERING out of unwanted records from the raw file.....
		if (!is.null(subset.pre))
			if (!is.na(subset.pre[[m]])) {
				cat("\nApplying PRE subset ", subset.pre[[m]]," for data file ", i,"\n")
				#sub <- eval(substitute(subset.pre[[m]]), data)
				rdata <- rdata[eval(parse(text=subset.pre[[m]]), rdata),]
				if (NROW(rdata) < 1)
					stop("`subset.pre' resulted in no valid rows for data file ",i,"\n")
			}
		
		
		#  Split file by 'split.name' if a grouping variable name is specified, per file id prefix
		if (!is.na(split.name[m])) {
			if (!split.name[m] %in% names(rdata))
				stop("`split.name' must be a column name in the data file used for splitting: ", paste(names(rdata),sep=","))
			grps <- unique(as.character(rdata[[split.name[m]]]))					
			rdata <- split(rdata, f=data[[split.name[m]]])
		} else {
			rdata <- list(alldata=rdata)
			grps <- "alldata"		  
		}
        
		#  loop over all distinct groups of responses in the data file
		for (grp in grps) {
		
			cat("\n****  Processing file group '",grp,"'","...  ****\n\n", sep="")
			
		
			# Get Date/Time indices, matching 1st occurrence on specified prefix
			#    Now match multiple possibilities and take first match....
			#  now take EXACT MATCH of names   6/26/2013
			#date.index <- grep(paste("^(",date.prefix,")",sep="",collapse="|"), names(data[[grp]]))[1]
			date.index <- grep(paste("^",date.names,"$", sep="",collapse="|"), names(rdata[[grp]]))[1]			
			#date.index <- grep(paste("^(",date.prefix,")",sep=""), names(data))[1]
			#date.index <- grep(paste("^(",date.prefix,")",sep=""), names(data))
			if (datetime[m] || is.null(time.names))
				time.index <- NULL
			else
			  #time.index <- grep(paste("^(",time.prefix,")",sep=""), names(data))[1]
				#time.index <- grep(paste("^(",time.prefix,")",sep="",collapse="|"), names(data[[grp]]))[1]
				time.index <- grep(paste("^",time.names,"$", sep="",collapse="|"), names(rdata[[grp]]))[1]
			
			if (is.na(date.index))
			  stop("Date columns not matched.")
			if (!is.null(time.index) && is.na(time.index))
			  stop("Date columns not matched.")
			  
 			if (debug) {
				cat("\nMatched date time columns.\n")			  
				cat("Date variable:", names(rdata[[grp]])[date.index])
				cat("Time variable:", names(rdata[[grp]])[time.index])
			}

			# Get the response index or indices, matching MULTIPLE occurrences
			#   on specified prefix
			value.index <- grep(paste("^(",value.prefix,")",sep="",collapse="|"), names(rdata[[grp]]))
			value.names <- grep(paste("^(",value.prefix,")",sep="",collapse="|"), names(rdata[[grp]]), value=TRUE)

			if (length(value.names) < 1)
			  stop(paste("\nNo response values found with prefix: ",value.prefix))
			
			#  keep track of value names in all the files
			all.value.names <- unique(c(all.value.names, value.names))

			# Check for duplicate value names
			#if (any(value.names %in% taken.names))
			#  We don't have to do this for the long format data, but I guess when
			#   put in WIDE format we could have some conflict.....  NK 5-Dec-09
			if (nameMatch(value.names, taken.names)[[1]])			
			  stop("Duplicate full name(s) used, please check event, id, include, and value names.  Duplicated names:  ",nameMatch(value.names, taken.names)[[2]])
		  
			# Rename concentration variable.    NOT YET see below...
			# names(data)[value.field] <- value.name

			# Create id variables - take all fields embedded in the file separated by
			#   the splitchar arg and make a separate index variable for each one
			#   using names given in id.names var.
			substring <- paste(".",extension[m], sep="")
			#ids <- unlist(strsplit(unlist(strsplit(i,"\\."))[1], splitchar))
			ids <- unlist(strsplit(unlist(sub(substring,"",i)), splitchar))
			cat("File Id's parsed: ",ids,"\n")
			if (length(id.names) == length(ids)) {
			  names(ids) <- id.names		  
			  for (j in 1:length(ids))
				 rdata[[grp]][[id.names[j]]] = ids[j]
			} else 
			  stop("Mismatch between number of ID names specified and embedded fields in an input filename.")


			# Check that include variable names are present in data names
			if (includes) 
			  if (!all(include.id.names %in% names(rdata[[grp]])))
				stop("Include variable names not found.  Missing: ",
					paste(taken.names[which(!taken.names %in% names(rdata))], collapse=" | "))

			# Create standard date format
			if (datetime[m])
			  dateraw <- rdata[[grp]][[date.index]]
			else
			  dateraw <- paste(rdata[[grp]][[date.index]], rdata[[grp]][[time.index]])

			rdata[[grp]][datetime.name] <- as.POSIXct(strptime(dateraw, format=format[m], tz=tz))
			
			if (debug) {
				cat("\nFirst instance of date-time variable:\n")
				print(rdata[[grp]][[datetime.name]][1])
				print(attributes(rdata[[grp]][[datetime.name]][1]))
			}
			
			if (any(is.na(rdata[[grp]][[datetime.name]]))) {
				cat("NA values introduced in datetime variable at input record(s):\n")
				print(rdata[[grp]][which(is.na(rdata[[grp]][[datetime.name]])),])
				stop("Date/Time not processed correctly.  Please check the date-time format string.")
			}
			  
 			if (debug) cat("\nAssigned date time variable.\n")
			  
			if (any(diff(rdata[[grp]][[datetime.name]]) <= 0))
				cat("\nOne or more times in data file not increasing.\n")
				
			# Apply any datetime offset
			if (!datetime.offset.sec[m] == 0) { 
				rdata[[grp]][datetime.name] <- data[[grp]][[datetime.name]] + datetime.offset.sec[m]
				cat("\nApplied non-zero date-time offset of ",datetime.offset.sec[m]," seconds.\n")
			}

			#  Now do time factors later to allow for customizing
			#   auto modes wrt merged variables...
				
			#  Create column for value.id.name
			rdata[[grp]][value.id.name] <- NA

			if (debug) {
				cat("\nRaw data before layering responses (response pre-set to NA) ....\n")
				print(head(rdata[[grp]]))
			}
			
			# ITERATE over every response variable, adding new rows for
			#   each response (with duplicate embedded file indices in id.names 
			#    and include.id.names).
			for (j in value.names) {

				  #  Convert to numeric values, non-numeric become NA's
				  if (convert2numeric)
					rdata[[grp]][[j]] <- as.numeric(as.character(rdata[[grp]][[j]]))
				  
  				  #  insure the response is numeric  ... don't need this anymore
				  # data[[grp]][[j]] <- as.numeric(data[[grp]][[j]])
			
				  #   Responses must be wholly numeric -- are any values non-decimal numbers?
				  #nn <- !grepl("^[+-]?\d+(\.\d+)?$", as.character(data[[grp]][[j]]), perl=TRUE)
				  if (is.factor(rdata[[grp]][[j]]) || !is.numeric(rdata[[grp]][[j]])) {
					if (debug) print(rdata[[grp]][[j]])
					#stop("Possible non-numeric response value(s) ",data[[grp]][which(nn),j]," for response ",j," in file ",i)
					stop("Possible non-numeric response value(s) for response '",j,"' in file '",i,"'")					
				  }
			
				  current.value.name <- j
				  
				  
				  # Automatically merge the split.name value with the response name if 
				  #     split.name is non-null
				  if (!is.na(split.name[m]))
						current.value.name <- paste(current.value.name, grp, sep="-")
			
				  newnames <- c(datetime.name, id.names)
				  if (includes) newnames <- c(newnames, include.id.names)
				  if (!is.na(split.name[m])) newnames <- c(newnames, split.name[m])
				  newnames <- c(newnames, value.id.name, j)

				  #print(newnames)
				  newdata <- rdata[[grp]][newnames]
				  names(newdata)[NCOL(newdata)] <- value.name
				  
				  #  Assign raw value name, we assign the `current.value.name' later
				  #    which may have split names, id names, etc merged in..
				  #  So with the raw response available, we can use it to merge
				  #      events/lookup variables....   8-Feb-2010
				  #   --> we can also use raw response in value-processing....
   				  newdata[[value.id.name]] <- j
				  
				  #  Before any user-defined processing, we add declared computed names
				  #    with NA placeholders to the newdata data frame
				  #   --> Not really necessary, since the `within' command used to
				  #   evaluate the processing commands will add any variables that
				  #    are used... But to use new variables in name merging, plots,
				  #    agg, etc, we need to declare them and make sure they exist....   
				  #						NK 1-Jan-2010
				  #  Actually, _is_ necessary if we have multiple prefixes and processing
				  #    commands.  We need to make sure they use consistent variables or
				  #     add NA if one prefix uses a variables and one doesn't....nk 2-Apr-2010
				  #  Actually, we can use the rbind.fill command to avoid having to pre set to NA
				  #    still need to declare new key names that are used in aggregating but 
				  #    not necessary to declare new response names....
  				  if (!is.null(computed.key.names)) {
				    for (var in computed.key.names)
						newdata[[var]] <- NA
					newnames <- c(newnames, computed.key.names)
				  }
  				  if (!is.null(computed.response.names)) {
				    for (var in computed.response.names)
						newdata[[var]] <- NA
					newnames <- c(newnames, computed.response.names)
				  }
				  
				  
				  # NEW:  Merging of events now comes before lookup -- so we may key off of
				  #    time-based merged variables in any subsequent lookup merge.
				  #				7-Feb-2010
				  
				  
				  #  EVENTS TO MERGE? -- now key off of event.key, can be custom key variable
				  #                Do we force it to match the key?   seems so now...
				  #  OK.  Yes we require an event.key to merge anything...
				  #   NO.   We set a flag if event data have no event key, and then we add the
				  #    event data to every record regardless of the value of event.key in the data file...
				  if (merge.events) {
				  
					#  New:  8-March-2013.   We iterate over each event set (file), applying individually.
					#		event.key is a list with event sets corresponding to each individual file.
					for (k in 1:length(edata.list)) {
					
						edata <- edata.list[[k]]
						if (event.numbers) edataI <- edataI.list[[k]]						
						event.value.names <- event.value.names.list[[k]]
						event.key <- event.key.list[[k]]
						
						cat("\nMerging event file #", k,".\n")
					
						#  NEW 3-Feb-2010.  Event merge rewritten to be more efficient and
						#   allow for  multiple event key variables.  Uses new 
						#    match.time.segments that gives similar results to the regular
						# 		match function.   Don't use event.numbers now.   NK
						
						#  If no event.key var in the event data, just merge into all records
						if (no.event.key[k]) {
							#  Get indices of matches in the event data...
							#    If multiple time matches, then the last one is used...
							cat("No key available for these events.\n\n")
							
							ematched <- match.time.segments(newdata[[datetime.name]],
															edata[[datetime.name]], debug=debug)
							newdata <- cbind(newdata, edata[ematched, ][event.value.names])
							newnames <- c(newnames, event.value.names)

							#   Option to create duplicate event variables with
							#		added sequential numbers to each event value occurrence
							#			NK  13-Dec-2011		
							#  Create alternate edataI above when events file is parsed.
							#			no need to redo it every iteration...
							if (event.numbers) {
								mergedata <- edataI[ematched, ][event.value.names]
								names(mergedata) <- paste(names(mergedata),"I",sep=".")
								newdata <- cbind(newdata, mergedata)
								newnames <- c(newnames, names(mergedata))
							}

							# otherwise we match events based on the event.key variable....
						#  Approach:  Match the event key variables, then match
						#          times within the key variable match.
						} else {
							event.key.values.e <- do.call(function(...) paste(..., sep="-"), edata[event.key])					
							event.key.values <- do.call(function(...) paste(..., sep="-"), newdata[event.key])
							if (debug) {
								cat("\nEvent key variables is(are):",event.key)
								cat("\nLooking for these merge key values:",unique(event.key.values.e))
								cat("\nMerge values found in data:",unique(event.key.values),"\n")						
							}
							
							#  Make check of whether anything will be matched, i.e., 
							#		some of the event file value will be matched by at
							#		least 1 of the values in the data file.  NK 3/3/2012
							if (!any(event.key.values.e %in% event.key.values))
								warning(paste("No event key value matches for file '",i,"'and value name '",j,"'."))
													
							for (l in unique(event.key.values)) {
								#if (l %in% event.key.values.e) {						
								mergedata <- edata[event.key.values.e == l,]							
								if (debug) {
								   cat("\nCheck times for merge:\n")
								   print(head(newdata[[datetime.name]]))
								   cat("\nCheck events to be merged:\n")
								   print(head(mergedata))
								   cat("\nCheck length of merge times:\n")
								   print(head(mergedata[[datetime.name]]))
								}					
								if (NROW(mergedata) > 0 &&
									any(diff(mergedata[[datetime.name]]) <= 0))
										stop("Duplicate or non-increasing times in the events file. Check event file and `event.key': ",event.key)				
								#  Rewrote match.time.segments to return NA,
								#    if the times or segment.limits arguments are NA.... Done.
								#  If segments is empty, then return
								#   NA vector as long as the times argument....Done.
								ematched <- match.time.segments(newdata[[datetime.name]],
														mergedata[[datetime.name]], debug=debug)
								newdata <- cbind(newdata, mergedata[ematched, ][event.value.names])
								newnames <- c(newnames, event.value.names)
								# do newnames have dupliates?  do we use this var anywhere below?  13-Dec-2011
								
								#  Option for new variables with added sequential numbers to
								#		event values...here we determine sequence for each
								#		key variable combination....see above for no keys
                # NOTE:  There is an error here when doing event numbers
                #    But no matches exist in the event file...fixed.
								if (event.numbers) {
									edataI <- mergedata
									#mergedata2 <- edataI[event.key.values.e == l,]
                  if (NROW(edataI) > 0)
									  for (var in event.value.names)
										  edataI[[var]] <- paste(edataI[[var]], 1:length(edataI[[var]]), sep="-")								
									ematched2 <- match.time.segments(newdata[[datetime.name]],
															edataI[[datetime.name]])								
									mergedata3 <- edataI[ematched2, ][event.value.names]
									names(mergedata3) <- paste(names(mergedata3),"I",sep=".")
									newdata <- cbind(newdata, mergedata3)
									newnames <- c(newnames, names(mergedata3))
								}
								
								
							}
						}
					
					}
					
					if (debug) {
						cat("Data after events merge:\n")
						print(head(newdata))
					}
				  }				  
				  
				  
				  #  LOOK-UP TABLE VARIABLES TO MERGE?
				  if (merge.lookup) { 
					#  Get the current value(s) of the lookup key variable
					#    for the current response value
					#  NEW. More general design, we use "match" to match combinations
					#   of lookup key variables of data in the lookup file (instead of just
					#   assuming the first data file record represents all possible combos in
					#   the response file).   
					#    !!! -- Only uses the FIRST match in the lookup file
					#      Automatically handles NA's...
					#  [could use merge command?]
					lookup.key.values.l <- do.call(function(...) paste(..., sep="-"), ldata[lookup.key])					
					lookup.key.values <- do.call(function(...) paste(..., sep="-"), newdata[lookup.key])
					if (debug) {
						cat("\nLookup key variables are:",lookup.key)
						cat("\nLooking for merge key variables:",unique(lookup.key.values.l))
						cat("\nMerge variables in data:",unique(lookup.key.values),"\n")						
					}
					lmatched <- match(lookup.key.values, lookup.key.values.l)
					newdata <- cbind(newdata, ldata[lmatched, ][lookup.value.names])
					newnames <- c(newnames, lookup.value.names)
					if (debug) {
					   cat("\nCheck lookup variables merged:\n")
					   print(head(newdata[match(lookup.key.values.l,lookup.key.values), lookup.value.names]))
					   cat("Data after lookup merge:\n")
					   print(head(newdata))
					}						
				  }
				  
				  
				  # Don't we have to apply the newnames here somewhere?????
				  #     Actually it's automatic if we are merging data frames together...

				  # Apply value-specific processing assigned by value.prefix matching...
				  #    We can do any processing we want here, but usually reserverd
				  #    for processing that is specific to a given response value....
				  #    other processing (pre/post) is assigned based on prefix values (file types).
				  #	 Moved this after events/lookup merge so we can refer to these variables.  23-March-2010 NK
				  if (!is.null(value.process)) {
						#  Assign value.processing using prefix match of value.name
						#    in value.prefix
						for (vp in 1:length(value.prefix)) {
							if (!is.na(pmatch(value.prefix[vp], j))) {
								if (debug) cat("\nMatched value prefix",value.prefix[vp],"with value",j,"\n")
								cmds <- value.process[vp]
								#if (debug) cat("\n`value.process' command for value",j,"is",value.process[vp],"\n")								
								if (!is.na(cmds)) {
									cat("\nProcessing",value.prefix[vp],"value(s) with command(s):\n",cmds,"\n")						
									newdata <- within(newdata, eval(parse(text=cmds)))							
								} else {
									cat("\n'value.process' for value.prefix",value.prefix[vp],"is NA. No values changed.\n")
								}
							}
						}
				  }
				  
				  

				  #  Apply any post-processing commands
				  #   NEW:  use `within' commmand to avoid adding newdata$
				  #     and we can 
				  #  Applies to ALL values regardless of prefix value (see value.process above
				  #     for processing specific to matched value prefixes)..
				  #  No... we do this per file prefix.....
				  if (!is.null(post.process)) {
						#  OLD.... Add "newdata$" to every variable to allow evaluation
						#     on the newdata data frame.
						if (debug) {
							cat("All post-processing commands:\n")
							print(post.process)
						}						
						cmds <- post.process[m]
						#for (var in taken.names) {
						#	#cat("Replacing variable: ",var,"\n")
						#	cmds <- gsub(var,paste("newdata$",var,sep=""),
						#					cmds)
						#}
						cat("Evaluating prefix post-processing command(s): ",cmds,"\n")
						#eval(parse(text=cmds))
						newdata <- within(newdata, eval(parse(text=cmds)))
						if (debug) {
							cat("All post-processing commands:\n")
							print(post.process)
							cat("After post-processing command(s): \n")
							print(head(newdata))
						}
				  }
				  
				  
				  # --------------------------------------------------------
				  #  Now create time factor variables...allowing for customization
				  #  with respect to merged/created variables for each response
				  
				  #  Add:  elapsed.name, time.factor.names, time.factor.POSIX.names, 
				  
				  # Assign the starting time for all processed files
				  #if (is.null(start) & first) start <- data[[datetime.name]][1]
				  #  NEW.  by default we assign start.type 'auto.byfile'... 7-Jan-10
				  if (start.type == "auto-byfile") {
						start <- newdata[[datetime.name]][1]
						cat("\n'auto-byfile' `start' starting time: ", as.character(start),"\n")
				  } else if (start.type == "auto-first" && first) {
						start <- newdata[[datetime.name]][1]
						cat("\n'auto-first' `start' starting time: ", as.character(start),"\n")						
				  } else if (start.type == "custom") {
						if (is.null(start.expression))
							stop("`start.expression' must evaluate to an index.")
						else {
							#  Rewritten using 'within' command  NK 7-Jan-10
							startExp <- start.expression
							#for (var in taken.names) {
							#	#cat("Replacing variable: ",var,"\n")
							#	#print(gsub(var, paste("newdata$",var,sep=""),
							#	#			startExp))		
							#	startExp <- gsub(var, paste("newdata$",var,sep=""),
							#				startExp)
							#}
							cat("\nCustom `start' expression: ",startExp,"\n")
							#  Take first index value returned by evalating start.expression:
							tidx <- eval(parse(text=startExp), newdata)[1]
							cat("Custom `start' index: ", tidx,"\n")
							if (!is.na(tidx)) 
								start <- newdata[[datetime.name]][tidx]
							else {
  							    start <- newdata[[datetime.name]][1]				  
								cat("Custom `start' evaluates to NA, keeping 'auto-byfile' start.\n")
							}
							#cat("Custom `start' starting time: ", as.character(start),"\n")
						}
				  } else if (first) {
						start <- as.POSIXct(strptime(start, format=format[m], tz=tz))
						cat("\nFixed `start' start time: ", as.character(start),"\n")						
				  }
				  
				  start <- as.POSIXct(as.POSIXlt(start, tz=tz))
				  print(start)
				  print(attributes(start))

				  # truncate start off to nearest unit in "trunc.start"
				  if (trunc.start != "none") {
						start <- trunc.POSIXt(start, units=trunc.start)
						start <- as.POSIXct(start, tz=tz)
						cat("\nTruncated start time (",trunc.start,") : ",as.character(start),"\n")
						print(start)
						print(attributes(start))
				  }
					
				  #print(start)
				  if (is.na(start))
					  stop("Error processing `start'.  Check date-time format string.")
							
				  #  See if times are increasing, remove or apply adjustment if
				  #    requested (using adjust.nonincreasing.secs)
				  tincr <- diff(newdata[[datetime.name]]) <= 0
				  if (any(tincr) && (remove.nonincreasing || relax.increasing || adjust.nonincreasing.times || debug)) {
					  cat("\nChecking where datetime is not increasing (1st six listed)....\n")
					  print(head(newdata[which(tincr),]))
					  #  first see if we should remove them, then see if we should adjust the times instead.  6/26/2013...
					  #		If neither, then we let the nonincreasing times pass through the time.factor function, see below
					  if (remove.nonincreasing) {
						cat("\nRemoving records with non-increasing time values.\n")
						newdata <- newdata[-which(tincr),]
					  } else if (adjust.nonincreasing.times) {
						cat("\nApplying time adjustment of multiples of",adjust.nonincreasing.secs,"secs to non-increasing times.\n")
						if (adjust.nonincreasing.secs < 0.000001)
							warning("`adjust.nonincreasing.secs' should be 1 microsecond (the precision of `strftime').")
						#  Subtract large to small multiples of adjust.nonincreasing.secs
						newdata[which(tincr),datetime.name] <- 
							newdata[which(tincr),datetime.name] - 
								rev(seq(from=adjust.nonincreasing.secs, 
										by=adjust.nonincreasing.secs, 
										length.out=length(which(tincr))))
					  }
				  }
				  
				  #  Option for having an extra elapsed.time.2 variable that restarts the
				  #    elapsed time at beginning of every time segment defined by selected
				  #	   key variables (events, lookup, etc.).
				  #   1.   Get unique starting times for selected key variables
				  #	  2.   Loop through all the starting times, assing new elapsed time each time
				  #   3.   Also have extra Time.Factor and Time.POSIX.Factor variables that
				  #	  		use the new elapsed.time.2 variable
		          #  Not done yet.....
				  
				  # Create elapsed minutes variable
				  newdata[[elapsed.name]] <-  as.numeric(difftime(newdata[[datetime.name]],
													 start, unit=unit, tz=tz))

													 
				  #   Make a new elapsed variable that resets to "0" at start of each
				  #			key variable juncture....
				  if (!is.null(elapsed.bykey.names))
					if (!all(elapsed.bykey.names %in% taken.key.names)) {
						stop("Please check `elapsed.bykey.names' argument; some specified key names not available.") 
					} else {
						elapsed.name.2 <- paste(elapsed.name, "bykey", sep=".")
						newdata[[elapsed.name.2]] <- NA								
						keyTimes <- newdata[[datetime.name]][unique(sort(c(unlist(apply(newdata[elapsed.bykey.names], 2, collapse)),NROW(newdata))))]
	
						if (debug) {
							cat("Event segments where elapsed time is to be reset:\n")
							print(keyTimes)
						}
						for (stimes in 2:length(keyTimes)) {
							idx <- newdata[[datetime.name]] >= keyTimes[stimes-1] &
										newdata[[datetime.name]] < keyTimes[stimes]
							if (debug) {
								cat("Index for segment starting:",keyTimes[stimes],"\n")
								print(length(idx))
								print(head(idx))
								cat("Data where elapsed time reset:\n")
								print(head(newdata[idx,]))
							}
							start.bykey <- newdata[[datetime.name]][idx][1]
							# truncate start.bykey to nearest unit in "trunc.bykey.start"
							if (trunc.bykey.start != "none") {
								start.bykey <- trunc.POSIXt(start.bykey, units=trunc.bykey.start)
								start.bykey <- as.POSIXct(start.bykey, tz=tz)
								#print(attributes(keyTimes))
							}
							newdata[[elapsed.name.2]][idx] <- as.numeric(difftime(newdata[[datetime.name]][idx],
									start.bykey, unit=unit, tz=tz))
							if (debug) {
								cat("Elapsed bykey:\n")
								print(head(newdata[[elapsed.name.2]][idx]))
							}
							for (k in 1:length(time.factor.bykey.names)) {
								#keyFactorName <- paste(time.factor.names[k],"bykey",sep=".")
								#  Make a character, can always convert to factor later, 
								#	bc assigning a factor to non-factor data.frame var
								#	removes the factor levels and just assigns (arbitrary) numeric values
								newdata[[time.factor.bykey.names[k]]][idx] <-
									 as.character(time.factor.2(newdata[[elapsed.name.2]][idx],
										interval=interval[k], origin=0,
										 integer.levels=FALSE, 
										relax.increasing=relax.increasing))
								if (debug) {
									cat("Time Factor for ",time.factor.names[k],"\n")
									print(head(newdata[[time.factor.bykey.names[k]]][idx]))
								}
								#keyFactorName <- paste(time.factor.POSIX.names[k],"bykey",sep=".")
								newdata[[time.factor.POSIX.bykey.names[k]]][idx] <-
									as.character(time.factor.POSIXt(newdata[[datetime.name]][idx],
										interval=interval[k], unit=unit,
										origin=start.bykey,
										integer.levels=FALSE,
										relax.increasing=relax.increasing,
										tz=tz))

							}

						}

					}
				  

				  if (debug & !is.null(elapsed.bykey.names)) { 
					cat("\nData with byKey elapsed vars:\n")				  
					print(head(newdata))
				  }
				  
				  if (debug) {
					cat("\nAssigned elapsed time variable:\n")
					print(newdata[[elapsed.name]][1])
					print(newdata[[datetime.name]][1])
					print(attributes(newdata[[datetime.name]][1]))
				  }

				  # Create 1 or more time factor variables,
				  #         showing values in each specified interval
				  #   NEW:  now add POSIX time factor showing time at beginning of each interval...
				  #            using the new time.factor.POSIXt function!  NK  3-June-2009
				  for (k in 1:length(time.factor.names)) {
						newdata[[time.factor.names[k]]] <-
							 time.factor.2(newdata[[elapsed.name]],
								interval=interval[k], origin=0,
								 integer.levels=FALSE, 
								relax.increasing=relax.increasing)
						newdata[[time.factor.POSIX.names[k]]] <-
							time.factor.POSIXt(newdata[[datetime.name]],
								interval=interval[k], unit=unit,
								origin=start, integer.levels=FALSE,
								relax.increasing=relax.increasing,
								tz=tz)
				  }
					
					
				  if (debug) cat("\nAssigned time factor variables.\n")
				  	  
					  
				  #   Done creating time variables....
				  # ---------------------------------------
				  
				  
				  #  -- Merge names into response id name ---
				  #  Now down here, we can merge any names we want in taken.key.names
				  #   into the response (not just the id.names as before...)
				  #     [we already merged any split names above...]
				  #  --> Also, we can match merging events/lookup on the raw Response
				  #      value before mangling it up down here.... :)
				  # SOOO, we need to assign raw response above, so we can merge 
				  #      events/lookup against it, but assign the final 
				  #				response down here.
				  #  More thoughts:   Since we allow any key variables to be matched
				  #    event from the `events' merge, the Response variable may end up
				  #   NOT being FIXED to a response in a given file...it's up to the
				  #   user to be aware of and understand this.... 26-Mar-2010
				  if (!is.null(merge.id.names))
					if (!all(merge.id.names %in% taken.key.names)) {
						stop("`merge.id.names' must contain one or more of the taken key names:", paste(taken.key.names, collapse=","))
				    } else {
						current.value.name <- 
							do.call(paste, c(current.value.name, newdata[merge.id.names], list(sep="-")))
						cat("\nNew Response Name(s) for Original Response,",j," : ",unique(current.value.name),"\n")
					}

				  newdata[[value.id.name]] <- current.value.name
				  
				  
				  #    Before final assembly, we apply a post-subset per file prefix
				  #		to allow for selecting only certain values... NK25June2013
				  #		put *after* time variables so we can key on those if desired...
				  if (!is.null(subset.post))
					if (!is.na(subset.post[[m]])) {
					cat("\nApplying POST subset ", subset.post[[m]]," for data file ", i," response value",j," \n")
					#sub <- eval(substitute(subset.post[[m]]), data)
					newdata <- newdata[eval(parse(text=subset.post[[m]]), newdata),]
					if (NROW(newdata) < 1)
						stop("`subset.post' resulted in no valid rows for data file ",i," response value ",j," \n")
				  }
				  
				  
  				  cat("\nFinal Processed Data for response '",j,"' :\n",sep="")
				  
				  print(head(newdata))				  
				  

				  # Add current data to master data frame
				  #    Update:  use rbind.fill now to fill in NA's if there are 
				  #       missing column names in one of the data frames, i.e., if
				  #       we compute new variables only for one prefix/response
				  #  		AND forget to declare it......with computed.key.names or
				  # 			computed.response.names....
				  #      			nk Apr-2-2010
				  alldata <- rbind.fill(alldata, newdata)
				  
				  # No longer first file being processed for time information
				  first <- FALSE
				  
			} # values loop
			
		}  # grps loop
		
	} # files loop


	#  MULTIPLY VALUES BY A NUMERIC FACTOR IF value.factor IS GIVEN....	
	
	#  TODO:   Tie value.factor to each type of value name, by way of value.prefix
	#       Really should make this a general numeric transformation keyed on 
	#      value.prefix...  Done.   see value.process above....


	
}



# --------------------------------------------

#   T R A N S F O R M    D A T A

# ---------------------------------------------


cat("\n=================================================\n")

# TODO:  apply numeric value tranformation.......  Iterate over all
#         response values, then apply transformation to each subset based
#         on a value.prefix match....  No...we do this above in the main loop.

# Do same for aggregation....

# Create an aggregated version, averaging over time factor  
#  TODO:  Not just average, allow for any aggregation function....min, max, most frequent, etc.

# ---------------------------------------------------------
#   Function to match data frame response values with value.prefix and apply
#      corresponding aggregation functions in fun.aggregate 
#    If wide formatted (i.e., value.id.name in columns), then we merge each
#		successive aggregation, otherwise we just stack them with rbind.
#    x == data frame to process
#    f == formula to apply to each value prefic match
aggByPrefix <- function (x, f, wide=FALSE, add.missing=FALSE) {
	result <- data.frame()
	for (i in 1:length(value.prefix)) {
		#  Fixed to match beginning of string like when we match file vars above.  NK 24-Mar-2010
		idx <- grep(paste("^(",value.prefix[i],")",sep=""), x[[value.id.name]])
		if (length(idx) > 0) {
			fun <- fun.aggregate[[i]]
			if (is.function(fun)) {
				if (debug) {
					cat("\nAggregating values with matched prefix",value.prefix[i],"using:\n")
					print(fun)
					cat("\n...with formula:\n")
					print(f)
				}
				if (debug) {
					cat("\nMelted data to aggregate:\n")
					print(head(x[idx,]))
				}
				# Now option to include structural missings so we have complete set of factor
				#		levels included in reshaping..  24-Mar-2010 
				# !! NOTE !!:  using add.missing=TRUE for dataframes with lots of rows can
				#    require WAY to much memory to perform.  Stick to using it
				#    for WIDE files with a relatively small number of rows....
				#   Actually, don't use it at all!!!!!!
				mdata <- cast(x[idx,], f, fun, add.missing=add.missing)
				if (debug) {
					cat("\nCurrent data to merge:\n")
					print(head(mdata))
					#cat("\nCurrent aggregated result:\n")
					#print(head(result))
				}				
				#  Now we add NA's if the there are no matches of factor vars.. NK 24-Mar-2010				
				#  NEW:  We now use rbind.fill for long data from Hadley's plyr package, which adds
				#    missing columns when binding by rows (inserting NA's).  This problem comes
				#    up with time-in-col reshapings when not all responses have same time factor
				#    levels.  I tried fixing this with the add.missing command in `cast' but this
				#    was adding way to many NA rows and used up memory very quickly...not a good
				#    solution, but rbind.fill is the ticket.... nk 25-Mar-2010
				if (i == 1) 
					result <- mdata
				else if (wide) result <- merge(result, mdata, all=TRUE)
					else result <- rbind.fill(result, mdata)
			} else {
				cat("Missing aggregation function for",value.prefix[i],". No results returned.\n")
			}
		} else {
			if (debug)
				cat("No data for `value.prefix' = ", value.prefix[i], "\n")
		}
	}
	as.data.frame(result)
}
# ----------------------------------------------------------

# ----------------------------------------------------------
#  Function to convert a variable in a data frame to numeric
name2numeric <- function(x, name) {
	x[[name]] <- as.numeric(as.character(x[[name]]))
	x
}


if (make.agg || make.wide || make.stats) {

  require(plyr)
  require(reshape)

  #basenames <- c(id.names, value.id.name)
  #basenames <- taken.key.names   # already has value.id.name + include.id.names in it
  basenames <- c(id.names, value.id.name)
  if (includes) basenames <- c(basenames, include.id.names)
  basenames <- c(basenames, split.name[!is.na(split.name)])
  
  basenames2 <- basenames
  
  #  Aggration only uses official response "Value", so we don't include any
  #   new respones.  But we do want to include new declared key variables.  nk 2-Apr-2010
  #if (!is.null(computed.response.names)) basenames <- c(basenames, computed.response.names)
  if (!is.null(computed.key.names)) basenames <- c(basenames, computed.key.names)  
  
  if (merge.lookup) 
    basenames <- c(basenames, lookup.value.names)
  if (merge.events) 
    basenames <- c(basenames, unlist(event.value.names.list))   # fix to use all events names in agg. NK 26Jan2016
    #basenames <- c(basenames, event.value.names)

  #  names for all aggregated files, we have separate
  #     agg dataframes for each interval....
  aggnames <- c(time.factor.names, time.factor.POSIX.names, datetime.name, basenames)
  
  if (!is.null(elapsed.bykey.names))
	aggnames2 <- c(time.factor.bykey.names,
				   time.factor.POSIX.bykey.names, 
				   basenames)

  aggcustom <- list()    #  Custom reshape aggregation
  
  agglist <- list()		#  'LONG' aggregation by each time factor + other factors
  agglistbykey <- list()		#  'LONG' aggregation by each time factor + other factors - bykey time factors
  aggtimelist <- list()		#  'LONG' aggregation by each time factor only 
  aggeventslist <- list()		#  'LONG' aggregation by each time factor + events  
  
  #widetimelist <- list()     # 'WIDE' raw data by just each time factor
  #wideeventslist <- list()     # 'WIDE' raw data by each time factor  + event factors
  
  aggwidelist <- list()    #  'WIDE' aggregation by each time factor + all other factors
  aggwidetimelist <- list()  #  'WIDE' aggregation just by each time factor
  aggwideeventslist <- list()  #  'WIDE' aggregation by each time factor + event factors
  
  aggwidecolslist <- list()  # 'WIDE' aggregation with each time factor in columns + other factors
  aggwideresponsecolslist <- list()  # 'WIDE' aggregation with just response value + each time factor in columns 
  aggwideeventscolslist <- list()  # 'WIDE' aggregation with response value + events + each time factor in columns   


  #  =========================================================
  #  Create main aggregated file of values averaged over
  #    groups defined by time factor and all id variables.
  #  =========================================================

  aggmelt1 <- melt(alldata, id.var=aggnames, measure.var=value.name)
  
  if (!is.null(elapsed.bykey.names))
	aggmelt2 <- melt(alldata, id.var=aggnames2, measure.var=value.name)
   
  #  Not used anymore
  #aggmelt2 <- melt(alldata, id.var=aggnames, measure.var=datetime.name)
  
  # =============   C U S T O M   ====================
  #  Apply custom (runtime) reshape formula
  if (!is.null(reshape.formula)) {
		for (i in 1:length(reshape.formula)) {
			aggcustom[[paste("aggcustom",i,sep="-")]] <- cast(aggmelt1, reshape.formula[i],
									fun=reshape.function, na.rm=na.rm)
		}
  }
  # ==========================================================
  
  #  !!!!  Need new variable with times of start of time factor intervals.....
  #             use this new var just for aggregated data files.....so we
  #         can easily match up aggregated files from different trips...according to
  #         the rounded intervals starting at an even minute, hour, etc.
  #  Question:  can we round off start to an even 15 min, 10 min time???
  
  # OK. to do this, we need to create a table with interval values from 'start' and
  #    the corresponding POSIX time where each interval starts.   We will then
  #    merge the times into the aggregated data frames.  Do this above....

  #  LONG aggregation by each time factor + other factors
  # .....create "long" data, averaging across each time interval specified...
  if (make.agg) {
		for (i in 1:length(interval)) {
			 formulanames <- c(time.factor.POSIX.names[i],time.factor.names[i],basenames)
			 cat("\nAggregating LONG data in ",interval[i],"-",unit,"intervals...\n")
			 f <- paste(c(paste(formulanames,collapse="+"), "variable"), collapse="~")
			 agglist[[time.factor.names[i]]] <-
					 # cast(aggmelt2, f, fun=function(x) x[1])[time.factor.POSIX.names[i]]
					#cast(aggmelt1, f, fun=mean, na.rm=na.rm)
					aggByPrefix(aggmelt1, f)
			 #agglist[[time.factor.POSIX.names[i]]] <-
			 #         agglist[[time.factor.names[i]]] + 
			 #	   strptime("1970-01-01", "%Y-%m-%d", tz=tz)
			 #  Make time.factor.names[i] numeric
			 agglist[[time.factor.names[i]]] <- 
					name2numeric(agglist[[time.factor.names[i]]],time.factor.names[i])
			 print(head(agglist[[i]]))
	
		}
		
		if (!is.null(elapsed.bykey.names))
			for (i in 1:length(interval)) {
				formulanames <- c(unique(c(elapsed.bykey.names, basenames2)),
								  time.factor.POSIX.bykey.names[i],
								  time.factor.bykey.names[i])
				cat("\nAggregating LONG BYKEY data in ",interval[i],"-",unit,"intervals...\n")
				f <- paste(c(paste(formulanames,collapse="+"), "variable"), collapse="~")
				agglistbykey[[time.factor.names[i]]] <- aggByPrefix(aggmelt2, f)			 
			 }

			 
		for (i in 1:length(interval)) {
			 formulanames <- c(time.factor.POSIX.names[i], time.factor.names[i], value.id.name)
			 cat("\nAggregating time-only LONG data for ",interval[i],"-",unit,"intervals...\n")
			 f <- paste(c(paste(formulanames,collapse="+"), "variable"), collapse="~")
			 aggtimelist[[time.factor.names[i]]] <-
						#cast(aggmelt1, f, fun=mean, na.rm=na.rm)
					aggByPrefix(aggmelt1, f)
			 #  Make time.factor.names[i] numeric
			 aggtimelist[[time.factor.names[i]]] <- 
					name2numeric(aggtimelist[[time.factor.names[i]]],time.factor.names[i])
					
			 print(head(aggtimelist[[i]]))
		}
		
		if (merge.events) {
			for (i in 1:length(interval)) {
				 formulanames <- c(time.factor.POSIX.names[i], time.factor.names[i], event.value.names, value.id.name)
				 cat("\nAggregating LONG data with Events for ",interval[i],"-",unit,"intervals...\n")
				 f <- paste(c(paste(formulanames,collapse="+"), "variable"), collapse="~")
				 aggeventslist[[time.factor.names[i]]] <-
						#cast(aggmelt1, f, fun=mean, na.rm=na.rm)
					aggByPrefix(aggmelt1, f)						
				  aggeventslist[[time.factor.names[i]]] <- 
					name2numeric(aggeventslist[[time.factor.names[i]]],time.factor.names[i])
				 print(head(aggeventslist[[i]]))
			}
		}
	}

  #aggdata <- aggregate(alldata[value.name], by=alldata[aggnames], FUN=mean)

  #aggdatetime <- strptime("1970-01-01", "%Y-%m-%d", tz=tz)+aggregate(alldata[datetime.name], by=alldata[aggnames], FUN=function(x) x[1])[[datetime.name]]

  #aggdata <- cbind(aggdatetime, aggdata)
  #names(aggdata)[1] <- datetime.name
  #aggdata[[datetime.name]] <- as.POSIXct(strptime("1970-01-01", "%Y-%m-%d", tz="") + aggdata[[datetime.name]])
  #print(head(aggdata))

  # ==================================================================
  # Optionally create WIDE format versions of raw and aggregated data
  #   1.  Time versus each Value ID (e.g., "Sensor" ID)
  #   2.  All factors versus Time
  #     Table cells contain Values averaged over factors,etc. ....
  # ==================================================================

  if (make.wide) {

	cat("\nMake WIDE format data...\n\n")

    #aggmelt <- melt(aggdata, measure.var=value.name)
    #print(head(aggmelt))

    widenames <- c(id.names)
    if (includes)
      widenames <- c(widenames, include.id.names)
	widenames <- c(widenames, split.name[!is.na(split.name)])    
    if (merge.events)
      widenames <- c(widenames, event.value.names)
    if (merge.lookup)
      widenames <- c(widenames, lookup.value.names)
	  
	# Wide raw file with only time as a factor-  to do: do for all time intervals
    #widedata.time.value.in.cols.raw <- cast(aggmelt1, 
    #     paste(c(time.factor.names[1], value.id.name), collapse="~"), fun=mean, na.rm=na.rm)
    #cat("Wide data, Time Factor with Value in Columns:\n")
    #print(head(widedata.time.value.in.cols))		  
	
	# Raw Wide data with raw time only - no aggregation
	#for (i in 1:length(interval)) {
		 formulanames <- c(datetime.name)
		 cat("\nRaw time-only WIDE data Value in Cols for ",interval[i],"-",unit,"intervals...\n")
		 f <- paste(c(paste(formulanames,collapse="+"), value.id.name), collapse="~")
		 #widetime <- cast(aggmelt1, f, fun=mean, na.rm=na.rm)
		 widetime <-  as.data.frame(cast(aggmelt1, f))		# no aggregation, just RAW wide data
		 print(head(widetime))
	#}
	
	# Wide data with raw time + events -- aggregation
	if (merge.events) {
	#	for (i in 1:length(interval)) {
			 formulanames <- c(datetime.name, event.value.names)
			 cat("\nRaw WIDE data Value in Cols With Events for ",interval[i],"-",unit,"intervals...\n")
			 f <- paste(c(paste(formulanames,collapse="+"), value.id.name), collapse="~")
			 wideevents <- 
				#cast(aggmelt1, f, fun=mean, na.rm=na.rm)
				aggByPrefix(aggmelt1, f, wide=TRUE)
			 print(head(wideevents))
	#	}
	}
	  
	
	if (make.agg) {
	
		cat("\nMake WIDE-AGG format data...\n\n")
		
		# Wide AGG file with only time as a factor-  to do: do for all time intervals
		#widedata.time.value.in.cols <- cast(aggmelt1, 
		#     paste(c(time.factor.names[1], value.id.name), collapse="~"), fun=mean, na.rm=na.rm)
		#cat("Wide data, Time Factor with Value in Columns:\n")
		#print(head(widedata.time.value.in.cols))	

		#  Wide AGG data value in cols for each time factor only
		for (i in 1:length(interval)) {
			 formulanames <- c(time.factor.POSIX.names[i], time.factor.names[i])
			 cat("\nAggregating time-only WIDE data Value in Cols for ",interval[i],"-",unit,"intervals...\n")
			 f <- paste(c(paste(formulanames,collapse="+"), value.id.name), collapse="~")
			 aggwidetimelist[[time.factor.names[i]]] <-	
				#cast(aggmelt1, f, fun=mean, na.rm=na.rm)
				aggByPrefix(aggmelt1, f, wide=TRUE)
			 print(head(aggwidetimelist[[i]]))
		}
		
		#  Wide AGG data value in cols for just events
		if (merge.events) {
			for (i in 1:length(interval)) {
				 formula.names <- c(time.factor.POSIX.names[i], time.factor.names[i], event.value.names)
				 cat("\nAggregating WIDE data Value in Cols with Events for ",interval[i],"-",unit,"intervals...\n")
				 f <- paste(c(paste(formula.names,collapse="+"), value.id.name), collapse="~")
				 aggwideeventslist[[time.factor.names[i]]] <- 
					#cast(aggmelt1, f, fun=mean, na.rm=na.rm)
					aggByPrefix(aggmelt1, f, wide=TRUE)
				 print(head(aggwideeventslist[[i]]))
			}
		    cat("\nAggregating no-time WIDE data Value in Cols with Events for ",interval[i],"-",unit,"intervals...\n")			
			f <- paste(c(paste(event.value.names,collapse="+"), value.id.name), collapse="~")
			aggwideevents <- 
				#cast(aggmelt1, f, fun=mean, na.rm=na.rm)
				aggByPrefix(aggmelt1, f, wide=TRUE)
			print(head(aggwideevents))			
		}

		
		#print(paste(c(paste(c(time.factor.names[1],widenames), collapse="+"),
		#           value.id.name), collapse="~"))

		# only use first Time Factor name
		#widedata.value.in.cols <- cast(aggmelt1, 
		#     paste(c(paste(c(time.factor.names[1], widenames), collapse="+"),
		#           value.id.name), collapse="~"), fun=mean, na.rm=na.rm)
		#cat("Wide data Value in Columns:\n")
		#print(head(widedata.value.in.cols))
		
		#  Wide AGG data value in cols for each time factor + all other factors
		for (i in 1:length(interval)) {
			 formulanames <- c(time.factor.POSIX.names[i], time.factor.names[i], widenames)
			 cat("\nAggregating WIDE data Value in Cols for ",interval[i],"-",unit,"intervals...\n")
			 f <- paste(c(paste(formulanames,collapse="+"), value.id.name), collapse="~")
			 aggwidelist[[time.factor.names[i]]] <-	
				#cast(aggmelt1, f, fun=mean, na.rm=na.rm)
				aggByPrefix(aggmelt1, f, wide=TRUE)				
			 print(head(aggwidelist[[i]]))
		}
		

		
		#widenames <- c(widenames, value.id.name)
		#  Only use first Time Factor name
		#widedata.time.in.cols <- cast(aggmelt1, 
		#     paste(c(paste(widenames, collapse="+"), time.factor.names[1]),
		#           collapse="~"), fun=mean, na.rm=na.rm)
		#cat("Wide data Time in Columns:\n")
		#print(head(widedata.time.in.cols))
		
		#  Wide AGG data time factor in cols and just value response in rows
		for (i in 1:length(interval)) {
			 cat("\nAggregating WIDE data Time in Cols Value in Rows for ",interval[i],"-",unit,"intervals...\n")
			 f <- paste(c(value.id.name, time.factor.names[i]), collapse="~")
			 if (debug) {
				cat("Time Factor levels:\n")
				print(levels(aggmelt1[[time.factor.names[1]]]))
			 }
			 aggwideresponsecolslist[[time.factor.names[i]]] <-	
					#cast(aggmelt1, f, fun=mean, na.rm=na.rm)
					aggByPrefix(aggmelt1, f, wide=FALSE, add.missing=TRUE)					
			 print(head(aggwideresponsecolslist[[i]]))
		}
		
		#  Wide Agg time in cols -- with value response and events in rows
		if (merge.events) {
			#  Wide AGG data time factor in cols + event factors in rows
			for (i in 1:length(interval)) {
				 formulanames <- c(event.value.names, value.id.name)	
				 cat("\nAggregating WIDE data Time in Cols Events in Rows for ",interval[i],"-",unit,"intervals...\n")
				 f <- paste(c(paste(formulanames,collapse="+"), time.factor.names[i]), collapse="~")
				 aggwideeventscolslist[[time.factor.names[i]]] <- 
					#cast(aggmelt1, f, fun=mean, na.rm=na.rm)
					aggByPrefix(aggmelt1, f, wide=FALSE)
				 print(head(aggwideeventscolslist[[i]]))
			}
		}
			
		
		#  Wide AGG data time factor in cols + other factors in rows
		for (i in 1:length(interval)) {
			 formulanames <- c(widenames, value.id.name)	
			 cat("\nAggregating WIDE data Time in Cols Factors in Rows for ",interval[i],"-",unit,"intervals...\n")
			 f <- paste(c(paste(formulanames,collapse="+"), time.factor.names[i]), collapse="~")
			 aggwidecolslist[[time.factor.names[i]]] <-	
				#cast(aggmelt1, f, fun=mean, na.rm=na.rm)
				aggByPrefix(aggmelt1, f, wide=FALSE)
			 print(head(aggwidecolslist[[i]]))
		}
	}

	
  }

  # ================================================
  # CALCULATE STATISTICS ACROSS EACH ID VARIABLE?
  #   Optionally create statistics tables with values
  #   averaged over all factors (value id's, file id's, lookup, event)
  #       INCLUDING MARGINS ACROSS EACH FACTOR.....
  # ================================================
  if (make.stats) {

    cat("\nCalculating STATISTICS tables by factors...\n\n")
  
    f <- paste(c(paste(taken.key.names,collapse="+"),
                          "variable"), collapse="~")
	#print(head(aggmelt1))
    #print(f)						  
	fun <- function(x,na.rm=TRUE){c(mean=mean(x,na.rm=na.rm),
						sd=sd(x,na.rm=na.rm),median=median(x,na.rm=na.rm),
						min=min(x,na.rm=na.rm), max=max(x,na.rm=na.rm))}
	fun2 <- function(x,na.rm=TRUE){c(mean=mean(x,na.rm=na.rm),
						sd=sd(x,na.rm=na.rm))}
	id.stats <- cast(aggmelt1, 
                     #paste(c(paste(c(value.id.name, id.names),collapse="+"),
                     paste(c(paste(taken.key.names,collapse="+"),
                          "variable"), collapse="~"),
                          fun.aggregate=fun2,
					margins=FALSE)
    cat("File ID Stats:\n")
    print(head(id.stats))
    if (merge.lookup) {
      lookup.id.stats <- as.data.frame(cast(aggmelt1,
            paste(c(paste(c(value.id.name, lookup.value.names),collapse="+"),
                  "variable"), collapse="~"),
                  fun.aggregate=function(x,na.rm=TRUE){c(mean=mean(x,na.rm=na.rm),
						sd=sd(x,na.rm=na.rm),median=median(x,na.rm=na.rm),
						min=min(x,na.rm=na.rm), max=max(x,na.rm=na.rm))},
				na.rm=TRUE, margins=FALSE))
      cat("Lookup ID Stats:\n")
	  
      print(head(lookup.id.stats))
    }
    if (merge.events) {
	  cat("Starting event stats...\n")
      event.id.stats <- as.data.frame(cast(aggmelt1,
            c(paste(c(paste(c(value.id.name, event.value.names),collapse="+"),
                  "variable"), collapse="~")),
				#fun.aggregate=function(x,na.rm=TRUE){c(mean=mean(x,na.rm=na.rm),
				#		sd=sd(x,na.rm=na.rm),median=median(x,na.rm=na.rm),
				#		min=min(x,na.rm=na.rm), max=max(x,na.rm=na.rm))}				  
				  fun.aggregate=function(x,na.rm=TRUE) {c(mean=mean(x,na.rm=na.rm), sd=sd(x,na.rm=na.rm))},
						na.rm=TRUE, margins=FALSE))
      cat("Event ID Stats:\n")
      print(head(event.id.stats))
    }
  }
}


# ---------------------------------------------

#   S A V E   D A T A 

# ------------------------------------------

cat("\nSaving data...\n")

# Save as an R data frame
#assign(quote(name), alldata)

save(alldata, file=paste(name,"-Raw-LONG.RData", sep=""), envir=sys.frame(sys.nframe()))
if (save.csv) write.csv(alldata, file=paste(name,"-Raw-LONG.CSV", sep=""), row.names=FALSE,
           quote=TRUE)
		   
if (!is.null(reshape.formula)) {
	attach(aggcustom)
	for (i in 1:length(reshape.formula)) {
		save(list=paste("aggcustom",i,sep="-"), 
					file=paste(name,"-Agg-CUSTOM-",i,".RData",sep=""),
					envir=sys.frame(sys.nframe()))
		if (save.csv) write.csv(aggcustom[[i]], file=paste(name,"-Agg-CUSTOM-",i,".CSV",sep=""))
	}
	detach(aggcustom)
}		
		   
if (make.agg) {
  attach(agglist)
  for (i in 1:length(interval)) {
    save(list=time.factor.names[i], file=paste(name,"-Agg-LONG-",interval[i],unit,".RData", sep=""),
			envir=sys.frame(sys.nframe()))
    if (save.csv) write.csv(agglist[[i]], file=paste(name,"-Agg-LONG-",interval[i],unit,".CSV", sep=""), row.names=FALSE, quote=TRUE) 	 
  }
  detach(agglist)
  
  if (!is.null(elapsed.bykey.names)) {
	  attach(agglistbykey)
	  for (i in 1:length(interval)) {
		save(list=time.factor.names[i], file=paste(name,"-Agg-LONG-BYKEY-",interval[i],unit,".RData", sep=""),
				envir=sys.frame(sys.nframe()))
		if (save.csv) write.csv(agglistbykey[[i]], file=paste(name,"-Agg-LONG-BYKEY-",interval[i],unit,".CSV", sep=""), row.names=FALSE, quote=TRUE) 	 
	  }
	  detach(agglistbykey)
  }
  
  attach(aggtimelist)
  for (i in 1:length(interval)) {
    save(list=time.factor.names[i], file=paste(name,"-Agg-LONG-TIME-",interval[i],unit,".RData", sep=""),
			envir=sys.frame(sys.nframe()))
    if (save.csv) write.csv(aggtimelist[[i]], file=paste(name,"-Agg-LONG-TIME-",interval[i],unit,".CSV", sep=""), row.names=FALSE, quote=TRUE) 	 
  }
  detach(aggtimelist)
  
  if (merge.events) {
	  attach(aggeventslist)
	  for (i in 1:length(interval)) {
		save(list=time.factor.names[i], file=paste(name,"-Agg-LONG-EVENT-",interval[i],unit,".RData", sep=""),
			envir=sys.frame(sys.nframe()))
		if (save.csv) write.csv(aggeventslist[[i]], file=paste(name,"-Agg-LONG-EVENT-",interval[i],unit,".CSV", sep=""), row.names=FALSE, quote=TRUE) 	 
	  }
	  detach(aggeventslist)
  }
}

if (make.wide) {
	save(widetime, file=paste(name,"-Raw-WIDE-TIME.RData", sep=""),
				envir=sys.frame(sys.nframe()))
    if (save.csv) write.csv(widetime, 
							file=paste(name,"-Raw-WIDE-TIME.CSV", sep=""),
							row.names=FALSE, quote=TRUE)
	if (merge.events) {
		save(wideevents, file=paste(name,"-Raw-WIDE-EVENT.RData", sep=""),
				envir=sys.frame(sys.nframe()))			  
		if (save.csv) write.csv(wideevents, 
				  file=paste(name,"-Raw-WIDE-EVENT.CSV", sep=""),
				  row.names=FALSE, quote=TRUE)			  
	}
}

if (make.wide && make.agg) {
	attach(aggwidelist)
	for (i in 1:length(interval)) {
		save(list=time.factor.names[i], file=paste(name,"-Agg-WIDE-",interval[i],unit,".RData", sep=""),
				envir=sys.frame(sys.nframe()))
		if (save.csv) write.csv(aggwidelist[[i]], 
              file=paste(name,"-Agg-WIDE-",interval[i],unit,".CSV", sep=""), 
              row.names=FALSE, quote=TRUE)	   
	}
	detach(aggwidelist)
	
	attach(aggwidetimelist)
	for (i in 1:length(interval)) {
		save(list=time.factor.names[i], file=paste(name,"-Agg-WIDE-TIME-",interval[i],unit,".RData", sep=""),
				envir=sys.frame(sys.nframe()))
		if (save.csv) write.csv(aggwidetimelist[[i]], 
              file=paste(name,"-Agg-WIDE-TIME-",interval[i],unit,".CSV", sep=""), 
              row.names=FALSE, quote=TRUE)	   	   
	}
	detach(aggwidetimelist)
	
	attach(aggwidecolslist)
	for (i in 1:length(interval)) {
		save(list=time.factor.names[i], file=paste(name,"-Agg-WIDE-TimeCols-",interval[i],unit,".RData", sep=""),
				envir=sys.frame(sys.nframe()))
		if (save.csv) write.csv(aggwidecolslist[[i]], 
              file=paste(name,"-Agg-WIDE-TimeCols-",interval[i],unit,".CSV", sep=""), 
              row.names=FALSE, quote=TRUE)	 	   
	}
	detach(aggwidecolslist)   
	
	attach(aggwideresponsecolslist)
	for (i in 1:length(interval)) {
		save(list=time.factor.names[i], file=paste(name,"-Agg-WIDE-TimeCols-VALUE-",interval[i],unit,".RData", sep=""),
				envir=sys.frame(sys.nframe()))
		if (save.csv) write.csv(aggwideresponsecolslist[[i]], 
              file=paste(name,"-Agg-WIDE-TimeCols-VALUE-",interval[i],unit,".CSV", sep=""), 
              row.names=FALSE, quote=TRUE)	 	   
	}
	detach(aggwideresponsecolslist)   	
	
	if (merge.events) {
	
		attach(aggwideeventslist)
		for (i in 1:length(interval)) {
			save(list=time.factor.names[i], file=paste(name,"-Agg-WIDE-TIME-EVENT-",interval[i],unit,".RData", sep=""),
					envir=sys.frame(sys.nframe()))
			if (save.csv) write.csv(aggwideeventslist[[i]], 
				  file=paste(name,"-Agg-WIDE-TIME-EVENT-",interval[i],unit,".CSV", sep=""), 
				  row.names=FALSE, quote=TRUE)	  	   
		}
		detach(aggwideeventslist)   	
		
		save(aggwideevents, file=paste(name,"-Agg-WIDE-EVENT.RData", sep=""),
				envir=sys.frame(sys.nframe()))
		if (save.csv) write.csv(aggwideevents, 
				file=paste(name,"-Agg-WIDE-EVENT.CSV", sep=""), 
				row.names=FALSE, quote=TRUE)	
		
		attach(aggwideeventscolslist)
		for (i in 1:length(interval)) {
			save(list=time.factor.names[i], file=paste(name,"-Agg-WIDE-TimeCols-EVENT-",interval[i],unit,".RData", sep=""),
					envir=sys.frame(sys.nframe()))
			if (save.csv) write.csv(aggwideeventscolslist[[i]], 
				  file=paste(name,"-Agg-WIDE-TimeCols-EVENT-",interval[i],unit,".CSV", sep=""), 
				  row.names=FALSE, quote=TRUE)		   
		}
		detach(aggwideeventscolslist)   
		
	}
}


if (make.stats) {
    save(id.stats, 
         file=paste(name,"-Agg-STATS-ID.RData", sep=""), envir=sys.frame(sys.nframe()))
    if (save.csv) write.csv(id.stats, 
              file=paste(name,"-Agg-STATS-ID.CSV", sep=""), 
              row.names=FALSE, quote=TRUE)		 
    if (merge.lookup) {
		save(lookup.id.stats, 
            file=paste(name,"-Agg-STATS-LOOKUP.RData", sep=""), envir=sys.frame(sys.nframe()))
		if (save.csv) write.csv(lookup.id.stats, 
                file=paste(name,"-Agg-STATS-LOOKUP.CSV", sep=""), 
                row.names=FALSE, quote=TRUE)	
	}
    if (merge.events) {
      save(event.id.stats, 
           file=paste(name,"-Agg-STATS-EVENT.RData", sep=""), envir=sys.frame(sys.nframe()))
      if (save.csv) write.csv(event.id.stats, 
                file=paste(name,"-Agg-STATS-EVENT.CSV", sep=""), 
                row.names=FALSE, quote=TRUE)
	}
}



# ---------------------------------------------

#   P L O T    D A T A 

# --------------------------------------------

#  Modular plotting functions

make.diagnostic.plot <- function () {

   condition <- unique(c(plot.group.key, value.id.name))
   #if (!is.null(plot.conditioning))
   #	condition <- unique(c(condition, plot.conditioning))
   form <- as.formula(paste(c(paste(c(value.name,elapsed.name), collapse="~"),
								paste(condition,collapse="*")), collapse="|"))   								   
   print(xyplot(form,
                 data=plotdata, 
                 main=list(paste(main,"by",value.id.name), cex=cex.main), 
                 as.table=as.table, cex=cex, type="p", 
                 ylab=list(value.name, cex=cex.lab), 
                 xlab=list(paste("Elapsed", unit), cex=cex.lab),
                 scales=scales.elapsed, ...))
   dev.off()

   cat("Made overall diagnostic plot...\n")
}

make.id.names.plot <- function(condition) {
     if (!is.null(plot.conditioning))
		condition <- unique(c(plot.conditioning, condition))
  	 form <- as.formula(paste(c(paste(c(value.name,xname), collapse="~"),
								paste(condition,collapse="*")), collapse="|"))   								   
			
     print(xyplot(form,
           groups=get(plot.group.key), data=plotdata, 
           layout=layout, as.table=as.table, 
           type=type, pch=16, lty=c("solid"), lwd=c(2,2), col=col.lines,
           panel=panel.superpose.3a,
           scales=scales,
           ylab=list(ylab, cex=cex.lab), 
           xlab=list(xlab, cex=cex.lab),
           main=list(paste(main,"by",i), cex=cex.main),...))
     dev.off()
	 cat("Finished FILEID plot: ",condition,"\n")
}

make.includes.plot <- function(condition) {

	   if (!is.null(plot.conditioning))
				condition <- unique(c(plot.conditioning, condition))
				
	   form <- as.formula(paste(c(paste(c(value.name,xname), collapse="~"),
				paste(condition,collapse="*")), collapse="|"))   								   
		
       print(xyplot(form,
           groups=get(plot.group.key), data=plotdata, 
           layout=layout, as.table=as.table, 
           type=type, pch=16, lty=c("solid"), lwd=c(2,2), col=col.lines,
           panel=panel.superpose.3a,
           scales=scales,
           ylab=list(ylab, cex=cex.lab), 
           xlab=list(xlab, cex=cex.lab),
           main=list(paste(main,"by",i), cex=cex.main),...))
       dev.off()
	   cat("Finished INCLUDEID plot: ",condition,"\n")
}	   

make.includes.as.events.plot <- function(k) {

	   if (!is.null(plot.conditioning.events))
		condition <- unique(c(plot.conditioning.events, k))

	   if (!is.null(plot.conditioning.events)) 
		form <- as.formula(paste(c(paste(c(value.name,xname), collapse="~"),
			paste(condition,collapse="*")), collapse="|"))   								   
	   else
		form <- as.formula(paste(c(value.name,xname), collapse="~"))				   
		   
	   print(xytact(form,
		 groups=get(plot.group.key), data=plotdata,
		 tact.var=k, codes.colors=col.areas[1:length(unique(plotdata[[k]]))],
		 layout=layout, as.table=as.table,
		 type=type, pch=16, lty=c("solid"), lwd=c(2,2), col=col.lines,
		 scales=scales,
		 ylab=list(ylab, cex=cex.lab),
		 xlab=list(xlab, cex=cex.lab),
		 main=list(paste(main,"with",k), cex=cex.main),...))
	   dev.off()
	   cat("Finished INCLUDE EVENTS plot: ",k,"\n")	
}			   


make.includes.as.events.ALL.plot <- function () {

	  if (!is.null(plot.conditioning.events)) 
		form <- as.formula(paste(c(paste(c(value.name,xname), collapse="~"),
				paste(plot.conditioning.events, collapse="*")), collapse="|")) 							   
	  else
		form <- as.formula(paste(c(value.name,xname), collapse="~"))
			   
	  print(xytact(form,
		 groups=get(plot.group.key), data=plotdata,
		 tact.var=include.id.names,
		 codes.colors=col.areas[1:length(unique(as.character(as.matrix(plotdata[include.id.names]))))],
		 layout=layout, as.table=as.table,
		 type=type, pch=16, lty=c("solid"), lwd=c(2,2), col=col.lines,
		 scales=scales,
		 ylab=list(ylab, cex=cex.lab),
		 xlab=list(xlab, cex=cex.lab),
		 main=list(paste(main,"with",paste(include.id.names,collapse="+")),
		 cex=cex.main*0.75),...))
	  dev.off()
	  cat("Finished overall INCLUDE EVENTS plot.\n")
}

make.lookup.plot <- function (condition) {

	if (!is.null(plot.conditioning))
	   condition <- unique(c(plot.conditioning, condition))
	   
	form <- as.formula(paste(c(paste(c(value.name,xname), collapse="~"),
							paste(condition,collapse="*")), collapse="|")) 
							
	print(xyplot(form,
		 groups=get(plot.group.key), data=plotdata,
		 layout=layout, as.table=as.table,
		 type=type, pch=16, lty=c("solid"), lwd=c(2,2), col=col.lines,
		 panel=panel.superpose.3a,
		 scales=scales,
		 ylab=list(ylab, cex=cex.lab),
		 xlab=list(xlab, cex=cex.lab),
		 main=list(paste(main,"by",i), cex=cex.main),...))
	 dev.off()
   
   cat("Finished LOOKUP plot: ",condition,"\n")
}


make.event.plot <- function(k) {

    if (!is.null(plot.conditioning.events))
	   form <- as.formula(paste(c(paste(c(value.name,xname), collapse="~"),
		paste(plot.conditioning.events, collapse="*")), collapse="|")) 							   
    else 
	   form <- as.formula(paste(c(value.name,xname), collapse="~"))
	   
    print(xytact(form,
		 groups=get(plot.group.key), data=plotdata,
		 tact.var=k, codes.colors=col.areas[1:length(unique(plotdata[[k]]))],
		 layout=layout, as.table=as.table,
		 type=type, pch=16, lty=c("solid"), lwd=c(2,2), col=col.lines,
		 scales=scales,
		 ylab=list(ylab, cex=cex.lab),
		 xlab=list(xlab, cex=cex.lab),
		 main=list(paste(main,"with",k), cex=cex.main),...))
    dev.off()
    cat("Finished EVENTS plot ",k,"\n")		
			   
}			   

make.event.ALL.plot <- function () {

	if (!is.null(plot.conditioning.events))
		   form <- as.formula(paste(c(paste(c(value.name,xname), collapse="~"),
							paste(plot.conditioning.events, collapse="*")), collapse="|")) 							   
	else 
		   form <- as.formula(paste(c(value.name,xname), collapse="~"))

	print(xytact(form,
		groups=get(plot.group.key), data=plotdata,
		tact.var=plot.event.names,
		codes.colors=col.areas[1:length(unique(as.character(as.matrix(plotdata[plot.event.names]))))],
		layout=layout, as.table=as.table,
		type=type, pch=16, lty=c("solid"), lwd=c(2,2), col=col.lines,
		scales=scales,
		ylab=list(ylab, cex=cex.lab),
		xlab=list(xlab, cex=cex.lab),
		main=list(paste(main,"with",paste(plot.event.names,collapse="+")),
		cex=cex.main*0.75),...))
	dev.off()
	cat("Finished overall EVENTS plot.\n")	
}				


# =============================================================

# Create lattice diagnostic plots as a postscript file
#          using above functions to make SVG, BITMAP, PS, and/or PDF files...

cat("\nPlotting data...\n")

#  Create time axis for minimum day at midnight to maximum day+1 at midnight
#    in alldata  by the specified 'plot.interval' argument, 
#    defaulting to "hour".   This makes the time intervals fall on
#    event intervals, e.g., hour, half-hour, 4-h, 6-h, etc.
#  !!! See DateTimeClsses for info on elements of POSIXlt
min.time <- min(as.POSIXlt(alldata[[datetime.name]], tz=tz))
max.time <- max(as.POSIXlt(alldata[[datetime.name]], tz=tz))
min.year <- min.time$year + 1900
min.mon <- min.time$mon + 1
min.mday <- min.time$mday
max.year <- max.time$year + 1900
max.mon <- max.time$mon + 1
max.mday <- max.time$mday

time.seq <- seq.POSIXt(ISOdatetime(min.year,min.mon,min.mday,0,0,0,tz=tz), 
                        ISOdatetime(max.year,max.mon,max.mday,23,59,59,tz=tz),
                        by=plot.interval)
#cat("Max.time",max.time,"\n")						
#cat("Min.time",min.time,"\n")						
#print(time.seq)			

scales.tod <- list(x=list(at=time.seq,format=plot.format,cex=cex.axis, relation=relation.x, rot=rot.tod, log=log.x),
					y=list(cex=cex.axis, relation=relation.y, log=log.y))
scales.elapsed <- list(cex=cex.axis, x=list(relation=relation.x, log=log.x), 
						y=list(relation=relation.y, log=log.y))			

if (plot.tod) {
	scales <- scales.tod
	xname <- datetime.name
} else {
	scales <- scales.elapsed
	xname <- elapsed.name
}	

if (!plot.group.key %in% taken.names)
      stop("'plot.group.key' (", plot.group.key,") not among available names.")						

if (plot) {
   require(lattice)
   #print(head(alldata))
   
   if (make.bitmap) warning("To makes bitmap graphics, Ghostscript must be installed and `gwin32c.exe' in your PATH.")
   
   plotnames <- c(elapsed.name, datetime.name, value.name, 
                  taken.key.names)
          # id.names, value.id.name)
   if (merge.lookup) plotnames <- c(plotnames, lookup.value.names)
   if (merge.events) plotnames <- c(plotnames, event.value.names)
   plotdata <- subset(alldata, select=plotnames)

   # Diagnostic plot of all values in a separate panel
   if (make.svg) {
	require(RSvgDevice)
	devSVG(file=paste(name,"-Plot-VALUES",".svg",sep=""), 
             width=width, height=height, onefile=onefile)
	make.diagnostic.plot()
   }
   if (make.ps) {
	postscript(file=paste(name,"-Plot-VALUES",".ps",sep=""),
		width=width, height=height, horizontal=TRUE, onefile=onefile)
	make.diagnostic.plot()
   }
   if (make.bitmap) {
	bitmap(file=paste(name,"-Plot-VALUES.",type.bitmap,sep=""),
			width=width, height=height, type=type.bitmap, onefile=onefile)
	# jpeg(filename=paste(name,"%03d.jpeg",sep=""), width=width, height=height,
	#       units = "in", res=72)
	make.diagnostic.plot()
   }
   if (make.pdf) {
	pdf(file=paste(name,"-Plot-VALUES",".pdf",sep=""), 
		width=width, height=height, onefile=onefile)	 
	make.diagnostic.plot()
   }
   
   
   # Plots by each factor in the "id.names"
   require(grid)
   for (i in id.names) {
     #npages <- 1 + length(unique(plotdata[[i]])) %/% 4
     #bitmap(file=paste(name,"-FILEID-",i,".jpeg",sep=""),width=width, height=height,
     #      type="jpeg")
     if (make.svg) {
       require(RSvgDevice)
       devSVG(file=paste(name,"-Plot-FILEID-",i,".svg",sep=""), 
             width=width, height=height, onefile=onefile)
	    make.id.names.plot(i)			 
     }
	 if (make.ps) {
        postscript(file=paste(name,"-Plot-FILEID-",i,".ps",sep=""), 
					width=width, height=height, horizontal=TRUE, onefile=onefile)
	    make.id.names.plot(i)					
     }
     if (make.bitmap) {
		bitmap(file=paste(name,"-Plot-FILEID-",i,".",type.bitmap, sep=""),
				width=width, height=height, units="in", type=type.bitmap, onefile=onefile)
		make.id.names.plot(i)
     }	 
	 if (make.pdf) {
		pdf(file=paste(name,"-Plot-FILEID-",i,".pdf",sep=""), 
					width=width, height=height, onefile=onefile)		
	    make.id.names.plot(i)					
	 } 
   }


   # Plots by each factor in the "include.id.names"
   if (includes) {
     require(grid)
     for (i in include.id.names) {
       #npages <- 1 + length(unique(plotdata[[i]])) %/% 4
       if (make.svg) {
         require(RSvgDevice)
         devSVG(file=paste(name,"-Plot-INCLUDEID-",i,".svg",sep=""), 
               width=width, height=height, onefile=onefile)
	       make.includes.plot(i)			   
       }
       if (make.ps) {
         postscript(file=paste(name,"-Plot-INCLUDEID-",i,".ps",sep=""), 
		        width=width, height=height, horizontal=TRUE, onefile=onefile)
	       make.includes.plot(i)				
	   }
       if (make.bitmap) {
		  bitmap(file=paste(name,"-Plot-INCLUDEID-",i,".",type.bitmap,sep=""),
				width=width, height=height, units="in", type=type.bitmap, onefile=onefile)
		  make.includes.plot(i)
       }	 	   
       if (make.pdf) {
		   pdf(file=paste(name,"-Plot-INCLUDEID-",i,".pdf",sep=""), 
				width=width, height=height, onefile=onefile)	
	       make.includes.plot(i)				
	   }
				
     }
	 
	if (includes.as.events) {
		require(heR.Activities)
        for (k in include.id.names) {
            if (!all(is.na(plotdata[[k]]))) {
               if (make.svg) {
                 require(RSvgDevice)
                 devSVG(file=paste(name,"-Plot-INCLUDE-EVENTS-",k,".svg",sep=""), 
                        width=width, height=height, onefile=onefile)
			     make.includes.as.events.plot(k)						
               }
	       if (make.ps) {
                       postscript(file=paste(name,"-Plot-INCLUDE-EVENTS-",k,".ps",sep=""), 
                       width=width, height=height, horizontal=TRUE, onefile=onefile)
			     make.includes.as.events.plot(k)					   
			   }
			   if (make.bitmap) {
				bitmap(file=paste(name,"-Plot-INCLUDE-EVENTS-",k,".",type.bitmap,sep=""),
						width=width, height=height, type=type.bitmap, onefile=onefile)
				make.includes.as.events.plot(k)
			   }	 	   			   
			   if (make.pdf) {
				  pdf(file=paste(name,"-Plot-INCLUDE-EVENTS-",k,".pdf",sep=""), 
                       			width=width, height=height, onefile=onefile)
			      make.includes.as.events.plot(k)					   
			   }
					   
		    }
          }
		  
		  # Now plot ALL INCLUDE events in each panel
		  if (make.svg) {
			 require(RSvgDevice)
			 devSVG(file=paste(name,"-Plot-INCLUDE-EVENTS-ALL.svg",sep=""), 
					width=width, height=height, onefile=onefile)
			 make.includes.as.events.ALL.plot()				   					
		  } 
		  if (make.ps) {
			 postscript(file=paste(name,"-Plot-INCLUDE-EVENTS-ALL.ps",sep=""), 
				   width=width, height=height, horizontal=TRUE, onefile=onefile)
			 make.includes.as.events.ALL.plot()				   				   
		  } 
		  if (make.bitmap) {
				bitmap(file=paste(name,"-Plot-INCLUDE-EVENTS-ALL.",type.bitmap,sep=""),
					width=width, height=height, type=type.bitmap, onefile=onefile)
				make.includes.as.events.ALL.plot()
		  }	 	   			   		  
		  if (make.pdf) {
			  pdf(file=paste(name,"-Plot-INCLUDE-EVENTS-ALL.pdf",sep=""), 
				   width=width, height=height, onefile=onefile)
			  make.includes.as.events.ALL.plot()				   
		  }
				   
		}
   }


   # Plots by each factor in the Lookup table
   if (merge.lookup) {
     require(grid)
     for (i in lookup.value.names) {
       #npages <- 1 + length(unique(plotdata[[i]])) %/% 4
       if (!all(is.na(plotdata[[i]]))) {
         if (make.svg) {
            require(RSvgDevice)
            devSVG(file=paste(name,"-Plot-LOOKUP-",i,".svg",sep=""), 
                  width=width, height=height, onefile=onefile)
			make.lookup.plot(i)					  				  
         } 
		 if (make.ps) {
  		    postscript(file=paste(name,"-Plot-LOOKUP-",i,".ps",sep=""), 
                       width=width, height=height, horizontal=TRUE, onefile=onefile)
			make.lookup.plot(i)					  					   
		 }
		 if (make.bitmap) {
				bitmap(file=paste(name,"-Plot-LOOKUP-",i,".",type.bitmap,sep=""),
						width=width, height=height, type=type.bitmap, onefile=onefile)
				make.lookup.plot(i)
		 }	 	   			   		 
         if (make.pdf) {
			pdf(file=paste(name,"-Plot-LOOKUP-",i,".pdf",sep=""), 
                      width=width, height=height, onefile=onefile)			
			make.lookup.plot(i)					  
		 }
					   
	   }
     }
   }

   #  Create time plots segmented by Events....
   if (merge.events) {
	 require(heR.Activities)
     #for (j in all.value.names) {
        #plotdata <- alldata[alldata[[value.id.name]] == j, ]
		  cat("Plotting by event names: ",plot.event.names,"\n\n")
          for (k in plot.event.names) {
             if (!all(is.na(plotdata[[k]]))) {
               #print(j)
               #print(k)
               #print(plotdata[c(elapsed.name, value.name, k)])
               if (make.svg) {
					require(RSvgDevice)
					devSVG(file=paste(name,"-Plot-EVENTS-",k,".svg",sep=""), 
                        width=width, height=height, onefile=onefile)
					make.event.plot(k)					   						
               }
			   if (make.ps) {
                 #bitmap(file=paste(name,"-EVENTS-",k,".jpeg",sep=""), 
                 #      width=width, height=height, type="jpeg")
					postscript(file=paste(name,"-Plot-EVENTS-",k,".ps",sep=""), 
                       width=width, height=height, horizontal=TRUE, onefile=onefile)
					make.event.plot(k)					   					   
               }
			   if (make.bitmap) {
					bitmap(file=paste(name,"-Plot-EVENTS-",k,".",type.bitmap,sep=""),
						width=width, height=height, type=type.bitmap, onefile=onefile)
					make.event.plot(k)
			   }	 	   			   			   
			   if (make.pdf) {
					pdf(file=paste(name,"-Plot-EVENTS-",k,".pdf",sep=""), 
                       width=width, height=height, onefile=onefile)
					make.event.plot(k)					   
				}
					   
	   
             } else
				cat("\nNo non-NA values found for plot.events.name: ",k, "\n")
          }
		  # Now plot ALL events in each panel
		  if (!all(is.na(as.matrix(plotdata[plot.event.names])))) {		  
				if (make.svg) {
					 require(RSvgDevice)
					 devSVG(file=paste(name,"-Plot-EVENTS-ALL.svg",sep=""), 
							width=width, height=height, onefile=onefile)
					make.event.ALL.plot()						   							
				}
				if (make.ps) {
					 postscript(file=paste(name,"-Plot-EVENTS-ALL.ps",sep=""), 
						   width=width, height=height, horizontal=TRUE, onefile=onefile)
					make.event.ALL.plot()						   
				} 
			    if (make.bitmap) {
					bitmap(file=paste(name,"-Plot-EVENTS-ALL.",type.bitmap,sep=""),
						width=width, height=height, type=type.bitmap, onefile=onefile)
					make.event.ALL.plot()
			    }	 	   			   				
				if (make.pdf) {
					pdf(file=paste(name,"-Plot-EVENTS-ALL.pdf",sep=""), 
						   width=width, height=height, onefile=onefile)
					make.event.ALL.plot()						   
				}
				
	  
		  } else {
			cat("\nNo non-NA values found for any plot.events.names.\n")
		  }
     # }
	 
	   cat("\n===================================================\n")
	   cat("Note:  `plot.group.key' is currently set to ",plot.group.key,"\n")
	   cat("If you are having trouble getting time-activity plots, try setting this\n")
	   cat("parameter to a different key variable.  Also try setting\n")
	   cat("`plot.conditioning.events', which adds extra conditioning variables\n")
	   cat("to each lattice plot.  Also, make sure times are strictly increasing.\n")
   }
}


# ----------------------------
#  A L L   D O N E

cat("\n...Finished processing '",name,"' files.\n\nYippee!\n\n", sep="")

}
