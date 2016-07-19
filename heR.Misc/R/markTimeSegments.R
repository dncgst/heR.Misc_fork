markTimeSegments <-function (x, date, time, values, format="%m/%d/%Y %H:%M:%S",
                              plot.format="%m/%d/%y %a %H:%M",  factor=NULL,
                              fit.model=FALSE, units="mins",  threshold=NULL,
                              plot.interval="1 hour",	type="l", cex=0.35, pch=16, ylog=FALSE, 
                              relax.increasing=FALSE, tz="",
                              debug=FALSE, main="", xlab="Time", ylab="Response Value", 
                              col="yellow", lwd=1.2, lty="solid", 
                              bg="black", fg="white", col.axis="white", 
                              col.lab="gray90", col.main="white", col.segment.lab="white", col.border="gray80", 
                              tck=NA, tcl=-0.5,  cex.main=2, cex.lab=1,
                              pal=c(rainbow(12), "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                                    "#FF7F00", "#FFFF33", "#A65628"), fill=TRUE, alpha=0.2, density=9,
                              width.pdf = 11, height.pdf = 8.5, width.png=800, height.png=480,
                              ...)
{
  
  # RGtk2 function to read in a real-time monitor data file, set date-time
  #    variables and allow user to click on a graph to specify times
  #    surrounding time segments, e.g., peaks.   Lets user label each segment
  #    and save all marked segments to a specific CSV file.
  
  #   BUG?  After doing a large session with many segments, sometimes a weird thing happens:
  #		The function seems to repeatedly draw the same segments over and over again
  #		in an infinite loop.    ....Probably something to do with staring a new
  #			draw cycle before last one finishes......
  #			 ... NK 5-July-2011
  
  # TODO:  Save all segments to a factor in the data -- so we can do our own analysis on it
  #			more easily later.   Or just allow for saving the factor itself.
  #	Add option to load segments from a factor in the data.  We automatically calculate all the
  #		extra data internally whenever segments are loaded -- either using a segment file
  #		or from a factor of equal length as the data.  NK  17-April2012
  
  # 	x = the full data with names,  Time.POSIX, Values, Index
  
  #  MORE TODO:    Add arguments for loading everything from a data file -- with specified
  #		variable names -- use standard ones for default.   Also, add GUI option for saving the data
  #		frame with a factor embedded or saving the factor by itself for use in any analysis
  #		we may want to do.....
  
  #  TODO:   Have option to overlay different types of events -- stacked like we do with 
  #			the lattice functions -- have a GUI "model" element to specify which activity is being
  #			edited (and how many total activity variables are being added to the data).
  
  # UPDATE:  fix save of segment RData (use save command instead of dump).  NK 9May2014
  # UPDAET:   Added csv and pdf extensions if not alrady there.  Add .Rdata extension..  NK 17Apr2012.
  # UPDATE:  6-July-2011.  Fixed Mouse coordinates to work again.
  # UPDATE:  5-July-2011.  We now only draw segments that fall in the visible window, 
  #			substantially speeding up drawing for large data sets (when zoomed in).
  # UPDATE:  2-May-2010.   Add option to have fill with transparent alpha channel, also fixed log Y axis
  #				always filling with tranparent color instead of hatching since `rect' does not allow
  #				hatching with a log axis...  NK
  # UPDATE:   1-May-2010.  Added user control of tic marks, tic labels, fitting model, log Y axis, ...
  # UPDATe:   18-Apr-2010.   Added miscellesous scrolling/zooming functions, buttons for 
  #					showing points, pointsize, threshold, etc.
  # UPDATE:   17-Apr-10.  Added "auto-end" feature for new segments that automatically assigns
  #			the end point based on the plot.interval value.
  # UPDATE:	16-Apr-2010 - Output stats and decay model parameter for each defined time segment
  #				We do a linear fit to the log-transformed data
  # UPDATE:    16-Apr-2010 - Show stats for each defined time segment
  # UPDATE:   15-Apr-2010 -  Made a lot of GUI improvements, more options for graphics...
  # UPDATE:   4-Apr-2010  Added tz argument where it was missing...
  # UPDATE:  5-March-2010  -- Added option to include x-axis tic marks at a specified
  #             time interval, defaulting to 1 hour -- user can change this interactively
  # UPDATE:  4-March-2010  -- Added "relax.increasing" argument to allow plotting of
  #            points with non-increasing time values.
  
  #  UPDATE:  1-Jan-2010  -- optionally have a single date-time variable passed
  #         in the date argument, if time is missing
  
  #require(heR.Misc) || stop("heR.Misc package is not available.")
  if (missing(time)) datetime <- get.time.POSIX(date, format=format,
                                                relax.increasing=relax.increasing, tz=tz)$Time.POSIX
  else datetime <- get.time.POSIX(paste(date, time), format=format,
                                  relax.increasing=relax.increasing, tz=tz)$Time.POSIX
  if (length(values) != length(datetime))
    stop("Date/Time and Values must be same length.")
  else
    data <- data.frame(datetime=datetime, values=values)
  
  if (fit.model) data$fitted <- NA	# initialize fitted values
  
  #dataMin <- min(values)
  #dataMax <- max(values)
  #timeMin <- min(datetime)
  #timeMax <- max(datetime)
  #incrTime <- 0.10*(timeMax - timeMin)
  #incrValue <- 0.10*(dataMax - dataMin)
  xlim0 <- range(datetime, na.rm=TRUE)
  ylim0 <- c(0, max(values, na.rm=TRUE))
  xlim1 <- xlim0
  ylim1 <- ylim0
  dataSegments <- data.frame()
  dataSegmentsSingle <- data.frame()  #  Version with just single column of start times
  
  #  Decompose the interval specification
  plot.interval <- strsplit(plot.interval , " ")[[1]]
  plot.tic <- plot.interval[1]
  plot.units <- plot.interval[2]
  
  
  # Convert window coordinates to plot region coordinates
  win2plot <- function (x, y) {
    if (debug) cat("\n\nx=",x," y=",y,"\n")
    #pa <- plotArea$Allocation$allocation
    pa <- gtkWidgetGetAllocation(plotArea)
    if (debug) {print(pa)}
    w <- pa$allocation$width	# width of drawing area in pixels
    h <- pa$allocation$height	# height of drawing area in pixels
    if (debug) cat("w=",w," h=",h,"\n")
    fr <- par()$plt   # x1,x2,y1,y2: plot coor as fraction of figure/window coor
    wp <- w*(fr[2] - fr[1])  # width of plot in pixels
    hp <- h*(fr[4] - fr[3])	# height of plot in pixels
    if (debug) cat("wp=",wp," hp=",hp,"\n")	
    usr <- par()$usr   # x1,x2,y1,y2: plot user coordinates
    wu <- usr[2] - usr[1]  #  width of plot in user units
    hu <- usr[4] - usr[3]	# height of plot in user units
    if (debug) cat("wu=",wu," hu=",hu,"\n")	
    xp <- wu / wp   # x user units per pixel
    yp <- hu / hp   # y user units per pixel
    if (debug) cat("xp=",xp," yp=",yp,"\n")	
    mx <- fr[1] * w * xp  # user units from origin to left edge of drawing area
    my <- fr[3] * h * yp	# user units from origin to bottom edge of drawing area
    if (debug) cat("mx=",mx," my=",my,"\n")		
    newx <- x*xp + usr[1] - mx
    newy <- (h - y)*yp + usr[3] - my
    if (debug) cat("newx=",newx," newy=",newy,"\n")	
    c(newx, newy)
  }
  
  # Convert seconds since 1/1/1970 to string rep of time
  secs2Time <- function (secs, string=TRUE) {
    #start <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = tz)
    #as.POSIXct(start+secs)
    class(secs) <- "POSIXct"
    if (string)
      format(secs, format, tz=tz)
    else	
      as.POSIXct(as.POSIXlt(secs, tz=tz))    # not sure why we have to do this to get the tz right but we do....
  }
  
  
  #  Plot the data with nice time axis, showing any existing marked segments
  #   --> only plot data and xaxis for times between specific limits
  #		--> now we also don't plot segments that don't fall between time limits...
  #			NK  5-July-2011
  plotIT <- function (ylim=NULL, xlim=NULL) {
    
    cat("Plot Y limits: ",ylim,"\n")
    cat("Plot X limits: ",xlim,"\n")	
    
    idx <- datetime >= xlim[1] & datetime <= xlim[2]
    #datetime2 <- datetime[datetime >= xlim[1] & datetime <= xlim[2]]
    #values2 <- values[datetime >= xlim[1] & datetime <= xlim[2]]
    #data2 <- subset(data, datetime >= xlim[1] & datetime <= xlim[2])
    data2 <- subset(data, idx)
    min.time <- min(as.POSIXlt(data2$datetime, tz=tz))
    min.year <- min.time$year + 1900
    min.mon <- min.time$mon + 1
    min.mday <- min.time$mday
    
    #  Compose the new plot format specificition from widget status
    plot.tic <- intervalSpin$GetValue()
    plot.units <- intervalComboBox$GetActiveText()
    plot.interval <<- paste(plot.tic, plot.units)
    
    time.seq <- seq.POSIXt(ISOdatetime(min.year,min.mon,min.mday,0,0,0,tz=tz),
                           data2$datetime[NROW(data2)],
                           by=plot.interval)
    
    tck2 <- tck
    tcl2 <- tcl
    if (gridButton$GetActive()) {
      cat("Plotting gridlines...\n")
      tck2 <- 1
      tcl2 <- NA
    }
    
    type2 <- type
    cex2 <- cex
    if (pointsButton$GetActive()) {
      cat("Plotting points...\n")
      type2 <- "p"
      cex2 <- pointSizeSpin$GetValue()
    }
    
    ylim2 <- ylim
    if (logButton$GetActive()) {
      cat("Using Log Y Axis...\n")
      cat("[Only plotting values greater than 0.]\n")			
      log2 <- "y"
      if (all(ylim < 0)) ylim2 <- c(0,1)
      else if (ylim[1] <=0) ylim2 <- c(ylim[1]+0.00001,ylim[2])
    } else {
      log2 <- ""
    }
    
    
    par(fg=fg, bg=bg, col.axis=col.axis, col.main=col.main, col.lab=col.lab, tck=tck2, tcl=tcl2)
    plot(data2$datetime, data2$values, type=type2, xaxt="n", 
         ylim=ylim2, xlim=xlim, cex=cex2, pch=pch, col=col, lwd=lwd, lty=lty,
         main=main, cex.main=cex.main, cex.lab=cex.lab, xlab=xlab, ylab=ylab, log=log2, ...)
    
    if (fit.model)
      points(data2$datetime, data2$fitted, col="white", lwd=2.1,
             type="l", lty="dashed")
    
    
    if (thresholdButton$GetActive())
      abline(h=as.numeric(thresholdEntry$GetText()), col="red", lwd=2.5, lty="dotted")
    
    
    plot.format <<- formatEntry$GetText()
    
    axis.POSIXct(1, data2$datetime, at=time.seq, format=plot.format)	
    
    y1 <- par()$usr[3]
    y2 <- par()$usr[4]
    if (par("ylog")) {
      y1 <- 10^y1
      y2 <- 10^y2
    }
    #print(list(y1=y1, y2=y2))
    
    fill2 <- fill
    if (fillButton$GetActive()) fill2 <- TRUE
    
    
    
    if (NROW(dataSegments) > 0) {
      #  Only draw segments that have one limit in bounds or both limits out of
      #		bounds.  NK  5-July-2011
      dataSegments2 <- subset(dataSegments, (Time1 >= xlim[1] & Time1 <= xlim[2]) |
                                (Time2 >= xlim[1] & Time2 <= xlim[2]) |
                                (Time1 <= xlim[1] & Time2 >= xlim[2]))	
      for (i in 1:NROW(dataSegments2)) {
        cat("Plotting Segment ",i," ID=", dataSegments2$ID[i],
            " Color= ",as.character(dataSegments2$Color[i]),
            " Mean = ",as.character(dataSegments2$Mean[i]),
            " Max = ",as.character(dataSegments2$Max[i]),
            " Decay = ",as.character(dataSegments2$Decay[i]),
            "\n")
        xs <- c(dataSegments2$Time1[i], dataSegments2$Time2[i])
        abline(v=xs[1], col=col.border)
        abline(v=xs[2], col=col.border)
        if (par("ylog") | fill2)
          rect(xs[1], y1, xs[2], y2,
               col=rgb(t(col2rgb(as.character(dataSegments2$Color[i])))/255, 
                       alpha=alphaSpin$GetValue()),
               border=col.border)			
        else
          rect(xs[1], y1, xs[2], y2,
               col=as.character(dataSegments2$Color[i]),
               density=densitySpin$GetValue(), angle=dataSegments2$Angle[i],
               border=col.border)
        text(xs[1], y2, label=dataSegments2$SegmentLabel[i],
             adj=c(1.05,1.05), srt=90, col=col.segment.lab, xpd=TRUE)
        text(xs[1], y2, label=dataSegments2$ID[i], adj=c(-0.5,-0.5),
             col=col.lab, xpd=TRUE)
        
      }
    }
  }
  
  
  # =======================================================================
  
  require(RGtk2) || stop("Required `RGtk2' package is not available.")
  require(cairoDevice) || stop("Required `cairoDevice' package is not available")
  
  
  Top <- gtkWindow(show = FALSE)
  Top$AddCallback("destroy-event", function(w, ev) graphics.off())
  Top$SetTitle("Mark Time Segments")
  MainBox <- gtkVBox(spacing = 10)
  Top$Add(MainBox)
  
  MenuBar <- gtkMenuBar()
  FileMenu <- gtkMenu()
  FileItem <- gtkMenuItem("File")
  MenuBar$Append(FileItem)
  FileItem$SetSubmenu(FileMenu)
  OpenItem <- gtkMenuItem("Open Segments...")
  SaveItem <- gtkMenuItem("Save Segments...")
  SaveDataItem <- gtkMenuItem("Save Data...")
  SaveDataItem$AddCallback("activate", function(w, cmd) {
    cat("\nSaving data frame...\n")
    dialogSaveData <- gtkFileChooserDialog("Save Data Frame", NULL, "save",
                                           "gtk-cancel", GtkResponseType["cancel"], 
                                           "gtk-save", GtkResponseType["accept"], 
                                           show=FALSE)
    if (dialogSaveData$run() == GtkResponseType["accept"]) {
      filename <- dialogSaveData$getFilename()
      write.csv(data, file=paste(filename,".csv",sep=""),quote=TRUE, row.names=FALSE)
      save(data, file=paste(filename,".RData",sep=""))
    }
    dialogSaveData$destroy()
  })
  ExportPngItem <- gtkMenuItem("Export PNG...")
  ExportPngItem$AddCallback("activate", function(w, cmd) {
    cat("\nExporting graphic as PNG...\n")
    dialogPNG <- gtkFileChooserDialog("Export as PNG", NULL, "save",
                                      "gtk-cancel", GtkResponseType["cancel"], 
                                      "gtk-save", GtkResponseType["accept"], 
                                      show=FALSE)
    if (dialogPNG$run() == GtkResponseType["accept"]) {
      filename <- dialogPNG$getFilename()
      png(file=paste(filename,".png", sep=""), width=width.png, height=height.png)
      plotIT(xlim=xlim1, ylim=ylim1)
      dev.off()
    }
    dialogPNG$destroy()									
  })
  ExportPDFItem <- gtkMenuItem("Export PDF...")
  ExportPDFItem$AddCallback("activate", function(w, cmd) {
    cat("\nExporting graphic as PDF...\n")
    dialogPDF <- gtkFileChooserDialog("Export as PDF", NULL, "save",
                                      "gtk-cancel", GtkResponseType["cancel"], 
                                      "gtk-save", GtkResponseType["accept"], 
                                      show=FALSE)
    if (dialogPDF$run() == GtkResponseType["accept"]) {
      filename <- dialogPDF$getFilename()
      #  Add .pdf extension if missing....
      if (!(grepl(".pdf", filename) | grepl(".PDF",filename)))
        filename <- paste(filename,".pdf",sep="")
      pdf(file=filename, width=width.pdf, height=height.pdf)
      plotIT(xlim=xlim1, ylim=ylim1)
      dev.off()
    }
    dialogPDF$destroy()
  })
  QuitItem <- gtkMenuItem("Quit")
  QuitItem$AddCallback("activate", function(w, cmd) {
    graphics.off()
    Top$Destroy()
  })
  OpenItem$AddCallback("activate", function(w, cmd) {
    cat("\nOpening segments...\n")
    dialogOpen <- gtkFileChooserDialog("Open Segment Data", NULL, "open",
                                       "gtk-cancel", GtkResponseType["cancel"], 
                                       "gtk-open", GtkResponseType["accept"],
                                       show=FALSE)	
    if (dialogOpen$run() == GtkResponseType["accept"]) {
      filename <- dialogOpen$getFilename()
      #filename <- file(filename)
      cat("filename = ", filename, "\n")
      #source(file=filename, local=TRUE)	   
      load(file=filename)
    }
    dataSegments <<- dataSegments
    dataSegmentsSingle <<- dataSegmentsSingle
    print(dataSegments)
    print(dataSegmentsSingle)
    dialogOpen$destroy()
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  SaveItem$AddCallback("activate", function(w, cmd) {
    cat("\nSaving segments...\n")
    dialogSave <- gtkFileChooserDialog("Save Segment Data", NULL, "save",
                                       "gtk-cancel", GtkResponseType["cancel"], 
                                       "gtk-save", GtkResponseType["accept"], 
                                       show=FALSE)
    if (dialogSave$run() == GtkResponseType["accept"]) {
      filename <- dialogSave$getFilename()    
      #  Add .csv extension if missing....  Dont't do this.... NK 8 May 2014
      #if (!(grepl(".csv", filename) | grepl(".CSV",filename)))
      #  filename <- paste(filename,".csv",sep="")    
      #  Add .RData extension...
      filename2 <- paste(filename,".RData",sep="")
      #filename2 <- file(filename2)
      #filename <- fileEntry$GetText()
      data1 <- dataSegments
      #print(order(data$Time1))
      data1 <- data1[order(data1$Time1),]
      data1$Time1 <- format(data1$Time1, format)
      data1$Time2 <- format(data1$Time2, format)
      print(data1)
      write.csv(data1, file=paste(filename,".csv",sep=""),quote=TRUE, row.names=FALSE)
      cat("\nSaving single segments...\n")
      data2 <- dataSegmentsSingle
      #print(order(data$Time))
      data2 <- data2[order(data2$Time),]
      data2$Time <- format(data2$Time, format)
      print(data2)
      write.csv(data2, file=paste(filename,"-single.csv",sep=""), row.names=FALSE)
      #dump(c("dataSegments", "dataSegmentsSingle"), file=filename2)
      save(dataSegments, dataSegmentsSingle, file=filename2)
      #close(filename2)
    }
    dialogSave$destroy()
  })
  FileMenu$Append(OpenItem)
  FileMenu$Append(SaveItem)
  FileMenu$Append(SaveDataItem)
  FileMenu$Append(ExportPngItem)
  FileMenu$Append(ExportPDFItem)
  FileMenu$Append(QuitItem)
  
  
  
  MainBox$PackStart(MenuBar, expand = FALSE, fill = FALSE)
  #notebook <- gtkNotebookNew()
  #MainBox$PackStart(notebook, expand = TRUE, fill = TRUE)
  
  plotVBox <- gtkVBox()
  #gtkNotebookAppendPage(notebook, plotVBox, gtkLabel("Data Plot"))
  #dataVBox <- gtkVBox()
  #gtkNotebookAppendPage(notebook, dataVBox, gtkLabel("Marked Segments"))
  
  MainBox$PackStart(plotVBox, expand = TRUE, fill = TRUE, padding=0)
  
  toolBarPlot1 <- gtkHBox()
  #toolBarPlot1$SetLayout('GTK_BUTTONBOX_START')
  toolBarPlot1$SetSpacing(10)
  plotVBox$PackStart(toolBarPlot1, expand=FALSE, fill=FALSE, padding=0)
  
  toolBarPlot2 <- gtkHBox()
  #toolBarPlot2$SetLayout('GTK_BUTTONBOX_START')
  #toolBarPlot2$SetHomogeneous(0)
  toolBarPlot2$SetSpacing(10)
  plotVBox$PackStart(toolBarPlot2, expand=FALSE, fill=FALSE, padding=0)
  
  toolBarPlot3 <- gtkHBox()
  #toolBarPlot3$SetLayout('GTK_BUTTONBOX_START')
  toolBarPlot3$SetSpacing(10)
  plotVBox$PackStart(toolBarPlot3, expand=FALSE, fill=FALSE, padding=0)
  
  toolBarPlot4 <- gtkHBox()
  #toolBarPlot4$SetLayout('GTK_BUTTONBOX_START')
  toolBarPlot4$SetSpacing(10)
  plotVBox$PackStart(toolBarPlot4, expand=FALSE, fill=FALSE, padding=0)
  
  toolBarPlot5 <- gtkHBox()
  #toolBarPlot5$SetLayout('GTK_BUTTONBOX_START')
  toolBarPlot5$SetSpacing(10)
  plotVBox$PackStart(toolBarPlot5, expand=FALSE, fill=FALSE, padding=0)
  
  
  
  
  
  
  #  TOOL BAR 1
  
  markButton <- gtkButton()
  gtkButtonSetLabel(markButton, "Mark New Segment")
  toolBarPlot1$PackStart(markButton, expand=FALSE, fill=FALSE, padding=0)
  #gtkWidgetModifyBg(markButton, "normal", "blue")  
  
  segmentEntry <- gtkEntry()
  segmentEntry$SetText("Segment Name Goes Here")
  toolBarPlot1$PackStart(segmentEntry, expand=FALSE, fill=FALSE, padding=0)
  gtkEntrySetWidthChars(segmentEntry, 25)
  
  markButton$AddCallback("clicked", function(w, cmd) {
    point1 <- locator(1)
    abline(v=point1$x, col=col.border)
    x1Label$SetText(secs2Time(point1$x))	
    
    if (autoEndButton$GetActive()) {
      cat("Automatically assigning end time....\n")
      endtime <- seq.POSIXt(secs2Time(point1$x,string=FALSE),
                            by=plot.interval, length=2)[2]
      point2 <- list(x=endtime)
    } else {
      point2 <- locator(1)
    }
    
    abline(v=point2$x, col=col.border)
    x2Label$SetText(secs2Time(point2$x))
    
    xs <- sort(c(point1$x, point2$x))   # Just in case later time is clicked first, we sort
    
    #  Show some basic stats, interval, mean, max, min, sd
    
    dif <- as.numeric(difftime(secs2Time(xs[2], string=FALSE),
                               secs2Time(xs[1], string=FALSE), tz=tz, units=units))
    #print(formatC(dif, format="d"))
    diffLabel$SetText(formatC(dif, format="f", digits=1))
    idx <- datetime > xs[1] & datetime < xs[2]
    sub <- values[idx]
    mn <- mean(sub, na.rm=TRUE)
    mx <- max(sub, na.rm=TRUE)
    meanLabel$SetText(formatC(mn, format="f", digits=1))
    maxLabel$SetText(formatC(mx, format="f", digits=1))
    
    #temp <- sample(1:20)
    
    currentnum <- 1
    if (NROW(dataSegments) > 0) currentnum <- max(dataSegments$ID) + 1	
    
    col <- sample(pal, 1);	
    ang <- sample(-75:75, 1)
    
    y1 <- par()$usr[3]
    y2 <- par()$usr[4]
    if (par("ylog")) {
      y1 <- 10^y1
      y2 <- 10^y2
    }
    
    fill2 <- fill
    if (fillButton$GetActive()) fill2 <- TRUE
    
    if (par("ylog") | fill2)
      rect(point1$x, y1, point2$x, y2,
           col=rgb(t(col2rgb(col))/255, alpha=alphaSpin$GetValue()), border=col.border)
    else
      rect(point1$x, y1, point2$x, y2,
           col=col, density=densitySpin$GetValue(), angle=ang, border=col.border)
    
    label <- segmentEntry$GetText()
    text(xs[1], y2, label=label, adj=c(1.05,1.05), col=col.segment.lab, xpd=TRUE,
         srt=90)
    text(xs[1], y2, label=currentnum, adj=c(-0.5,-0.5), col=col.lab, xpd=TRUE)		
    
    
    if (modelButton$GetActive()) {
      fit.model <<- TRUE
    } else {
      fit.model <<- FALSE
    }
    
    #  Fit model if requested
    
    p <- c(NA,NA)
    # note:  if we want to fit a generic nl function, should set the origin
    #    to be at the beginning of the time segment so we can set the starting value
    #     for the algorithm more easily....
    # Even with the linear fit, we transform the x-axis so the origin is at the start of the
    #		segment.  Then we plot the A*exp(-Bx) function to see how well the exponential
    #      model actually fits the data (rather than use the "fitted" function for lm)...
    if (fit.model) {
      try({
        df <- subset(data, idx)
        first <- as.numeric(df$datetime[1])
        df$datetime.orig <- df$datetime
        df$datetime <- as.numeric(df$datetime - first)
        df$values <- log(df$values)
        cat("Fitting model...\n")
        #print(model)
        #start <- as.numeric(unlist(strsplit(startEntry$GetText()," ")))
        #names(start) <- startNames		
        #cat("Starting parameters:\n"); print(start)
        #m <- nls(model, data=df, start=start, trace=TRUE)
        #p <- coef(m)
        m <- lm(values ~ datetime, data=df, na.action=na.exclude)
        p <- m$coefficients
        cat("Results...\n")
        print(m)
        #df$fitted <- exp(fitted(m))
        df$fitted <- exp(p[1])*exp(p[2]*df$datetime)
        #print(head(df))
        #data$fitted[idx] <<- exp(fitted(m))
        #data$fitted[idx] <<- exp(p[1])*exp(-p[2]*as.numeric(data$datetime[idx]))
        if (!"fitted" %in% names(data)) data$fitted <<- NA	# initialize fitted values			
        data$fitted[idx] <<- df$fitted
        decayLabel$SetText(formatC(p[2], format="e", digits=3))
        points(df$datetime.orig, df$fitted, col="white", lwd=2.1, type="l",
               lty="dashed")
      })
    }
    
    currentSegment <- data.frame(ID=currentnum, Time1=secs2Time(xs[1], string=FALSE),
                                 Time2=secs2Time(xs[2], string=FALSE),
                                 SegmentLabel=label, Color=col, Angle=ang, Diff.mins=dif, Max=mx, Mean=mn,
                                 Decay=p[2])
    currentSegmentSingle1 <- data.frame(Time=secs2Time(xs[1], string=FALSE),
                                        SegmentLabel=label)
    currentSegmentSingle2 <- data.frame(Time=secs2Time(xs[2], string=FALSE),
                                        SegmentLabel=paste(label,"End",sep="-"))
    
    
    cat("\nGot new segment\n")
    print(currentSegment)
    dataSegments <<- rbind(dataSegments, currentSegment)
    dataSegmentsSingle <<- rbind(dataSegmentsSingle,currentSegmentSingle1,
                                 currentSegmentSingle2)
    
    #  Here is where we should update the factor and data for saving later....
  })
  
  
  autoEndButton <- gtkCheckButton()
  gtkButtonSetLabel(autoEndButton, "Auto End")
  autoEndButton$SetActive(0)
  toolBarPlot1$PackStart(autoEndButton, expand=FALSE, fill=FALSE, padding=0)
  
  clearLastSegmentButton <- gtkButton()
  gtkButtonSetLabel(clearLastSegmentButton, "Clear Last")
  toolBarPlot1$PackStart(clearLastSegmentButton, expand=FALSE, fill=FALSE, padding=0)
  
  clearLastSegmentButton$AddCallback("clicked", function(w, cmd) {
    cat("\nClearing last segment...\n")
    dataSegments <<- dataSegments[-NROW(dataSegments),]
    dataSegmentsSingle <<- dataSegmentsSingle[-NROW(dataSegmentsSingle),]
    dataSegmentsSingle <<- dataSegmentsSingle[-NROW(dataSegmentsSingle),]	
    #if (fit.model) data$fitted <<- NA
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  
  clearSegmentsButton <- gtkButton()
  gtkButtonSetLabel(clearSegmentsButton, "Clear All")
  toolBarPlot1$PackStart(clearSegmentsButton, expand=FALSE, fill=FALSE, padding=0)
  
  clearSegmentsButton$AddCallback("clicked", function(w, cmd) {
    cat("\nClearing segments...\n")
    dataSegments <<- data.frame()
    dataSegmentsSingle <<- data.frame()
    if (fit.model) data$fitted <<- NA
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  
  logButton <- gtkCheckButton()
  gtkButtonSetLabel(logButton, "Log")
  if (ylog) logButton$SetActive(1) else logButton$SetActive(0)
  toolBarPlot1$PackStart(logButton, expand=FALSE, fill=FALSE, padding=0)
  gSignalConnect(logButton, "clicked", function(w, ev) {
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  modelButton <- gtkCheckButton()
  gtkButtonSetLabel(modelButton, "Fit Model")
  if (fit.model) modelButton$SetActive(1) else modelButton$SetActive(0)
  toolBarPlot1$PackStart(modelButton, expand=FALSE, fill=FALSE, padding=0)
  
  fillButton <- gtkCheckButton()
  gtkButtonSetLabel(fillButton, "Solid")
  if (fill) fillButton$SetActive(1) else fillButton$SetActive(0)
  toolBarPlot1$PackStart(fillButton, expand=FALSE, fill=FALSE, padding=0)
  gSignalConnect(fillButton, "clicked", function(w, ev) {
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  alphaSpin <- gtkSpinButton(min=0, max=1, step=0.1)
  alphaSpin$SetWidthChars(3)
  alphaSpin$SetValue(alpha)
  toolBarPlot1$PackStart(alphaSpin, expand=FALSE, fill=FALSE, padding=0)
  gSignalConnect(alphaSpin, "value-changed", function(w, ev) {
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  densitySpin <- gtkSpinButton(min=1, max=50, step=1)
  densitySpin$SetWidthChars(2)
  densitySpin$SetValue(density)
  toolBarPlot1$PackStart(densitySpin, expand=FALSE, fill=FALSE, padding=0)
  gSignalConnect(densitySpin, "value-changed", function(w, ev) {
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  #  Set background color to light gray
  
  #   Need to add the container box to an "event box" to be able to change
  #    the background color.     8-May-2014.
  
  #gtkWidgetSetStyle(toolBarPlot1, gtk"gray30")
  
  
  
  #toolBarPlot$PackStart(gtkVSeparator(), expand=FALSE)
  
  #toolBarData <- gtkHButtonBox()
  #dataVBox$PackStart(toolBarData, expand=FALSE, fill=FALSE)
  #saveButton <- gtkButton()
  #gtkButtonSetLabel(saveButton, "Save Segments")
  #toolBarPlot1$PackStart(saveButton, expand=FALSE, fill=FALSE, padding=0)
  #toolBarPlot$PackStart(saveButton)
  
  
  #  TOOL BAR 2
  
  x1LabLab <- gtkLabel()
  x1LabLab$SetText("Start:")
  x1Label <- gtkLabel()
  
  x2LabLab <- gtkLabel()
  x2LabLab$SetText("End:")
  x2Label <- gtkLabel()
  
  diffLabLab <- gtkLabel()
  diffLabLab$SetText(paste("Diff (",units,"):", sep=""))
  diffLabel <- gtkLabel()
  #intervalLabel$SetText("Time Diff Here")
  #intervalLabel$SetWidthChars(6)
  
  meanLabLab <- gtkLabel()
  meanLabLab$SetText("Mean:")
  meanLabel <- gtkLabel()
  meanLabel$SetWidthChars(6)
  
  maxLabLab <- gtkLabel()
  maxLabLab$SetText("Max:")
  maxLabel <- gtkLabel()
  maxLabel$SetWidthChars(6)
  
  decayLabLab <- gtkLabel()
  decayLabLab$SetText("Decay (1/s):")
  decayLabel <- gtkLabel()
  #decayLabel$SetWidthChars(6)
  
  
  toolBarPlot2$PackStart(x1LabLab, expand=FALSE, fill=FALSE, padding=0)
  toolBarPlot2$PackStart(x1Label, expand=FALSE, fill=FALSE, padding=0)
  #toolBarPlot2$PackStart(gtkVSeparator(), expand=FALSE, fill=FALSE, padding=0)
  toolBarPlot2$PackStart(x2LabLab, expand=FALSE, fill=FALSE, padding=0)
  toolBarPlot2$PackStart(x2Label, expand=FALSE, fill=FALSE, padding=0)
  toolBarPlot2$PackStart(diffLabLab, expand=FALSE, fill=FALSE, padding=0)
  toolBarPlot2$PackStart(diffLabel, expand=FALSE, fill=FALSE, padding=0)
  toolBarPlot2$PackStart(meanLabLab, expand=FALSE, fill=FALSE, padding=0)
  toolBarPlot2$PackStart(meanLabel, expand=FALSE, fill=FALSE, padding=0)
  toolBarPlot2$PackStart(maxLabLab, expand=FALSE, fill=FALSE, padding=0)
  toolBarPlot2$PackStart(maxLabel, expand=FALSE, fill=FALSE, padding=0)
  toolBarPlot2$PackStart(decayLabLab, expand=FALSE, fill=FALSE, padding=0)
  toolBarPlot2$PackStart(decayLabel, expand=FALSE, fill=FALSE, padding=0)
  
  
  
  # TOOL BAR 3
  
  YScaleButton <- gtkButton()
  gtkButtonSetLabel(YScaleButton, "Mark Y Range")
  toolBarPlot3$PackStart(YScaleButton, expand=FALSE, fill=FALSE, padding=0)
  YScaleButton$AddCallback("clicked", function(w, cmd) {
    cat("\nResetting Y Scale, click two points on the plot....\n")
    newY1 <- locator(1)
    abline(h=newY1$y, col="red")
    newY2 <- locator(1)
    abline(h=newY2$y, col="red")	
    print(c(newY1$y, newY2$y))
    ylim1 <<- range(c(newY1$y, newY2$y), na.rm=TRUE)
    plotIT(ylim=ylim1, xlim=xlim1)
  })
  YRefreshButton <- gtkButton()
  gtkButtonSetLabel(YRefreshButton, "Refresh Y")
  toolBarPlot3$PackStart(YRefreshButton, expand=FALSE, fill=FALSE, padding=0)
  YRefreshButton$AddCallback("clicked", function(w, cmd) {
    cat("\nRefeshing Y Scale....\n")
    ylim1 <<- ylim0
    plotIT(ylim=ylim0, xlim=xlim1)
  })
  
  XRangeButton <- gtkButton()
  gtkButtonSetLabel(XRangeButton, "Mark X Range")
  toolBarPlot3$PackStart(XRangeButton, expand=FALSE, fill=FALSE, padding=0)
  XRangeButton$AddCallback("clicked", function(w, cmd) {
    cat("\nResetting X Scale, click two points on the plot....\n")
    newX1 <- locator(1)
    abline(v=newX1$x, col="red")
    newX2 <- locator(1)
    abline(v=newX2$x, col="red")	
    print(c(newX1$x,newX2$x))
    xlim1 <<- range(c(newX1$x, newX2$x), na.rm=TRUE)
    plotIT(ylim=ylim1, xlim=xlim1)
  })
  XRefreshButton <- gtkButton()
  gtkButtonSetLabel(XRefreshButton, "Refresh X")
  toolBarPlot3$PackStart(XRefreshButton, expand=FALSE, fill=FALSE, padding=0)
  XRefreshButton$AddCallback("clicked", function(w, cmd) {
    cat("\nRefeshing X Scale....\n")
    xlim1 <<- xlim0
    plotIT(xlim=xlim0, ylim=ylim1)
  })
  
  plotRefreshButton <- gtkButton()
  gtkButtonSetLabel(plotRefreshButton, "Refresh Plot")
  toolBarPlot3$PackStart(plotRefreshButton, expand=FALSE, fill=FALSE)
  plotRefreshButton$AddCallback("clicked", function(w, cmd) {
    cat("\nRefreshing plot....\n")
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  leftButton <- gtkButton()
  gtkButtonSetLabel(leftButton, "<<")
  toolBarPlot3$PackStart(leftButton, expand=FALSE, fill=FALSE)
  leftButton$AddCallback("clicked", function(w, cmd) {
    cat("\nScrolling left....\n")
    delta <- as.numeric(seq.POSIXt(secs2Time(xlim1[1],string=FALSE),
                                   by=plot.interval, length=2))
    delta <- diff(delta)
    cat("Delta:\n", delta)
    xlim1[1] <<- xlim1[1] - delta
    xlim1[2] <<- xlim1[2] - delta	
    print(xlim1)	
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  rightButton <- gtkButton()
  gtkButtonSetLabel(rightButton, ">>")
  toolBarPlot3$PackStart(rightButton, expand=FALSE, fill=FALSE)
  rightButton$AddCallback("clicked", function(w, cmd) {
    cat("\nScrolling right....\n")
    delta <- as.numeric(seq.POSIXt(secs2Time(xlim1[1],string=FALSE),
                                   by=plot.interval, length=2))
    delta <- diff(delta)
    cat("Delta:\n", delta)
    xlim1[1] <<- xlim1[1] + delta
    xlim1[2] <<- xlim1[2] + delta	
    print(xlim1)	
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  zoomOutButton <- gtkButton()
  gtkButtonSetLabel(zoomOutButton, "^")
  toolBarPlot3$PackStart(zoomOutButton, expand=FALSE, fill=FALSE)
  zoomOutButton$AddCallback("clicked", function(w, cmd) {
    cat("\nZooming Out....\n")
    delta <- as.numeric(seq.POSIXt(secs2Time(xlim1[1],string=FALSE),
                                   by=plot.interval, length=2))
    delta <- diff(delta)
    cat("Delta:\n", delta)
    xlim1[1] <<- xlim1[1] - delta
    xlim1[2] <<- xlim1[2] + delta	
    print(xlim1)	
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  zoomInButton <- gtkButton()
  gtkButtonSetLabel(zoomInButton, "v")
  toolBarPlot3$PackStart(zoomInButton, expand=FALSE, fill=FALSE)
  zoomInButton$AddCallback("clicked", function(w, cmd) {
    cat("\nZooming In....\n")
    delta <- as.numeric(seq.POSIXt(secs2Time(xlim1[1],string=FALSE),
                                   by=plot.interval, length=2))
    delta <- diff(delta)
    cat("Delta:", delta,"\n")
    print(xlim1)
    print(attributes(xlim1))
    check <- 0.5*abs(as.numeric(difftime(secs2Time(xlim1[1], string=FALSE),
                                         secs2Time(xlim1[2],string=FALSE), unit="secs")))
    print(check)
    if (delta < check) {
      xlim1[1] <<- xlim1[1] + delta
      xlim1[2] <<- xlim1[2] - delta	
      print(xlim1)	
      plotIT(xlim=xlim1, ylim=ylim1)
    } else {
      cat("Delta exceeds 0.5*x-range.\n")
    }
  })
  
  
  
  # TOOL BAR 4
  
  mouseTimeLabLab <- gtkLabel()
  mouseTimeLabLab$SetText("Mouse Time:")
  mouseTimeLabel <- gtkLabel()
  mouseTimeLabel$SetText("")
  mouseLevelLabLab <- gtkLabel()
  mouseLevelLabLab$SetText("Mouse Level:")
  mouseLevelLabel <- gtkLabel()
  mouseLevelLabel$SetText("")
  toolBarPlot4$PackStart(mouseTimeLabLab, expand=FALSE, fill=FALSE)
  toolBarPlot4$PackStart(mouseTimeLabel, expand=FALSE, fill=FALSE)
  toolBarPlot4$PackStart(mouseLevelLabLab, expand=FALSE, fill=FALSE)
  toolBarPlot4$PackStart(mouseLevelLabel, expand=FALSE, fill=FALSE)
  
  
  
  #Tool bar 5
  
  gridButton <- gtkCheckButton()
  gtkButtonSetLabel(gridButton, "Gridlines")
  gridButton$SetActive(0)
  if (!is.na(tck))
    if (tck == 1) gridButton$SetActive(1)
  toolBarPlot5$PackStart(gridButton, expand=FALSE, fill=FALSE, padding=0)
  gSignalConnect(gridButton, "clicked", function(w, ev) {
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  pointsButton <- gtkCheckButton()
  gtkButtonSetLabel(pointsButton, "Show Points")
  pointsButton$SetActive(0)
  if (type=="p") pointsButton$SetActive(1)
  toolBarPlot5$PackStart(pointsButton, expand=FALSE, fill=FALSE, padding=0)
  gSignalConnect(pointsButton, "clicked", function(w, ev) {
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  pointSizeSpin <- gtkSpinButton(min=0.1, max=10, step=0.05)
  pointSizeSpin$SetWidthChars(4)
  #gtkSpinSetLabel(pointSizeSpin, "Pointsize")
  #pointSizeSpin$SetRange(0.1, 10)
  #pointSizeSpin$SetIncrements(0.05, 0.1)
  pointSizeSpin$SetValue(cex)
  toolBarPlot5$PackStart(pointSizeSpin, expand=FALSE, fill=FALSE, padding=0)
  gSignalConnect(pointSizeSpin, "value-changed", function(w, ev) {
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  
  
  
  intervalLabel <- gtkLabel()
  intervalLabel$SetText("Time Interval: ")
  intervalSpin <- gtkSpinButton(min=1, max=100000, step=1)
  intervalSpin$SetValue(plot.tic)
  intervalSpin$SetWidthChars(2)
  intervalComboBox <- gtkComboBoxNewText()
  #intervalComboBox$SetWidthChars(5)
  un <- c("sec","min","hour","day","week","month","year")
  idx <- which(plot.units %in% un)
  if (!length(idx) == 0) idx <- 1
  intervalComboBox$InsertText(0, "sec")
  intervalComboBox$InsertText(1, "min")
  intervalComboBox$InsertText(2, "hour")
  intervalComboBox$InsertText(3, "day")
  intervalComboBox$InsertText(4, "week")
  intervalComboBox$InsertText(5, "month")
  intervalComboBox$InsertText(6, "year")
  intervalComboBox$SetActive(idx + 1)
  intervalComboBox$show()
  
  formatEntry <- gtkEntry()
  formatEntry$SetText(plot.format)
  formatEntry$SetWidthChars(15)
  
  toolBarPlot5$PackStart(intervalLabel, expand=FALSE, fill=FALSE)
  toolBarPlot5$PackStart(intervalSpin, expand=FALSE, fill=FALSE)
  toolBarPlot5$PackStart(intervalComboBox, expand=FALSE, fill=FALSE)
  toolBarPlot5$PackStart(formatEntry, expand=FALSE, fill=FALSE)
  gSignalConnect(intervalSpin, "value-changed", function(w, ev) {
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  gSignalConnect(intervalComboBox, "changed", function(w, ev) {
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  # gSignalConnect(intervalEntry, "value-changed", function(w, ev) {
  #	press RET and update the plot
  # if (ev$keyval == 65293) {
  # cat("\nRefreshing plot....\n")
  # plotIT(xlim=xlim1, ylim=ylim1)
  # }
  # })
  
  thresholdEntry <- gtkEntry()
  thresholdEntry$SetText(threshold)
  thresholdEntry$SetWidthChars(4)
  thresholdButton <- gtkCheckButton()
  gtkButtonSetLabel(thresholdButton, "Threshold")
  if (!is.null(threshold)) thresholdButton$SetActive(1)
  else thresholdButton$SetActive(0)
  toolBarPlot5$PackStart(thresholdButton, expand=FALSE, fill=FALSE)
  toolBarPlot5$PackStart(thresholdEntry, expand=FALSE, fill=FALSE)
  gSignalConnect(thresholdButton, "clicked", function(w, ev) {
    plotIT(xlim=xlim1, ylim=ylim1)
  })
  
  
  
  #   ---------- PLOTTING AREA -----------------
  
  
  plotArea <- gtkDrawingArea()
  gtkWidgetSetSizeRequest(plotArea, 300, 400)
  #  Receive mouse events....
  gtkWidgetAddEvents(plotArea, 2)
  asCairoDevice(plotArea)
  gtkWidgetShow(plotArea)
  #  pixil rgb
  #gr <- c( 0, "0x0011", "0x0000", "0x0011" )
  #plotArea$modifyBg("normal", gdkColorParse("gray60")$color)
  #plotArea$modifyBg("prelight", gdkColorParse("gray20")$color)
  plotVBox$PackStart(plotArea, expand=TRUE, fill=TRUE)
  plotArea$AddCallback("motion-notify-event", function(w,ev) {
    x <- ev$x
    y <- ev$y
    if (debug) {
      print(ev)
      cat("X=",x, "  Y=",y,"\n")
    }
    
    #cr$moveTo(x,y)		
    #cr$showText(paste(x,y, sep=", "))
    #layout <- pangoCairoCreateLayout(cr)		
    #layout$setText(paste(x,y, sep=", "))		
    #pangoCairoShowLayout(cr, layout)
    #cr$restore()
    coor <- win2plot(x, y)
    if (debug) {
      cat("winX=",coor[1], "  winY=",coor[2],"\n")
    }
    mouseTimeLabel$SetText(secs2Time(coor[1]))
    mouseLevelLabel$SetText(coor[2])		
    #points(coor[1], coor[2], col="blue", pch=16, cex=0.3)
  })
  
  
  #  DATA VIEW  (not currently used)
  
  #dataView <- gtkTextView()
  #dataVBox$PackStart(dataView, expand=TRUE, fill=TRUE)
  #dataBuffer <- gtkTextViewGetBuffer(dataView)
  
  #gtkTextBufferSetText(dataBuffer, "This is example text\n", -1)
  #dataIter <- gtkTextBufferGetEndIter(dataBuffer)
  #gtkTextBufferInsert(dataBuffer, dataIter$iter, "This is more example text\n", -1)
  
  Top$Show()
  
  #  Tried to draw text on plotArea using cairo but couldn't get it to work
  #    Maybe try passing a "cairo context" to asCairoDevice instead of
  #	     passing a drawing area (drawable).....
  #cairoWidth <- plotArea$Allocation$width
  #cairoHeight <- plotArea$Allocation$height
  #device.radius <- min(width, height) / 2.
  #cr <- gdkCairoCreate(plotArea$window)
  #cr$moveTo(10,10)		
  #cr$showText("some text here")
  
  
  
  plotIT(ylim0, xlim0)
  
  
}
