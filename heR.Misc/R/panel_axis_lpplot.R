panel.axis.lpplot <-    
    
    #   Based on axis.default
    #  makes custom axes for the log-probability plot.  NK 20-Nov-2015
  
    function (side = c("top", "bottom", "left", "right"), scales, 
              components, as.table, labels = c("default", "yes", "no"), 
              ticks = c("default", "yes", "no"), ...,
              prefix = lattice.getStatus("current.prefix")) 
    {
      # ----- Create ticks and label values... ---------
      limits <- current.panel.limits()
      ytics  <- log.tics(10^limits$ylim, exact10=FALSE)
      majtics <- ytics$major.tics
      mintics <- ytics$minor.tics   
      xlim <- limits$xlim
      allxtics <- qnorm(c(0.0000001, 0.000001, 0.00001, 0.0001,0.001,0.01,
                          0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,0.999,
                          0.9999,0.99999, 0.999999, 0.9999999))
      allxnames <- c(0.00001, 0.0001,0.001,0.01,0.1,1,5,10,25,50,75,90,95,99,99.9,
                     99.99,99.999,99.9999, 99.99999)
      xtics <- allxtics[allxtics >= xlim[1] & allxtics <= xlim[2]]	       
      xnames <- allxnames[allxtics >= xlim[1] & allxtics <= xlim[2]]  
      
      # ------------------------------------
      
      #print(components)
      
      if (exists("bottom", where=components)) {
        components$bottom$labels$at <- allxtics
        components$bottom$ticks$at <- allxtics        
        components$bottom$labels$labels <- allxnames
      }

      if (exists("left", where=components)) {
         components$left$labels$at <- log10(majtics)
         components$left$ticks$at <- log10(majtics)
         components$left$labels$labels <- majtics
      }
      
      if (exists("top$tick", where=components)) {
        components$top$labels$at <- allxtics
        components$top$ticks$at <- allxtics        
        components$top$labels$labels <- allxnames
      }
      
       if (exists("right$tick", where=components)) {
         components$right$labels$at <- log10(majtics)
         components$right$ticks$at <- log10(majtics)
         components$right$labels$labels <- majtics
       }
      
      #print(components)
      
      lGs <- lattice:::lattice.getStatus
      # ------------------------------------
      
      side <- match.arg(side)
      labels <- match.arg(labels)
      ticks <- match.arg(ticks)
      row <- lGs("current.focus.row", prefix = prefix)
      column <- lGs("current.focus.column", prefix = prefix)
      panel.layout <- trellis.currentLayout("panel", prefix = prefix)
      layout.dim <- dim(panel.layout)
      determineStatus <- function(x) {
        if (is.null(x) || (is.logical(x) && !x)) 
          FALSE
        else TRUE
      }
      lastPanel <- function() {
        ((pn <- panel.number(prefix = prefix)) > 0 && pn == max(panel.layout))
      }
      atBoundary <- function() {
        switch(side, top = if (as.table) row == 1 else row == 
                 layout.dim[1], bottom = if (!as.table) row == 1 else row == 
                 layout.dim[1], left = column == 1, right = column == 
                 layout.dim[2] || lastPanel())
      }
      do.ticks <- switch(ticks, yes = TRUE, no = FALSE, default = scales$draw && 
                           determineStatus(components[[side]]) && (if (scales$relation == 
                          "same") atBoundary() else TRUE))
      do.labels <- switch(labels, yes = TRUE, no = FALSE, default = scales$draw && 
                            (if (scales$relation == "same") {
                             atBoundary() && switch(side, top = rep(scales$alternating, 
                             length.out = column)[column] %in% c(2, 3), bottom = rep(scales$alternating, 
                             length.out = column)[column] %in% c(1, 3), left = rep(scales$alternating, 
                             length.out = row)[row] %in% c(1, 3), right = rep(scales$alternating, 
                             length.out = row)[row] %in% c(2, 3))
                            } else TRUE))
      if (do.ticks || do.labels) {
        comp.list <- switch(side,
                            top = if (is.logical(components[["top"]]) && 
                                            components[["top"]]) components[["bottom"]] else components[["top"]], 
                            bottom = components[["bottom"]],
                            left = components[["left"]], 
                            right = if (is.logical(components[["right"]]) && 
                                        components[["right"]]) components[["left"]] else components[["right"]])
        scales.tck <- switch(side, left = , bottom = scales$tck[1], 
                             right = , top = scales$tck[2])
        if (!is.logical(comp.list)) {
          if (do.ticks) {
            panel.axis(side = side, at = comp.list$ticks$at, 
                       labels = FALSE, draw.labels = FALSE, check.overlap = FALSE, 
                       outside = TRUE, ticks = TRUE, tck = scales.tck * 
                         comp.list$ticks$tck, ...)
          }
          if (do.labels) 
            panel.axis(side = side, at = comp.list$labels$at, 
                       labels = comp.list$labels$labels, draw.labels = TRUE, 
                       check.overlap = comp.list$labels$check.overlap, 
                       outside = TRUE, ticks = FALSE, tck = scales.tck * 
                         comp.list$ticks$tck, ...)
        }
      }
    }
