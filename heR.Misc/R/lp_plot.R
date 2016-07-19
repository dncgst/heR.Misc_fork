lpplot<-
  function (xlim, ylim, grid=TRUE, exact10=FALSE,
            grid.lty="solid", grid.col="lightgray",
            main = "Log-Probability Plot", 
            xlab = "Standard Normal Cumulative Probability (%)", 
            ylab = "Sample Quantiles", format="fg",
            bty="o", axes = TRUE, cex.main=1, cex.lab=1, cex.axis=1, las=1,
            tck.minor=-0.009, tck.major=-0.02,
            xtic.minor=FALSE, ...) 
  {
    # IDEA:   Have this function plot an "x" vector...
    #    Then can add other data, or models with other function....
    
    # IDEA:   Specify "limit.resolution" as a factor
    #   -2 = round limit to the nearest 10 ^ -2 = 0.2    
    #   -1 = round limit to the nearest 10 ^ -1 = 0.1
    #    0 = round limit to 10 ^ 0 = 1
    #   1 = round limit to nearest 10 ^ 1
    #   2 = round limit to nearest 10 ^ 2
    #  No just use log.tics to compute tics iwth exact10=TRUE/FALSE
    
    # This function simply creates an empty log-probability
    # plot with nice tics and, optionally, grids.
    
    # See the 'add.data.lp', 'add.hist.lp', and 'add.lnorm.lp' R functions
    # to add curves from data, histograms, or a lognormal model,
    # respectively, to the plot.
    
    # On the x axis are normal quantiles labeled as normal probabilities
    # On the y axis are the data (sample) quantiles (corresponding to
    # the cumulative probabilities, converted to normal quantiles and
    # plotted on the x axis)
    
    # This is a type of Q-Q (quantile-quantile) plot commonly used to
    # judge how data conform to a specific model or data vector.
    
    # If the curve appears as a straight line on this plot, then
    # the data are lognormally distributed.
    
    # UPDATE:  Changed tolerance for being over an upper even power of
    #           ten limit or under a lower one to be less, so that
    #           we need to be within something like 1 in 10^4 to 
    #           trip a new upper limit to retain a lower limit.
    
    #  UPDATE:  Fixed exact10 calculation for limits equal to exactly 0.1/10
    #                          --NK 13-Mar-04
    
    # UPDATE:  Now we allow user-defined limits for the probability axis,
    #          and we check the xlim and ylim values. 
    #                                        --NK 13-Mar-04
    
    #  UPDATE:  HAve option for "exact10" vertical (Y) limits (exact power-of-ten limits)
    #    or limits for multiples of exact10 limits.  --NK 18Nov2015
    
    # ------------------------
    
    
    
    if (missing(xlim))
      xlim <- qnorm(c(0.0001, 0.9999))
    else if (!is.vector(xlim) | !is.numeric(xlim) | 
             length(xlim) != 2 | any(xlim <= 0) | any(xlim >= 1))
      stop("`xlim' must be a two-element numeric vector containing the lower and upper limits of the horizontal probablility axis, with values between 0 and 1.")
    else xlim <- qnorm(xlim)
    
    #xlim <- c(qnorm(0.0001),qnorm(0.9999))
    
    if (missing(ylim))
      ylim <- c(0.1,1000)
    else if (!is.vector(ylim) | !is.numeric(ylim) | 
             length(ylim) !=2 | ylim[1] <=0 | diff(ylim) <=0)
      stop("`ylim' must be a two-element numeric vector containing the lower and upper limit of the vertical axis with values greater than 0.")
    
    #if (ylim[1] <=0)
    #   stop("Lower y limit must be greater than 0.")
    
    #par(xaxs="i", yaxs="i")
    #par(ps=12,font=3,lwd=1,las=0)
    
    
    # By default, get closest vertical axis limit to an even power of 10.
    #y2 <- ifelse(ylim[2]<=0.1, trunc(log10(ylim[2])-0.0001), trunc(log10(ylim[2])+0.9999))
    #y1 <- ifelse(ylim[1]>=10, trunc(log10(ylim[1])), trunc(log10(ylim[1])-0.9999))
    #ylim <- c(10^y1,10^y2) 
    tics <- log.tics(ylim, exact10=exact10)
    cat("Computed Tics: \n")
    print(tics)
    ylim <- tics$tic.range
    majtics <- tics$major.tics
    mintics <- tics$minor.tics
    
    #par(usr=c(qnorm(0.0001),qnorm(0.9999),ylim[1],ylim[2]))
    
    plot.new()
    plot.window(xlim, ylim, log="y") 
    title(main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, cex.lab=cex.lab)
    
    
    # x axis tics with labels
    xtics <- if (xtic.minor) tck.minor else tck.major
    
    # enough probabilities for anyone
    all.probs <- qnorm(c(0.0000001, 0.000001, 0.00001, 0.0001,0.001,0.01,
                         0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,0.999,
                         0.9999,0.99999, 0.999999, 0.9999999))
    all.names <- c(0.00001, 0.0001,0.001,0.01,0.1,1,5,10,25,50,75,90,95,99,99.9,
                   99.99,99.999,99.9999, 99.99999)
    probs <- all.probs[all.probs >= xlim[1] & all.probs <= xlim[2]]	       
    prob.names <- all.names[all.probs >= xlim[1] & all.probs <= xlim[2]]	       
    
    # x axis grid lines 
    if (grid) 
      axis(1,probs,label=FALSE,
           lty=grid.lty, col=grid.col, tck=1, cex.axis=cex.axis, las=las, ...)
    if (axes)
      axis(1,probs, prob.names,
           lwd=1, lty=1, cex.axis=cex.axis, las=las, tck=xtics, ...)
    
    # y axis tics and grids
    if (grid) {
      axis(2,majtics,labels=FALSE, lty=grid.lty, col=grid.col, tck=1)
      axis(2,mintics,labels=FALSE, lty=grid.lty, col=grid.col, tck=1)
    }
    if (axes) {
      # major y tics
      axis(2,majtics,labels=formatC(majtics,format=format), lty=1, tck=tck.major, cex.axis=cex.axis, las=las, ...)
      # minor y tics
      axis(2,mintics,labels=FALSE, lty=1, tck=tck.minor, cex.axis=cex.axis, las=las, ...)
    }
      
#     for (i in y1:y2) {
#       if (grid) 
#         # grid lines over minor tics
#         axis(2,seq(10^i,10^(i+1),by=10^i),labels=F, lty=grid.lty, col=grid.col, tck=1)
#       if (axes) {
#         # major y tics
#         axis(2,10^i,labels=formatC(10^i,format="fg"), lty=1, tck=tck.major, cex.axis=cex.axis, las=las, ...)
#         # minor y tics
#         axis(2,seq(10^i,10^(i+1),by=10^i),labels=FALSE, lty=1, tck=tck.minor, cex.axis=cex.axis, las=las, ...)
#       }
#     }
    
    box(bty=bty)
    #par(tcl=-0.5,lty=1)
    
  }