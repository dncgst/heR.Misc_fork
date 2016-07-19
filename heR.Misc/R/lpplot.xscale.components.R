lpplot.xscale.components <- 
  function(...) {
    limits <- current.panel.limits()    
    xlim <- limits$xlim
    all.tics <- qnorm(c(0.0000001, 0.000001, 0.00001, 0.0001,0.001,0.01,
                        0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,0.999,
                        0.9999,0.99999, 0.999999, 0.9999999))
    all.names <- c(0.00001, 0.0001,0.001,0.01,0.1,1,5,10,25,50,75,90,95,99,99.9,
                   99.99,99.999,99.9999, 99.99999)
    xtics <- all.tics[all.tics >= xlim[1] & all.tics <= xlim[2]]	       
    xnames <- all.names[all.tics >= xlim[1] & all.tics <= xlim[2]]
    
    ans <- xscale.components.default(...)
    #ans$top <- ans$bottom
    ans$bottom$labels$at <- all.tics
    ans$bottom$labels$labels <- all.names
    print(ans)
    ans
  }


