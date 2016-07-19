panel.lsfit<-
function (x, y, intercept=TRUE, pch = plot.symbol$pch, col,
    col.line = plot.line$col,
    col.symbol = plot.symbol$col, font = plot.symbol$font,
    fontfamily = plot.symbol$fontfamily,
    fontface = plot.symbol$fontface, lty = plot.line$lty,
    cex = plot.symbol$cex,
    lwd = plot.line$lwd, horizontal = FALSE, right=TRUE, top=TRUE,
    usr = NULL, text.line=1/10, cex.text = 1, ...)
{
	require(grid)
	ylim <- current.viewport()$yscale
	xlim <- current.viewport()$xscale
	if (is.null(usr)) usr <- c(xlim, ylim)
	
    x <- as.numeric(x)
    y <- as.numeric(y)
#    vp <- current.viewport()
#    print(vp[c("width","height")])
    if (length(x) < 1)
        return()
    if (!missing(col)) {
        if (missing(col.line))
            col.line <- col
        if (missing(col.symbol))
            col.symbol <- col
    }
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.line <- trellis.par.get("plot.line")
    lpoints(x = x, y = y, cex = cex, font = font, fontfamily = fontfamily,
            fontface = fontface, col = col.symbol, pch = pch)
    ls.out <- lsfit(x, y, intercept=intercept)
    coeff <- format(ls.out$coefficients, digits = 2)
    #require(heR.Misc)
    rsquared <- round(rsquared(ls.out), digits=3)
    panel.abline(ls.out, col = col.line, lty = lty, lwd = lwd)
    if (intercept)
      results <- c(paste("Slope = ", coeff[2]), paste("Intercept = ",
                   coeff[1]), paste("R-Squared = ", rsquared))
    else		   
      results <- c(paste("Slope = ", coeff[1]), "Intercept = 0",
                   paste("R-Squared = ", rsquared))
    
	th <- diff(usr[c(3, 4)])*text.line
				   
	if (right) {
		x1 <- usr[2]    #x1 + diff(usr[1:2])/2
		xadj <- 1.05
	} else {
	    x1 <- usr[1] #+(diff(usr[c(1, 2)]))/20
		xadj <- -0.05		
	}
	if (top) {
		y2 <- usr[4]
	    ypos <- c(y2 - th, y2 - 2 * th, y2 - 3 * th)
		yadj <- -0.05
	} else {  # bottom
	    y2 <- usr[3]
	    ypos <- c(y2 + 3*th, y2 + 2 * th, y2 + 1 * th)		
		yadj <- 1.05
	}
    ltext(rep(x1, 3), ypos,
          labels = results, adj=c(xadj,yadj), cex = cex.text)

}