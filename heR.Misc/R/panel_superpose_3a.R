panel.superpose.3a<-
function (x, y, subscripts, groups, col, col.line = superpose.line$col, 
    col.symbol = superpose.symbol$col, pch = superpose.symbol$pch, 
    cex = superpose.symbol$cex, lty = superpose.line$lty, 
    lwd = superpose.line$lwd, type = "p", ...) 
{

 #  Simple change to define factors only on the levels in the subgroup
 #     of values passed to each panel.  This allows for specifying
 #     colors for each panel series and making a legend for each panel.
 #           NK  19-June-2008

    if (length(x) > 0) {
        if (!missing(col)) {
            if (missing(col.line)) 
                col.line <- col
            if (missing(col.symbol)) 
                col.symbol <- col
        }
        superpose.symbol <- trellis.par.get("superpose.symbol")
        superpose.line <- trellis.par.get("superpose.line")
        x <- as.numeric(x)
        y <- as.numeric(y)
        subgr <- groups[subscripts]
        vals <- sort(unique(subgr))
        nvals <- length(vals)
        col.line <- rep(col.line, length = nvals)
        col.symbol <- rep(col.symbol, length = nvals)
        pch <- rep(pch, length = nvals)
        lty <- rep(lty, length = nvals)
        lwd <- rep(lwd, length = nvals)
        cex <- rep(cex, length = nvals)
        type <- rep(type, length = nvals)
        for (i in seq(along = vals)) {
            id <- (subgr == vals[i])
            if (any(id)) 
                panel.xyplot(x = x[id], y = y[id], pch = pch[i], 
                  cex = cex[i], col.line = col.line[i],
                  col.symbol = col.symbol[i], 
                  lty = lty[i], lwd = lwd[i], type = type[i], 
                  ...)
         }

        grid.legend.3(pch, col.symbol, vals)
    }
}