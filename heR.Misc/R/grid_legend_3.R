grid.legend.3<-
function (pch, col, labels, frame = TRUE, hgap = unit(0.5, "lines"), 
    vgap = unit(0.5, "lines"), default.units = "lines", gp = gpar(), 
    draw = TRUE, vp = NULL, just=c("right","top")) 
{
    labels <- as.character(labels)
    nkeys <- length(labels)
    if (length(pch) != nkeys) 
        stop("'pch' and 'labels' not the same length")
    if (!is.unit(hgap)) 
        hgap <- unit(hgap, default.units)
    if (length(hgap) != 1) 
        stop("'hgap' must be single unit")
    if (!is.unit(vgap)) 
        vgap <- unit(vgap, default.units)
    if (length(vgap) != 1) 
        stop("'vgap' must be single unit")
    legend.layout <- grid.layout(nkeys, 3, widths = unit.c(unit(2, 
        "lines"), max(unit(rep(1, nkeys), "strwidth", as.list(labels))), 
        hgap), heights = unit.pmax(unit(1, "lines"), vgap+unit(rep(1, 
        nkeys), "strheight", as.list(labels))), just=just)
    fg <- frameGrob(layout = legend.layout, vp = vp, gp = gp)
    for (i in 1L:nkeys) {
        fg <- placeGrob(fg, pointsGrob(0.5, 0.5, pch = pch[i],
					gp=gpar(col=col[i], cex=0.8)), 
            col = 1, row = i)  
        fg <- placeGrob(fg, textGrob(labels[i], x = 0, y = 0.5, 
            just = c("left", "centre"), gp=gpar(cex=0.8)), col = 2, row = i)
    }
    if (draw) 
        grid.draw(fg)
    fg
}