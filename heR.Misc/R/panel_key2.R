panel.key2 <-
    function(text, ..., cex=0.8, just=c("right","top"), x=0.75, y=0.9) 
		#corner = c(0, 1), x = corner[1], y = corner[2])
{

    # UPDATE:   convert any text factor to a character vector.  NK 19Dec2012

    #key <- simpleKey(text, ...)
    # see the latticeExtra panel.key function (this was adapted from it)
    #    and see the `key' documentation in ?xyplot
    #  "..." must be one or more specifications of graphical parameters in key docs
    require(grid)
    text[[1]] <- as.character(text[[1]])
    cat("\n\n **** Creating the key \n\n")    
    key <- list(text=text, cex=cex, just=just,
		            vp = viewport(x = unit(x, "npc"), y = unit(y, "npc")),...)
    print(key)
      #x=x, y=y, corner=corner, ...)
    key.gf <- draw.key(key, draw = FALSE)
    #vp <- viewport(x = unit(x, "npc") + unit(0.5 - corner[1], "grobwidth", list(key.gf)),
    #               y = unit(y, "npc") + unit(0.5 - corner[2], "grobheight", list(key.gf)))
    #pushViewport(vp)
    cat("\n\n  *** Assigned key \n\n")    
    print(key.gf)
    grid.draw(key.gf)
    cat("\n\n  *** Finished drawing the key \n\n")
    upViewport()
}
