panel.lpplot <-
function (x, subscripts, groups=NULL, model=TRUE, show.inside.key=TRUE, cex.key=0.8,
          columns.key=2, between.key=1, between.columns=0, 
          col = superpose.symbol$col,...) 
{

  superpose.symbol <- trellis.par.get("superpose.symbol")
  ###  UPDATE:  just use basic  qqmath panel functions with
  ###    adding grids, model, etc.
  panel.qqmath(x, ...)
  if (model) panel.qqmathline(x, ...)
  
  #  here we just supply the grid...
  limits <- current.panel.limits()
  ylim <- limits$ylim    
  ytics  <- log.tics(10^limits$ylim, exact10=FALSE)
  majtics <- ytics$major.tics
  mintics <- ytics$minor.tics      
  panel.abline(h = log10(majtics),lwd=1, col="lightgray")   # major
  panel.abline(h = log10(mintics), lwd=1, col="lightgray")  # minor
  
  xlim <- limits$xlim
  all.tics <- qnorm(c(0.0000001, 0.000001, 0.00001, 0.0001,0.001,0.01,
                      0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,0.999,
                      0.9999,0.99999, 0.999999, 0.9999999))
  xtics <- all.tics[all.tics >= xlim[1] & all.tics <= xlim[2]]	       
  panel.abline(v=xtics, lwd=1, col="lightgray")
  # if (!is.null(groups)) {
  #   subgr <- groups[subscripts]
  #   vals <- sort(unique(subgr))
  #   #print(vals)
  #   if (show.inside.key) {
  #     panel.key2(text=vals,
  #              rectangles=list(col=col[1:length(vals)], size=0.5, height=0.5),
  #              cex=cex.key, columns=columns.key,
  #              between=between.key, between.columns=between.columns)
  #   }
  # }
  
}