lpplot.lattice <- 
  function(x, axis=panel.axis.lpplot,
           panel=panel.lpplot, model=TRUE, ...)
{
  #  Got qqmath to work pretty much with some quirks on 
  #   axes to work out....
  #    just use custom panel.lpplot and panel.axis.lpplot
  #       could leave out panel.axis to just have z scores on bottom
  require(lattice)
  qqmath(x, scales=list(y=list(log=10)),
         panel=panel, axis=axis, model=model, ...)
  
}  