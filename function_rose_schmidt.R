

function (x, bins = 36, rscale = NULL, labels = TRUE, rings = TRUE, shrink = 1,    
    ...)                                                                 
{                                                                        
    if (max(x) > 360 || min(x) < 0) {                                    
        stop("Data is out of range (0 - 360)")                           
    }                                                                    
    histogram.out <- hist(x, breaks = seq(0, 360, length = bins +        
        1), plot = FALSE)                                                
    pieMid <- histogram.out$mids                                         
    pieCount <- histogram.out$counts                                     
    pieWidth <- 360/length(pieMid)                                       
    pieFreq <- histogram.out$density * pieWidth                          
    oldpar <- par()                                                      
    par(pty = "s")                                                       
    if (!is.null(rscale)) {                                              
        rscale <- pretty(pieFreq, rscale)                                
    }                                                                    
    else {                                                               
        rscale <- pretty(pieFreq)                                        
    }                                                                    
    plotLimits <- c(-max(rscale), max(rscale)) * 1.04                    
    plot(0, 0, ylim = c(-1,1), xlim = c(-1,1), axes = FALSE,     #modificato rispetto all'originale(plot(0, 0, ylim = plotLimits, xlim = plotLimits, axes = FALSE,)
        xlab = "", ylab = "")                                            
    abline(h = 0)                                                        
    abline(v = 0)                                                        
    if (rings == TRUE) {                                                 
        symbols(rep(0, length(rscale)), rep(0, length(rscale)),          
            circles = rscale, inches = FALSE, add = TRUE)                
    }                                                                    
    pie <- function(h, k, direction, spread, magnitude, ...) {           
        direction <- 360 - direction + 90                                
        start <- (direction + 0.5 * spread) * pi/180                 
        stop <- (direction - 0.5 * spread) * pi/180                 
        x1 <- h                                                          
        y1 <- k
        x2 <- (shrink * magnitude * cos(start)) + h	#Qua ho aggiunto la variabile shrink per aumentare area degli spicchi
        y2 <- (shrink * magnitude * sin(start)) + k
        x3 <- (shrink * magnitude * cos(stop)) + h
        y3 <- (shrink * magnitude * sin(stop)) + k
        x <- c(x1, x2, x3, x1)
        y <- c(y1, y2, y3, y1)
        polygon(x, y, ...)
    }
    for (i in 1:length(pieMid)) {
        pie(0, 0, pieMid[i], pieWidth, pieFreq[i], ...)
    }
    if (labels == TRUE) {
        mtext("N", side = 3, line = 1)
        mtext("E", side = 4, line = 1)
        mtext("S", side = 1, line = 1)
        mtext("W", side = 2, line = 1)
        pie10percent <- round(length(pieMid) * 0.1) + 1
        pieRank <- length(pieCount) - rank(pieCount)
        top10 <- which(pieRank < pie10percent)
        theta <- 360 - pieMid[top10] + 90
        theta <- theta * pi/180
        x <- pieFreq[top10] * cos(theta) * shrink	#Qua ho aggiunto la variabile shrink per spostare il label
        y <- pieFreq[top10] * sin(theta) * shrink
        text(x, y, format(pieFreq[top10], digits = 2))
        par <- oldpar
        histogram.out
    }
}
