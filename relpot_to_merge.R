commatFct <- function(object, compMatch)
{
    parmMat <- object$parmMat

    if (!is.null(compMatch))
    {
        return(parmMat[, (colnames(parmMat) %in% c(compMatch[1], compMatch[2])), drop = FALSE ])
    } else {
        parmMat
    }
}

stewart_relpot <- function(object, plotit = TRUE, compMatch = NULL, percVec = NULL, interval = "none", 
type = c("relative", "absolute"), scale = c("original", "percent", "unconstrained"), ggcolor = NULL, ...)
{
    scale <- match.arg(scale)
    type <- match.arg(type)

#    ## Checking arguments
#    if (length(compMatch) != 2)
#    {
#        stop("Argument 'compMatch' should have length 2")
#    }

    ## Defining range for 'percVec' 
    parmMat <- commatFct(object, compMatch)    
    lowerVec <- apply(parmMat, 2, object$"fct"$"lowerAs")
    upperVec <- apply(parmMat, 2, object$"fct"$"upperAs")
    maxLow <- max(lowerVec)
    minUp <- min(upperVec)
    
    if ( (type == "absolute") && (is.null(percVec)) )
    {
        percVec <- seq(maxLow*1.05, minUp*0.95, length.out = 1000)      
    }
    if ( (type == "relative") && (is.null(percVec)) )
    {
        uMin <- max( (maxLow - lowerVec) / (upperVec - lowerVec) )
        uMax <- min( (minUp - lowerVec) / (upperVec - lowerVec) )  
        if (object$"fct"$"monoton"(parmMat[, 1]) < 0)
        {
            uTemp <- uMin
            uMin <- 1 - uMax
            uMax <- 1 - uTemp
        }  
        percVec <- 100 * (seq(uMin, uMax, length.out = 101))[-c(1, 101)]
    }
    if ( (type == "relative") && (scale == "unconstrained") )
    {
        percVec <- 1:99
    } 

    lenpv <- length(percVec)
    rpVec <- rep(NA, lenpv)
    if (identical(interval, "none"))
    {
        for (i in 1:lenpv)
        {
            SIobj <- EDcomp(object, rep(percVec[i], 2), compMatch, 
                            type = type, display = FALSE)
            rpVec[i] <- SIobj[1]
        }
    } else {
        lrpVec <- rep(NA, lenpv)
        urpVec <- rep(NA, lenpv)

        for (i in 1:lenpv)
        {
            SIobj <- EDcomp(object, rep(percVec[i], 2), compMatch, interval = interval, 
                            type = type, display = FALSE)
            rpVec[i] <- SIobj[1]
            lrpVec[i] <- SIobj[2]
            urpVec[i] <- SIobj[3]            
        }
    }

    if (plotit)
    {
        if ( (type == "relative") && ((scale == "percent") || (scale == "unconstrained")) )
        {
            xlabStr <- "Relative response level (%)"
            xVec <- percVec
        }
        if ( (type == "relative") && (scale == "original") )
        {
            xlabStr <- "Response level"
            xVec <- seq(maxLow, minUp, length.out = 99)            
        }
        if (type == "absolute")
        {
            xlabStr <- "Response level"
            xVec <- percVec
        }
                
        if (!identical(interval, "none"))
        {
            #plot(xVec, rpVec, type = "l", xlab = xlabStr, ylab = "Relative potency", 
            #ylim = c(min(lrpVec), max(urpVec)), ...)
         
            #lines(xVec, lrpVec, lty = 3)
            #lines(xVec, urpVec, lty = 3)
            #print("test 1")
            ##############################
            
            tmp = ggplot(data = data.frame(xVec, rpVec), 
                         aes(x = xVec, y = rpVec)) + 
              geom_line(color = ggcolor) + theme_bw() + 
              lims(x = c(0,100), y = c(0, 2)) +
              geom_ribbon(aes(ymin = lrpVec, ymax = urpVec), color = "darkgray",
                          alpha = 0) +
              labs(x = "Relative Response Level (%)",
                   y = "Relative Potency")
            
            ###############################
            
            
        } else {
          #plot(xVec, rpVec, type = "l", xlab = xlabStr, ylab = "Relative potency", ...)
          ##########
          tmp = ggplot(data = data.frame(xVec, rpVec), 
                         aes(x = xVec, y = rpVec)) + 
              geom_line(color = ggcolor) + theme_bw() + 
              lims(x = c(0,100), y = c(0, 2)) +
              labs(x = "Relative Response Level (%)",
                   y = "Relative Potency")
          ###############
        }

        ## Adding reference line corresponding to EC50/ED50
        if (type == "relative")
        { 
            #abline(h = EDcomp(object, c(50, 50), compMatch, type = type, display = FALSE)[1], lty = 2)
          return(tmp + geom_abline(intercept = EDcomp(object, c(50, 50), 
                                                     compMatch, type = type, 
                                                     display = FALSE)[1],
                                  slope = 0,
                                  linetype = "dashed"))
          #print(EDcomp(object, c(50, 50), compMatch, type = type, display = FALSE)[1])
        }
        
    }
    invisible(list(x = xVec, y = rpVec, percVec = percVec))
}




tmp123 = stewart_relpot(test.drc.relpot.object, 
       type = "relative", 
       scale = "unconstrained",
       interval = "delta",
       col = product_graph_colors[1],
       ggcolor = product_graph_colors[1])
