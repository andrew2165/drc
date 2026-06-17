"ggplotDrc" <-
function(object, ..., level = NULL,
         type = c("average", "all", "confidence", "none", "obs"),
         gridsize = 500, log_x_axis = FALSE, log_y_axis = FALSE,
         xlab, ylab, xlim, ylim,
         bp, conName = "0", normal = FALSE, normRef = 1,
         confidence.level = 0.95)
{
    if (!inherits(object, "drc"))
    {
        stop("Argument 'object' must be an object of class 'drc'")
    }
    ggplotDrcRequireGgplot2()

    type <- match.arg(type)
    plotData <- ggplotDrcData(object, level = level, type = type,
                              gridsize = gridsize,
                              log_x_axis = log_x_axis,
                              xlim = xlim, bp = bp, normal = normal,
                              normRef = normRef,
                              confidence.level = confidence.level)

    varNames <- ggplotDrcVariableNames(object)
    if (missing(xlab)) {xlab <- ggplotDrcDefaultLabel(varNames[["dose"]], "Dose")}
    if (missing(ylab)) {ylab <- ggplotDrcDefaultLabel(varNames[["response"]], "Response")}

    p <- ggplot2::ggplot()
    p <- ggplotDrcAddRibbon(p, plotData[["ribbon"]])
    p <- ggplotDrcAddLine(p, plotData[["line"]], type)
    p <- ggplotDrcAddPoints(p, plotData[["points"]], type)
    p <- ggplotDrcAddLabels(p, xlab, ylab, varNames[["curve"]],
                            plotData[["ribbon"]])
    p <- ggplotDrcAddGuides(p, plotData[["line"]])
    p <- ggplotDrcAddScales(p, object, plotData, log_x_axis,
                            log_y_axis, conName)

    if (!missing(ylim))
    {
        p <- p + ggplot2::coord_cartesian(ylim = ylim)
    }

    p
}


"ggplotDrcRequireGgplot2" <- function(package = "ggplot2")
{
    if (!requireNamespace(package, quietly = TRUE))
    {
        stop("Package 'ggplot2' is required for ggplotDrc(). ",
             "Please install it before using this plotting helper.",
             call. = FALSE)
    }
    invisible(TRUE)
}


"ggplotDrcData" <-
function(object, level = NULL,
         type = c("average", "all", "confidence", "none", "obs"),
         gridsize = 500, log_x_axis = FALSE, xlim, bp,
         normal = FALSE, normRef = 1, confidence.level = 0.95)
{
    type <- match.arg(type)

    dataList <- object[["dataList"]]
    dose <- ggplotDrcDose(dataList)
    ggplotDrcCheckDoseDimension(dose)

    dose <- as.vector(dose)
    resp <- dataList[["origResp"]]
    curveid <- dataList[["curveid"]]
    plotid <- dataList[["plotid"]]

    if (identical(object[["type"]], "ssd"))
    {
        ssdData <- ggplotDrcSsdData(dataList, dose, curveid)
        dose <- ssdData[["dose"]]
        resp <- ssdData[["response"]]
    }

    assayNo <- ggplotDrcAssayNo(curveid, plotid)
    uniAss <- unique(assayNo)
    level <- ggplotDrcPlotLevels(level, uniAss)

    if (normal)
    {
        resp <- ggplotDrcNormalizeResponse(resp, curveid, object, normRef)
    }

    plotFct <- object[["curve"]][[1]]
    logDose <- object[["curve"]][[2]]
    axisData <- ggplotDrcAxisData(dose, xlim, bp, log_x_axis, logDose)
    dose <- axisData[["dose"]]
    dosePts <- ggplotDrcDoseGrid(axisData[["xlim"]], gridsize,
                                 log_x_axis, logDose)

    plotMat <- ggplotDrcCurveMatrix(plotFct, logDose, dosePts, object,
                                    normal, normRef, uniAss, level)
    lineData <- ggplotDrcLineData(dosePts, plotMat, level)
    pointData <- ggplotDrcPointData(dose, resp, assayNo, level,
                                    axisData[["xlim"]], type)
    ribbonData <- NULL
    if (identical(type, "confidence"))
    {
        ribbonData <- ggplotDrcRibbonData(object, dosePts, level,
                                          confidence.level, normal, normRef)
    }

    list(line = lineData, points = pointData, ribbon = ribbonData,
         xlim = axisData[["xlim"]], conLevel = axisData[["conLevel"]],
         controlShifted = axisData[["controlShifted"]])
}


"ggplotDrcVariableNames" <- function(object)
{
    dlNames <- object[["dataList"]][["names"]]
    list(dose = dlNames[["dName"]],
         response = dlNames[["orName"]],
         curve = dlNames[["cNames"]])
}


"ggplotDrcDefaultLabel" <- function(name, fallback)
{
    if (identical(name, "")) {fallback} else {name}
}


"ggplotDrcDose" <- function(dataList)
{
    dataList[["dose"]]
}


"ggplotDrcCheckDoseDimension" <- function(dose)
{
    doseDim <- ncol(dose)
    if (is.null(doseDim))
    {
        doseDim <- 1
    }
    if (doseDim > 1)
    {
        stop("ggplotDrc does not support models with more than one dose variable")
    }
    invisible(TRUE)
}


"ggplotDrcSsdData" <- function(dataList, dose, curveid)
{
    dose <- unlist(with(dataList, tapply(dose, curveid,
                                         function(x) {sort(x)}))[unique(curveid)])
    response <- unlist(with(dataList, tapply(dose, curveid,
                                             function(x) {ppoints(x, 0.5)}))[unique(curveid)])
    list(dose = dose, response = response)
}


"ggplotDrcAssayNo" <- function(curveid, plotid)
{
    if (!is.null(plotid)) {as.vector(plotid)} else {as.vector(curveid)}
}


"ggplotDrcPlotLevels" <- function(level, uniAss)
{
    if (is.null(level))
    {
        level <- uniAss
    } else {
        level <- intersect(level, uniAss)
    }
    if (length(level) < 1)
    {
        stop("Nothing to plot")
    }
    level
}


"ggplotDrcNormalizeResponse" <- function(resp, curveid, object, normRef)
{
    names(resp) <- seq(length(resp))
    respList <- split(resp, curveid)
    respNorm <- mapply(normalizeLU, respList,
                       as.list(as.data.frame(getLU(object)))[names(respList)],
                       normRef = normRef, SIMPLIFY = FALSE)
    do.call(c, unname(respNorm))[as.character(seq(length(resp)))]
}


"ggplotDrcAxisData" <- function(dose, xlim, bp, log_x_axis, logDose)
{
    xLimits <- if (missing(xlim)) c(min(dose), max(dose)) else xlim
    conLevel <- NA
    controlShifted <- FALSE

    if (isTRUE(log_x_axis) || (!is.null(logDose)))
    {
        conLevel <- ggplotDrcConLevel(dose, logDose, bp)
        if (xLimits[1] < conLevel)
        {
            xLimits[1] <- conLevel
            dose[dose < conLevel] <- conLevel
            controlShifted <- TRUE
        }
    }

    if (xLimits[1] >= xLimits[2])
    {
        stop("Argument 'bp' is set too high")
    }

    list(dose = dose, xlim = xLimits, conLevel = conLevel,
         controlShifted = controlShifted)
}


"ggplotDrcDoseGrid" <- function(xlim, gridsize, log_x_axis, logDose)
{
    if ((is.null(logDose)) && isTRUE(log_x_axis))
    {
        dosePts <- exp(seq(log(xlim[1]), log(xlim[2]), length = gridsize))
        dosePts[1] <- xlim[1]
        dosePts[gridsize] <- xlim[2]
        return(dosePts)
    }
    seq(xlim[1], xlim[2], length = gridsize)
}


"ggplotDrcCurveMatrix" <- function(plotFct, logDose, dosePts, object,
                                   normal, normRef, uniAss, level)
{
    plotMat <- if (is.null(logDose)) plotFct(dosePts) else plotFct(logDose ^ dosePts)
    plotMat <- as.matrix(plotMat)
    if (ncol(plotMat) == 1 && length(uniAss) == 1)
    {
        colnames(plotMat) <- as.character(uniAss)
    } else if (is.null(colnames(plotMat)) && ncol(plotMat) == length(uniAss))
    {
        colnames(plotMat) <- as.character(uniAss)
    }

    if (normal)
    {
        plotMat <- mapply(normalizeLU, as.list(as.data.frame(plotMat)),
                          as.list(as.data.frame(getLU(object))),
                          normRef = normRef)
        plotMat <- as.matrix(plotMat)
    }

    levelIndex <- match(as.character(level), colnames(plotMat))
    if (any(is.na(levelIndex)))
    {
        levelIndex[is.na(levelIndex)] <- match(level[is.na(levelIndex)], uniAss)
    }
    plotMat <- plotMat[, levelIndex, drop = FALSE]
    colnames(plotMat) <- as.character(level)
    plotMat
}


"ggplotDrcLineData" <- function(dosePts, plotMat, level)
{
    data.frame(
        dose = rep(dosePts, times = length(level)),
        response = as.vector(plotMat),
        curve = factor(rep(as.character(level), each = length(dosePts)),
                       levels = as.character(level))
    )
}


"ggplotDrcPointData" <- function(dose, resp, assayNo, level, xlim, type)
{
    keepObs <- (dose >= xlim[1]) & (dose <= xlim[2]) & (assayNo %in% level)
    if (!any(keepObs))
    {
        return(NULL)
    }

    pointData <- data.frame(dose = dose[keepObs],
                            response = resp[keepObs],
                            curve = assayNo[keepObs])
    if (identical(type, "average"))
    {
        pointData <- aggregate(response ~ dose + curve, pointData, mean)
    }
    pointData$curve <- factor(as.character(pointData$curve),
                              levels = as.character(level))
    pointData
}


"ggplotDrcAddRibbon" <- function(p, ribbonData)
{
    if (is.null(ribbonData))
    {
        return(p)
    }
    p + ggplot2::geom_ribbon(
        data = ribbonData,
        ggplot2::aes(x = dose, ymin = lower, ymax = upper,
                     fill = curve, group = curve),
        alpha = 0.2, colour = NA
    )
}


"ggplotDrcAddLine" <- function(p, lineData, type)
{
    if (identical(type, "obs"))
    {
        return(p)
    }
    p + ggplot2::geom_line(
        data = lineData,
        ggplot2::aes(x = dose, y = response, colour = curve,
                     group = curve)
    )
}


"ggplotDrcAddPoints" <- function(p, pointData, type)
{
    if (identical(type, "none") || is.null(pointData))
    {
        return(p)
    }
    p + ggplot2::geom_point(
        data = pointData,
        ggplot2::aes(x = dose, y = response, colour = curve,
                     group = curve)
    )
}


"ggplotDrcAddLabels" <- function(p, xlab, ylab, curveName, ribbonData)
{
    if (is.null(ribbonData))
    {
        return(p + ggplot2::labs(x = xlab, y = ylab, colour = curveName))
    }
    p + ggplot2::labs(x = xlab, y = ylab,
                      colour = curveName, fill = curveName)
}


"ggplotDrcAddGuides" <- function(p, lineData)
{
    if (length(unique(lineData$curve)) == 1)
    {
        return(p + ggplot2::guides(colour = "none", fill = "none"))
    }
    p
}


"ggplotDrcAddScales" <- function(p, object, plotData, log_x_axis,
                                 log_y_axis, conName)
{
    if (isTRUE(log_x_axis) && is.null(object[["curve"]][[2]]))
    {
        p <- ggplotDrcAddLogXScale(p, plotData, conName)
    }
    if (isTRUE(log_y_axis))
    {
        p <- p + ggplot2::scale_y_log10()
    }
    p
}


"ggplotDrcAddLogXScale" <- function(p, plotData, conName)
{
    if (plotData[["controlShifted"]])
    {
        return(p + ggplot2::scale_x_log10(
            breaks = ggplotDrcLogBreaks(plotData[["xlim"]],
                                        plotData[["conLevel"]]),
            labels = function(x) {
                out <- format(x, trim = TRUE, scientific = FALSE)
                out[abs(x - plotData[["conLevel"]]) <
                    sqrt(.Machine$double.eps)] <- conName
                out
            }
        ))
    }
    p + ggplot2::scale_x_log10()
}


"ggplotDrcConLevel" <- function(dose, logDose, bp)
{
    if (!missing(bp))
    {
        return(bp)
    }

    finiteDose <- dose[is.finite(dose)]
    if (!is.null(logDose))
    {
        return(round(min(finiteDose)) - 1)
    }

    posDose <- finiteDose[finiteDose > 0]
    if (length(posDose) < 1)
    {
        stop("Positive dose values are required for a logarithmic dose axis")
    }
    10 ^ (round(log10(min(posDose))) - 1)
}


"ggplotDrcRibbonData" <-
function(object, dosePts, level, confidence.level, normal, normRef)
{
    varNames <- ggplotDrcVariableNames(object)
    out <- vector("list", length(level))

    for (i in seq_along(level))
    {
        newdata <- data.frame(dosePts, rep(level[i], length(dosePts)),
                              check.names = FALSE)
        names(newdata) <- c(varNames[["dose"]], varNames[["curve"]])
        pred <- try(predict(object, newdata = newdata,
                            interval = "confidence",
                            level = confidence.level), silent = TRUE)
        if (inherits(pred, "try-error") || is.null(dim(pred)) ||
            !all(c("Prediction", "Lower", "Upper") %in% colnames(pred)))
        {
            warning("Confidence limits could not be calculated for curve '",
                    level[i], "'", call. = FALSE)
            next
        }

        response <- pred[, "Prediction"]
        lower <- pred[, "Lower"]
        upper <- pred[, "Upper"]
        if (normal)
        {
            lu <- ggplotDrcLU(object, level[i], i)
            response <- normalizeLU(response, lu, normRef)
            lower <- normalizeLU(lower, lu, normRef)
            upper <- normalizeLU(upper, lu, normRef)
        }

        out[[i]] <- data.frame(dose = dosePts, response = response,
                               lower = lower, upper = upper,
                               curve = as.character(level[i]))
    }

    out <- out[!vapply(out, is.null, logical(1))]
    if (length(out) < 1)
    {
        return(NULL)
    }
    out <- do.call(rbind, out)
    out$curve <- factor(out$curve, levels = as.character(level))
    out
}


"ggplotDrcLU" <- function(object, level, index)
{
    luMat <- getLU(object)
    level <- as.character(level)
    if (!is.null(colnames(luMat)) && level %in% colnames(luMat))
    {
        return(luMat[, level])
    }
    luMat[, index]
}


"ggplotDrcLogBreaks" <- function(xlim, conLevel)
{
    breaks <- pretty(xlim)
    breaks <- breaks[breaks > 0 & breaks >= xlim[1] & breaks <= xlim[2]]
    unique(c(conLevel, breaks))
}

utils::globalVariables(c("dose", "response", "curve", "lower", "upper"))
