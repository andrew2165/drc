library(drc)

missingGgplot2 <- try(drc:::ggplotDrcRequireGgplot2("drcMissingGgplot2"),
                      silent = TRUE)
stopifnot(inherits(missingGgplot2, "try-error"))
stopifnot(grepl("Package 'ggplot2' is required", missingGgplot2))

if (!requireNamespace("ggplot2", quietly = TRUE))
{
    message("Skipping ggplotDrc tests: package 'ggplot2' is not available")
    quit(save = "no")
}

library(ggplot2)

makeDrcObject <- function()
{
    fct <- function(x)
    {
        cbind(A = 10 / (1 + x), B = 8 / (1 + 0.5 * x))
    }
    structure(
        list(
            curve = list(fct, NULL),
            type = "continuous",
            dataList = list(
                dose = c(0, 1, 1, 10, 10, 0, 1, 1, 10, 10),
                origResp = c(9.8, 5.2, 4.9, 1.1, 0.8,
                             8.1, 5.0, 5.4, 1.3, 1.5),
                curveid = c(rep("A", 5), rep("B", 5)),
                names = list(dName = "dose", orName = "response",
                             wName = "weights", cNames = "curve",
                             rName = "")
            )
        ),
        class = "drc"
    )
}

fit <- makeDrcObject()

p <- ggplotDrc(fit)
stopifnot(inherits(p, "ggplot"))
stopifnot(inherits(p + labs(title = "Dose response"), "ggplot"))

pb <- ggplot_build(p)
stopifnot(length(pb$data) == 2)
stopifnot(nrow(pb$data[[1]]) == 1000)
stopifnot(min(pb$data[[1]]$x) == 0)
stopifnot(nrow(pb$data[[2]]) == 6)

p_one <- ggplotDrc(fit, level = "B", type = "none", gridsize = 25)
pb_one <- ggplot_build(p_one)
stopifnot(length(pb_one$data) == 1)
stopifnot(nrow(pb_one$data[[1]]) == 25)

p_obs <- ggplotDrc(fit, type = "obs")
pb_obs <- ggplot_build(p_obs)
stopifnot(length(pb_obs$data) == 1)
stopifnot(nrow(pb_obs$data[[1]]) == 10)

p_log <- ggplotDrc(fit, log_x_axis = TRUE)
stopifnot(min(p_log$layers[[1]]$data$dose) > 0)

data("ryegrass", package = "drcData")
ryegrass_fit <- drm(rootl ~ conc, data = ryegrass, fct = LL.3())
ryegrass_plot <- ggplotDrc(ryegrass_fit, type = "confidence", gridsize = 50)
ryegrass_build <- ggplot_build(ryegrass_plot)
stopifnot(inherits(ryegrass_plot, "ggplot"))
stopifnot(length(ryegrass_build$data) == 3)
stopifnot(all(c("ymin", "ymax") %in% names(ryegrass_build$data[[1]])))
stopifnot(nrow(ryegrass_build$data[[2]]) == 50)

spinach_env <- new.env(parent = emptyenv())
spinach_loaded <- try(data("spinach", package = "drcData", envir = spinach_env),
                      silent = TRUE)
spinach_available <- !inherits(spinach_loaded, "try-error") &&
    exists("spinach", envir = spinach_env, inherits = FALSE)
if (spinach_available)
{
    spinach <- get("spinach", envir = spinach_env)
    spinach_fit <- drm(SLOPE ~ DOSE, CURVE, data = spinach, fct = LL.4())
    spinach_plot <- ggplotDrc(spinach_fit, level = 1:2, gridsize = 30)
    spinach_build <- ggplot_build(spinach_plot)
    stopifnot(inherits(spinach_plot, "ggplot"))
    stopifnot(nrow(spinach_build$data[[1]]) == 60)
}
