library(drc)
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
