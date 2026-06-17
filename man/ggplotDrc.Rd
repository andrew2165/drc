\name{ggplotDrc}
\alias{ggplotDrc}

\title{Plot fitted dose-response curves with ggplot2}

\description{
  \code{ggplotDrc} creates a \pkg{ggplot2} plot from a fitted \code{drc}
  model, including observed responses, fitted dose-response curves, and
  optional confidence bands.
}

\usage{
ggplotDrc(object, ..., level = NULL,
  type = c("average", "all", "confidence", "none", "obs"),
  gridsize = 500, log_x_axis = FALSE, log_y_axis = FALSE,
  xlab, ylab, xlim, ylim,
  bp, conName = "0", normal = FALSE, normRef = 1,
  confidence.level = 0.95)
}

\arguments{
  \item{object}{An object of class \code{drc}.}
  \item{...}{Reserved for future extensions.}
  \item{level}{Optional vector specifying the curves to plot by name.}
  \item{type}{Character string specifying which layers to draw. Use
  \code{"average"} for averaged observations and fitted curves,
  \code{"all"} for all observations and fitted curves,
  \code{"confidence"} for all observations, fitted curves, and confidence
  bands, \code{"none"} for fitted curves only, or \code{"obs"} for
  observations only.}
  \item{gridsize}{Number of dose values used to draw each fitted curve. The
  default is 500 for smooth curves and confidence bands.}
  \item{log_x_axis}{Logical. If \code{TRUE}, the dose axis is displayed on a
  base-10 logarithmic scale. The default is \code{FALSE}.}
  \item{log_y_axis}{Logical. If \code{TRUE}, the response axis is displayed on
  a base-10 logarithmic scale. The default is \code{FALSE}.}
  \item{xlab}{Optional label for the dose axis.}
  \item{ylab}{Optional label for the response axis.}
  \item{xlim}{Optional numeric vector of length two giving dose limits.}
  \item{ylim}{Optional numeric vector of length two giving response limits.}
  \item{bp}{Numeric value specifying the positive dose value used to display
  zero or very small doses on a logarithmic dose axis.}
  \item{conName}{Label used for the shifted control dose on a logarithmic dose
  axis.}
  \item{normal}{Logical. If \code{TRUE}, normalized data and fitted curves are
  plotted as in \code{\link{plot.drc}}.}
  \item{normRef}{Numeric reference for normalization.}
  \item{confidence.level}{Confidence level for confidence bands. Defaults to
  0.95.}
}

\value{
  A \code{ggplot} object that can be modified with ordinary \pkg{ggplot2}
  layers and formatting functions.
}

\details{
  The fitted curves are generated from the same fitted model components used by
  \code{\link{plot.drc}}, so the helper works across standard single-dose
  models supported by the existing \pkg{drc} plotting path. Confidence bands use
  \code{\link{predict.drc}} with \code{interval = "confidence"} when available.
  The \pkg{ggplot2} package is required to use this helper.
}

\examples{
if (requireNamespace("ggplot2", quietly = TRUE)) {
ryegrass.m1 <- drm(rootl ~ conc, data = ryegrass, fct = LL.3())

ggplotDrc(ryegrass.m1) + ggplot2::labs(title = "Ryegrass")

ggplotDrc(ryegrass.m1, type = "confidence",
          xlab = "Ferulic acid (mM)",
          ylab = "Root length (cm)")

ggplotDrc(ryegrass.m1, type = "confidence", log_x_axis = TRUE)
}
}

\keyword{aplot}
