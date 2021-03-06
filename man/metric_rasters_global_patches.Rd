% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics_raster.R
\name{metric_rasters_global_patches}
\alias{metric_rasters_global_patches}
\title{Determine the number of patches of a list of rasters}
\usage{
metric_rasters_global_patches(simp, dataname = "raster_landUseIndex",
  directions = 8, relevantindices = NULL, relevantafts = NULL,
  asvector = FALSE)
}
\arguments{
\item{simp}{}

\item{directions}{}

\item{relevantindices}{raster values not included in \code{relevantindices} are considered 
to be background values and separate clumps.}

\item{relevantafts}{vector of AFT names. Alternative to \code{relevantindices} which has precedence.}

\item{asvector}{if \code{TRUE} metric is returned as named vector}
}
\value{
data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric (changes (integer)) when \code{asvector == TRUE}
}
\description{
Determine the number of patches of a list of rasters
}
\author{
Sascha Holzhauer
}
