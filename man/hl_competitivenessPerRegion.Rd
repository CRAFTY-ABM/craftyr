% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hl_aggregate_visualise.R
\name{hl_competitivenessPerRegion}
\alias{hl_competitivenessPerRegion}
\title{Load from csv data, aggregate, and plot AFT competitiveness per region}
\usage{
hl_competitivenessPerRegion(simp, dataname = "csv_cell_aggregated",
  facet_ncol = 4, returnplot = FALSE)
}
\arguments{
\item{simp}{}

\item{dataname}{}
}
\value{
ggplot2 plot
}
\description{
If the number of regions is above \code{simp$fig$maxnumtypes} facets will be applied to region variable
}
\author{
Sascha Holzhauer
}
