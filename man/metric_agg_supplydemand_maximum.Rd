% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics_agg.R
\name{metric_agg_supplydemand_maximum}
\alias{metric_agg_supplydemand_maximum}
\title{Calculates the maximum percental demand supply gap for across services for all ticks}
\usage{
metric_agg_supplydemand_maximum(simp, services = NULL,
  datanamedemand = "csv_aggregated_demand",
  datanamesupply = "csv_aggregated_supply", includesum = TRUE,
  asvector = FALSE)
}
\arguments{
\item{simp}{}

\item{services}{vector of character. Limits set of considered services if not \code{NULL}.}

\item{includesum}{if \code{TRUE} computes the sum across all services}

\item{asvector}{if \code{TRUE} metric is returned as named vector}

\item{dataname}{}
}
\value{
data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric (percental demand supply gap) when \code{asvector == TRUE}
}
\description{
Calculates the maximum percental demand supply gap for across services for all ticks
}
\author{
Sascha Holzhauer
}
