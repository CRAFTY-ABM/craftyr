% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hl_data_tables.R
\name{hl_table_demand}
\alias{hl_table_demand}
\title{Output aggregated demand as table with services as columns}
\usage{
hl_table_demand(simp, dataname = "dataAggregateSupplyDemand",
  demanddataname = "csv_aggregateServiceDemand", latex = TRUE,
  forceparam = FALSE, regionset = NULL)
}
\arguments{
\item{simp}{}

\item{dataname}{passed to \link{convert_aggregate_demand} as \code{sourcedataname}}

\item{demanddataname}{passed to \link{convert_aggregate_demand}}

\item{latex}{if \code{FALSE} return the data.frame of demands instead of LaTeX table.}
}
\value{
data.frame or printed LaTeX table
}
\description{
Output aggregated demand as table with services as columns
}
\author{
Sascha Holzhauer
}
