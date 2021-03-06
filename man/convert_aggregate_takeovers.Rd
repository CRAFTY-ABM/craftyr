% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_aggregate.R
\name{convert_aggregate_takeovers}
\alias{convert_aggregate_takeovers}
\title{Extracts numbers of take overs for every pair of AFT for every tick from stored cell csv data
Requires land use indices stored in an object whose name is given by \code{landusedataname}.}
\usage{
convert_aggregate_takeovers(simp,
  landusedataname = "csv_LandUseIndex_rbinded", grouping = c("Scenario",
  "Runid", "Region"))
}
\arguments{
\item{simp}{\itemize{
 \item \code{\link{input_tools_load}}
 \item \code{simp$mdata$aftNames}
}}

\item{landusedataname}{Name of stored land use indices object}

\item{grouping}{adjusts which columns are considered for aggregatation}
}
\value{
data.frame with previous AFT as row and resulting AFT as column
}
\description{
Scenario, Runid and Region are preserved.
}
\seealso{
Other takeovers: \code{\link{hl_takeovers_all}},
  \code{\link{hl_takeovers}},
  \code{\link{output_visualise_aftFluctuations}},
  \code{\link{output_visualise_takeovers}}
}
\author{
Sascha Holzhauer
}
