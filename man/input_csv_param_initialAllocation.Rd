% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_csv_param.R
\name{input_csv_param_initialAllocation}
\alias{input_csv_param_initialAllocation}
\title{Reads aft allocation from CSV data for potentially multiple regions}
\usage{
input_csv_param_initialAllocation(simp, aftColumn = "LandUseIndex",
  filename = paste(simp$sim$regions, "-Cell.csv", sep = ""))
}
\arguments{
\item{simp}{SIMulation Properties}

\item{aftColumn}{header of AFT column}
}
\value{
list of data.frames containing aft allocation (And x and y coordinates)
}
\description{
Reads aft allocation from CSV data for potentially multiple regions
}
\author{
Sascha Holzhauer
}
