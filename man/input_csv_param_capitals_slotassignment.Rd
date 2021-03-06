% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_csv_param.R
\name{input_csv_param_capitals_slotassignment}
\alias{input_csv_param_capitals_slotassignment}
\title{Assign regions to slots for parallel computing}
\usage{
input_csv_param_capitals_slotassignment(simp, capitals = simp$mdata$capitals,
  numslots = 16, regionpartfromend = 2, regionpartdevider = "_")
}
\arguments{
\item{simp}{}

\item{capitals}{}

\item{regionpartfromend}{}

\item{regionpartdevider}{}
}
\value{
list of groups of regions
}
\description{
Assign regions to slots for parallel computing
}
\author{
Sascha Holzhauer
}
