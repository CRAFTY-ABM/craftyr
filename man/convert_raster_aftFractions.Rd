% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_raster_aggregate.R
\name{convert_raster_aftFractions}
\alias{convert_raster_aftFractions}
\title{Determine the fraction of AFTs in the given rasters whose state corresonds with the given state}
\usage{
convert_raster_aftFractions(aft_raster, state_raster, state)
}
\arguments{
\item{aft_raster}{list (Regions) of list (Tick) of raster including information of AFT IDs}

\item{state_raster}{list (Regions) of list (Tick) of raster including information of AFT states}

\item{state}{the state of interest}
}
\value{
data.farme of fractions
}
\description{
Determine the fraction of AFTs in the given rasters whose state corresonds with the given state
}
\author{
Sascha Holzhauer
}
