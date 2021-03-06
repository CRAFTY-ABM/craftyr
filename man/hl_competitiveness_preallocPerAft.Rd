% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hl_aggregate_visualise.R
\name{hl_competitiveness_preallocPerAft}
\alias{hl_competitiveness_preallocPerAft}
\title{Visualise pre-allocation competitiveness per AFT}
\usage{
hl_competitiveness_preallocPerAft(simp, dataname = "csv_preAlloc_rbinded",
  maxcompetitiveness = "90\%", numbins = 20, facet_ncol = 5,
  filename = paste("PreAllocationCompetition", simp$sim$id, sep = "_"),
  title = NULL, ggplotaddons = NULL, setfigdims = TRUE,
  afts = simp$mdata$aftNames[-1], checkexists = FALSE,
  skipemptybins = TRUE, returnplot = FALSE)
}
\arguments{
\item{simp}{}

\item{dataname}{}

\item{maxcompetitiveness}{either an absoulte value or a percentage given as string (e.g. "90%"). The latter
selects the lowest X percent.}

\item{numbins}{number of bins to bin data}

\item{facet_ncol}{if \code{NULL} ticks will be as rows, agents as columns. Otherwise, facets will 
distinguished by ticks only (and facet_cols specifies the number of columns)}

\item{filename}{}

\item{title}{plot title}

\item{ggplotaddons}{list of ggplot2 elements}

\item{setfigdims}{if \code{TRUE} \code{simp$fig$height} and \code{simp$fig$width} are set appropriately}

\item{afts}{AFTs to display (as vector of names)}

\item{returnplot}{if true the ggplot object is returned}
}
\value{
facet histogram plot in file
}
\description{
Write one file per AFT.
}
\author{
Sascha Holzhauer
}
