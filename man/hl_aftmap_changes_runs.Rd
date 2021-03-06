% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hl_maps.R
\name{hl_aftmap_changes_runs}
\alias{hl_aftmap_changes_runs}
\title{Plot changes between runs of a specific tick for a particular (combination of) land use(s)}
\usage{
hl_aftmap_changes_runs(simp, dataname = "csv_LandUseIndex_rbinded", ids,
  outdirs,
  selectedAFTGroups = as.list(as.numeric(names(simp$mdata$aftNames))),
  regions = simp$sim$regions, tick = 2040, ncol = 1,
  title = paste(title, "_", paste(simp$mdata$aftNames[as.character(aftgroup)],
  collapse = "-"), sep = ""), ggplotaddon = NULL, addcountryshapes = FALSE,
  plotunchanged = TRUE, returnplot = FALSE)
}
\arguments{
\item{simp}{}

\item{dataname}{}

\item{ncol}{}

\item{ggplotaddon}{}

\item{returnplot}{if true the ggplot object is returned}

\item{selectedAFT}{}

\item{starttick}{}

\item{endtick}{}
}
\value{
figure file / plot
}
\description{
Plot changes between runs of a specific tick for a particular (combination of) land use(s)
}
\author{
Sascha Holzhauer
}
