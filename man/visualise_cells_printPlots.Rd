% Generated by roxygen2 (4.0.2): do not edit by hand
\name{visualise_cells_printPlots}
\alias{visualise_cells_printPlots}
\title{Prints a list of data.frames as ggplot2 facet plot.}
\usage{
visualise_cells_printPlots(simp, celldata, idcolumn = "Tick",
  valuecolumn = "LandUseIndex", title = "", filenamepostfix = title,
  legendtitle = "", factorial = FALSE, omitaxisticks = FALSE, ncol = if
  (is.list(celldata)) length(celldata[[1]][, 1]) else length(celldata[, 1]),
  coloursetname = simp$colours$defaultset, legenditemnames = NULL,
  ggplotaddon = NULL)
}
\arguments{
\item{simp}{SIMulation Properties}

\item{celldata}{(list of) data.frames contain info and X and X coordinates. If a list of data.frames,
elements must be named differently}

\item{idcolumn}{column used to separate and name rasters, refering to column names (set with colnames()) of the data.frame(s).}

\item{valuecolumn}{}

\item{title}{name of plot}

\item{filenamepostfix}{appended to the default output filename}

\item{legendtitle}{title for legend of raster values}

\item{factorial}{true if raster values are factorial (affects colour palette)}

\item{omitaxisticks}{omit axis ticks if true}

\item{ncol}{number of columns of facet wrap. Defauls to the number of rasters in the first dataframe}

\item{coloursetname}{id for colour set (if factorial) to pass to simp$colours$GenericFun (e.g. "AFT", "Capital", "Service")}

\item{legenditemnames}{names for legend items}
}
\value{
raster visualisation
}
\description{
Prints a list of data.frames as ggplot2 facet plot.
}
\examples{
#######################################################################
# crafty demo for raster visualisation (capital input data)
# Last update: 	20.02.2015
# Author: 		Sascha Holzhauer
#######################################################################
source(simp$simpDefinition)   # read simp
data <- input_csv_param_capitals()
visualisation_cells_printPlots(data, legendTitle = "Capitals Example", 
		factorial= FALSE, omitAxisTicks = FALSE, ncol = 1, id="None")
}
\author{
Sascha Holzhauer
}
\seealso{
output_tools_getDefaultFilename
}
