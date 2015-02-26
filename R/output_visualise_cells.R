library(ggplot2)  # correct (see stack exchange question) for %+replace%
#' Prints a list of data.frames as ggplot2 facet plot.
#' 
#' @param simp SIMulation Properties
#' @param celldata (list of) data.frames contain info and X and X coordinates. If a list of data.frames,
#'  elements must be named differently
#' @param valuecolumn
#' @param idcolumn column used to separate and name rasters, refering to column names (set with colnames()) of the data.frame(s).
#' @param title name of plot
#' @param filenamepostfix appended to the default output filename @seealso output_tools_getDefaultFilename
#' @param legendtitle title for legend of raster values
#' @param factorial true if raster values are factorial (affects colour palette)
#' @param omitaxisticks omit axis ticks if true
#' @param ncol number of columns of facet wrap. Defauls to the number of rasters in the first dataframe
#' @param coloursetname id for colour set (if factorial) to pass to simp$colours$GenericFun (e.g. "AFT", "Capital", "Service")
#' @param legenditemnames names for legend items
#' @return raster visualisation
#' @example demo/example_visualise_cells_plots.R
#'
#' @author Sascha Holzhauer
#' @export
visualise_cells_printPlots <- function(simp, celldata, idcolumn = "Tick", valuecolumn = "LandUseIndex",
		title = "", filenamepostfix = title, legendtitle = "",
		factorial= FALSE, omitaxisticks = FALSE, ncol = if (is.list(celldata)) length(celldata[[1]][,1]) else length(celldata[,1]), 
		coloursetname=simp$colours$defaultset, legenditemnames = NULL, ggplotaddon = NULL) {
	
	if (simp$debug$output > 0) cat("Print cell data", "...\n")
	
	if(!is.list(celldata)) {
		Roo::throw.default("Parameter celldata must be a data.frame or other list!")
	}
	
	if(is.null(names(celldata))) {
		warning("Assign names to elements of list! Using letters...")
		names(celldata) <- letters[1:length(celldata)]
	}
	
	listlen <- length(celldata)
	
	celldata <- mapply(function(infoCellDataVector, listname) {
				s <- data.frame(
						X = infoCellDataVector[simp$csv$cname_x],
						Y = infoCellDataVector[simp$csv$cname_y],
						Values = as.numeric(infoCellDataVector[[valuecolumn]]),
						ID = paste(if (listlen > 1) listname else "", infoCellDataVector[[idcolumn]]))
				colnames(s) <- c("X", "Y", "Values", "ID")
				s
			}, celldata, names(celldata), SIMPLIFY = FALSE)
	
	gc()
	celldata <- do.call(rbind, celldata)

	## PLOTTING
	simp$fig$numcols <- ncol
	simp$fig$numfigs <- length(unique(celldata$ID))
	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "raster", sep="/"), 
			filename = output_tools_getDefaultFilename(simp, postfix = filenamepostfix))

	scaleFillElem <- ggplot2::scale_fill_gradientn(name=legendtitle, colours = simp$colours$binarycolours)
	if (factorial) {
		celldata$Values <- factor(celldata$Values)
		scaleFillElem <- ggplot2::scale_fill_manual(name=legendtitle, 
				values = simp$colours$GenericFun(simp, number = length(unique(celldata$Values)), set = coloursetname),
				labels = legenditemnames)
	}
	
	omitaxistickselem <- NULL
	if (omitaxisticks) {
		omitaxistickselem <- ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), 
				axis.title = ggplot2::element_blank())
	}
	
	p1 <- ggplot2::ggplot()+
			ggplot2::layer(geom="raster", data=celldata, mapping=ggplot2::aes(X,Y,fill=Values)) +
			ggplot2::facet_wrap(~ID, ncol = ncol) +
			ggplot2::theme(strip.text.x = ggplot2::element_text(size=simp$fig$facetlabelsize)) +
			(if (title != "") ggplot2::labs(title = title)) + 
			scaleFillElem +
			omitaxistickselem +
			ggplot2::coord_equal(ratio=1) +
			ggplotaddon
	print(p1)
	simp$fig$close()
}