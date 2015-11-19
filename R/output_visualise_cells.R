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
#' @example demo/example_visualise_cells_csv_aft.R
#'
#' @author Sascha Holzhauer
#' @export
visualise_cells_printPlots <- function(simp, celldata, idcolumn = "Tick", valuecolumn = "LandUseIndex",
		title = "", filenamepostfix = title, legendtitle = "",
		factorial= FALSE, omitaxisticks = FALSE, ncol = if (!is.data.frame(celldata)) length(celldata) else 1, 
		coloursetname=simp$colours$defaultset, legenditemnames = NULL, ggplotaddon = NULL,
		theme = visualisation_raster_legendonlytheme) {
	
	futile.logger::flog.debug("Print cell data...",
			name="craftyr.visualise.cells")
	
	if(!is.list(celldata)) {
		Roo::throw.default("Parameter celldata must be a data.frame or other list!")
	}
	
	if(is.data.frame(celldata)) {
		celldata <- list(celldata)
	}
	
	if(is.null(names(celldata))) {
		warning("Assign names to elements of list! Using letters...")
		names(celldata) <- paste(letters[1:length(celldata)], ")", sep="")
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
	
	# ggplot throws an error if any facet consists only of NAs.
	celldata <- plyr::ddply(celldata, "ID", function(df) {
				if (all(is.na(df$Values))) {
					df[1, "Values"] <- levels(df$Values)[1]
				}
				df
			})
	#ggplotaddon <- countryshapeelem

	p1 <- ggplot2::ggplot()+
			ggplot2::layer(geom="raster", data=celldata, mapping=ggplot2::aes(X,Y,fill=Values)) +
			ggplot2::facet_wrap(~ID, ncol = ncol) +
			ggplot2::theme(strip.text.x = ggplot2::element_text(size=simp$fig$facetlabelsize)) +
			(if (title != "") ggplot2::labs(title = title)) +
			theme() +
			scaleFillElem +
			omitaxistickselem +
			ggplot2::coord_equal(ratio=1) +
			ggplotaddon
	print(p1)
	simp$fig$close()
}
#' Writes a list of raster data as single raw (only the raster data) ggplot2 plot files
#' @param simp SIMulation Properties
#' @param rasterdata if a list, elements must be named differently
#' @param datanames vector of names of the same length as rasterData
#' @param factorial true if raster values are factorial (affects colour palette)
#' @param omitaxisticks omit axis ticks if true
#' @param ncol number of columns of facet wrap
#' @param coloursetname id for colour set (if factorial) to pass to simp$colours$GenericFun (e.g. "AFT", "Capital", "Service")
#' @return raster visualisation files
#' @example demo/example_visualise_cells_raster_aft_raw.R
#'
#' @author Sascha Holzhauer
#' @export
visualise_cells_printRawPlots <- function(simp, celldata, idcolumn = "Tick", valuecolumn = "LandUseIndex",
		title = "", filenamepostfix = title,
		factorial= FALSE, ncol = if (!is.data.frame(celldata)) length(celldata) else 1, 
		coloursetname=simp$colours$defaultset, ggplotaddon = NULL) {
	
	futile.logger::flog.debug("Print cell data...",
			name="craftyr.visualise.cells")
	
	if (is.null(celldata)) {
		Roo::throw.default("celldata is null!")
	}
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
	
	scaleFillElem <- ggplot2::scale_fill_gradientn(colours = simp$colours$binarycolours)
	if (factorial) {
		celldata$Values <- factor(celldata$Values)
		scaleFillElem <- ggplot2::scale_fill_manual( 
				values = simp$colours$GenericFun(simp, number = length(unique(celldata$Values)), set = coloursetname))
	}
			
	p1 <- ggplot2::ggplot()+
			ggplot2::layer(geom="raster", data=celldata, mapping=ggplot2::aes(X,Y,fill=Values)) +
			ggplot2::facet_wrap(~ID, ncol = ncol) +
			scaleFillElem +
			ggplot2::scale_x_continuous(expand=c(0,0)) + ggplot2::scale_y_continuous(expand=c(0,0)) +
			ggplot2::coord_equal() +
			ggplot2::theme_bw() +
			visualisation_raster_legendonlytheme()
	
	gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p1))
	ge <- subset(gt$layout, substring(name,1,5) == "panel")
	
	for (i in 1:length(ge[,1])) {
		g <- ge[i,]
		simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "raster", sep="/"), 
				filename = output_tools_getDefaultFilename(simp, postfix = paste(filenamepostfix, "_", i, sep="")))
		grid::grid.draw(gt[g$t:g$b, g$l:g$r])
		simp$fig$close()
	}
}