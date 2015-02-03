library(ggplot2)  # correct (see stack exchange question) for %+replace%
#' Prints a list of raster data as ggplot2 facet plot
#' 
#' @param simp SIMulation Properties
#' @param inforasterdata (list of) data.frames contain info and raster objects. If a list, elements must be named differently
#' @param idcolumn column used to separate and name rasters
#' @param title name of plot
#' @param filenamepostfix appended to the default output filename @seealso output_tools_getDefaultFilename
#' @param legendtitle title for legend of raster values
#' @param factorial true if raster values are factorial (affects colour palette)
#' @param omitaxisticks omit axis ticks if true
#' @param ncol number of columns of facet wrap. Defauls to the number of rasters in the first dataframe
#' @param coloursetname id for colour set (if factorial)
#' @param legenditemnames names for legend items
#' @return raster visualisation
#' @example demo/example_visualise_raster_plots.R
#'
#' @author Sascha Holzhauer
#' @export
visualise_raster_printPlots <- function(simp, inforasterdata, idcolumn = "Tick",
		title = "", filenamepostfix = title, legendtitle = "",
		factorial= FALSE, omitaxisticks = FALSE, ncol = if (is.list(inforasterdata)) length(inforasterdata[[1]][,1]) else length(inforasterdata[,1]), 
		coloursetname=simp$colours$defaultset, legenditemnames = NULL, ggplotaddon = NULL) {
	
	if (simp$debug$output > 0) cat("Print raster data", "...\n")
	
	if(!is.list(inforasterdata)) {
		inforasterdata <- list("Something" = inforasterdata)
	}
	
	data <- NULL
	listlen <- length(inforasterdata)
	data <- mapply(function(infoRasterDataVector, listname) {
						idata <- apply(infoRasterDataVector, MARGIN=1, function(infoRaster) {
							s <- data.frame(raster::rasterToPoints(infoRaster[["Raster"]]), 
									ID = paste(if (listlen > 1) listname, infoRaster[[idcolumn]]))
							colnames(s) <- c("X", "Y", "Values", "ID")
							s
						})
						do.call(rbind, idata)
		}, inforasterdata, names(inforasterdata), SIMPLIFY = FALSE)
	d <- do.call(rbind, data)

	## PLOTTING
	simp$fig$numcols <- ncol
	simp$fig$numfigs <- length(unique(d$ID))
	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "raster", sep="/"), 
			filename = output_tools_getDefaultFilename(simp, postfix = filenamepostfix))

	scaleFillElem <- ggplot2::scale_fill_gradientn(name=legendtitle, colours = simp$colours$binarycolours)
	if (factorial) {
		d$Values <- factor(d$Values)
		scaleFillElem <- ggplot2::scale_fill_manual(name=legendtitle, 
				values = simp$colours$GenericFun(set = coloursetname),
				labels = legenditemnames)
	}
	
	omitaxistickselem <- NULL
	if (omitaxisticks) {
		omitaxistickselem <- ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), 
				axis.title = ggplot2::element_blank())
	}
	
	p1 <- ggplot2::ggplot()+
			ggplot2::layer(geom="raster", data=d, mapping=ggplot2::aes(X,Y,fill=Values)) +
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
#' Prints a list of raster data as ggplot2 facet plot
#' DEPRECATED
#' @param simp SIMulation Properties
#' @param rasterdata if a list, elements must be named differently
#' @param dataname vector of names of the same length as rasterData
#' @param legendtitle title for legend of raster values
#' @param factorial true if raster values are factorial (affects colour palette)
#' @param omitaxisticks omit axis ticks if true
#' @param ncol number of columns of facet wrap
#' @param coloursetname id for colour set (if factorial)
#' @param legenditemnames names for legend items
#' @return raster visualisation
#' @example demo/example_visualise_raster_plots.R
#'
#' @author Sascha Holzhauer
#' @export
visualise_raster_printPlotsUnnamed <- function(simp, rasterdata, dataname = NULL, legendtitle = "",
		factorial= FALSE, omitaxisticks = FALSE, ncol = 1, coloursetname=simp$colours$defaultset, legenditemnames = NULL) {

	if(!is.list(rasterdata)) {
		rasterdata <- list(rasterdata)
	}
	
	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "raster", legendtitle, sep="/"), filename = legendtitle)
	
	pointDf <- data.frame()
	counter = 0
	outercounter = 0
	for (rasters in rasterdata) {
		outercounter = outercounter + 1
		outerListName <- names(rasterdata)[outercounter]
		for (l in rasters) {
			rasterName <- names(l)
			if (is.null(rasterName)) 
			counter = counter + 1
			s <- data.frame(raster::rasterToPoints(l), ID = paste(outerListName, "-", names(l)), sep="")
			colnames(s) <- c("X", "Y", "Values", "ID")
			pointDf <- rbind(pointDf,s)
		}
	}

	if (length(rasterdata) != length(unique(pointDf$ID))) {
		R.oo::throw.default("Elemens of rasterData must be named differently!")
	}

	if (!is.null(dataname)) {
		levels(pointDf$ID) <- unlist(dataname)
	}

	scaleFillElem <- ggplot2::scale_fill_gradientn(name=legendtitle, colours = c("red", "green"))
	if (factorial) {
		pointDf$Values <- factor(pointDf$Values)
		scaleFillElem <- ggplot2::scale_fill_manual(name=legendtitle, 
				values = settings_colours_getColorSet(set = coloursetname),
				labels = legenditemnames)
	}

	omitaxistickselem <- NULL
	if (omitaxisticks) {
		omitaxistickselem <- ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), 
				axis.title = ggplot2::element_blank())
	}

	p1 <- ggplot2::ggplot()+
			ggplot2::layer(geom="raster", data=pointDf, mapping=ggplot2::aes(X,Y,fill=Values)) +
			ggplot2::facet_wrap(~ID, ncol = ncol) +
			ggplot2::theme(strip.text.x = ggplot2::element_text(size=simp$fig$facetlabelsize)) +
			scaleFillElem +
			omitaxistickselem +
			ggplot2::coord_equal()
	print(p1)
	simp$fig$close()
}
#' Writes a list of raster data as single raw (only the raster data) ggplot2 plot files
#' @param simp SIMulation Properties
#' @param rasterdata if a list, elements must be named differently
#' @param datanames vector of names of the same length as rasterData
#' @param legendtitle title for legend of raster values
#' @param factorial true if raster values are factorial (affects colour palette)
#' @param omitaxisticks omit axis ticks if true
#' @param ncol number of columns of facet wrap
#' @param coloursetname name of colour set from settings_colours_getColorSet (if factorial)
#' @return raster visualisation files
#' @example demo/example_visualise_raster_rawplots.R
#'
#' @author Sascha Holzhauer
#' @export
visualise_raster_printRawPlots <- function(simp, rasterdata, datanames = NULL, legendtitle = "", 
		factorial= FALSE, omitaxisticks = FALSE, ncol = 1, coloursetname=simp$colours$defaultset, legenditemnames = NULL) {

	if(!is.list(rasterdata)) {
		rasterdata <- list(rasterdata)
	}

	counter = 0
	outercounter = 0
	for (rasters in rasterdata) {
		outercounter = outercounter + 1
		outerListName <- names(rasterdata)[outercounter]
		for (l in rasters) {
			rasterName <- names(l)
			
			simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "raster", outerListName, sep="/"), filename = rasterName)
			
			s <- data.frame(raster::rasterToPoints(l), id = names(l))
			colnames(s) <- c("X", "Y", "Values", "ID")
			if (!is.null(datanames)) {
				levels(s$ID) <- unlist(datanames)
			}
	
			scaleFillElem <- ggplot2::scale_fill_gradientn(name=legendtitle, colours = c("red", "green"))
			if (factorial) {
				s$Values <- factor(s$Values)
				scaleFillElem <- ggplot2::scale_fill_manual(name=legendtitle, 
						values=settings_colours_getColorSet(set = coloursetname),
						labels = legenditemnames)
			}
	
			p1 <- ggplot2::ggplot()+
				ggplot2::layer(geom="raster", data=s, mapping=ggplot2::aes(X,Y,fill=Values)) +
				scaleFillElem +
				visualisation_raster_printRawPlots_theme_nothing() +
				ggplot2::scale_x_continuous(expand=c(0,0)) + ggplot2::scale_y_continuous(expand=c(0,0)) +
				ggplot2::coord_equal()
	
			gt <- ggplot2::ggplot_gtable(ggplot_build(p1))
			ge <- subset(gt$layout, name == "panel")
	
			grid::grid.draw(gt[ge$t:ge$b, ge$l:ge$r])
	
			simp$fig$close()
		}
	}
}
#' GGplot2 theme for visualisation_raster_printRawPlots
#' @param base_size
#' @param base_family
#' @return ggplot2 theme
#'
#' @author Sascha Holzhauer
visualisation_raster_printRawPlots_theme_nothing <- function(base_size = 12, base_family = "Helvetica")
{
	ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
			ggplot2::theme(
					rect             = ggplot2::element_blank(),
					line             = ggplot2::element_blank(),
					text             = ggplot2::element_blank(),
					axis.ticks.margin = grid::unit(0, "lines"),
					legend.position = "none",
					panel.background = ggplot2::element_blank(),
					panel.grid.major = ggplot2::element_blank(),
					panel.grid.minor = ggplot2::element_blank(),
					panel.margin = grid::unit(0,"lines"),
					panel.border = ggplot2::element_blank(),
					plot.margin = grid::unit(rep(0,4),"lines"),
					axis.ticks = ggplot2::element_blank(),
					axis.text.x = ggplot2::element_blank(),
					axis.text.y = ggplot2::element_blank(),
					axis.title.x = ggplot2::element_blank(),
					axis.title.y = ggplot2::element_blank(),
					axis.line = ggplot2::element_blank(),
					axis.ticks.length = grid::unit(0,"null"),
					axis.ticks.margin = grid::unit(0,"null")
			)
}

#' Only facet elements
#' @param base_size 
#' @param base_family 
#' @return theme
#' 
#' @author Sascha Holzhauer
#' @export
visualisation_raster_legendonlytheme <- function(base_size = 11, base_family = "Helvetica"){
	ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
			ggplot2::theme(
					axis.ticks.margin = grid::unit(0, "lines"),
					panel.background = ggplot2::element_blank(),
					panel.grid.major = ggplot2::element_blank(),
					panel.grid.minor = ggplot2::element_blank(),
					panel.margin = grid::unit(0,"lines"),
					panel.border = ggplot2::element_blank(),
					plot.margin = grid::unit(rep(0,4),"lines"),
					plot.title = ggplot2::element_blank(),
					axis.ticks = ggplot2::element_blank(),
					axis.text.x = ggplot2::element_blank(),
					axis.text.y = ggplot2::element_blank(),
					axis.title.x = ggplot2::element_blank(),
					axis.title.y = ggplot2::element_blank(),
					axis.line = ggplot2::element_blank(),
					axis.ticks.length = grid::unit(0,"null"),
					axis.ticks.margin = grid::unit(0,"null")
			)
}
# check that it is a complete theme
# attr(theme_nothing(), "complete")