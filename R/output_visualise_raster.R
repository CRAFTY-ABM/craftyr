#' Prints a list of raster data as ggplot2 facet plot
#' @param rasterdata if a list, elements must be named differently
#' @param dataname vector of names of the same length as rasterData
#' @param legendtitle title for legend of raster values
#' @param factorial true if raster values are factorial (affects colour palette)
#' @param omitaxisticks omit axis ticks if true
#' @param ncol number of columns of facet wrap
#' @param coloursetname id for colour set (if factorial)
#' @return raster visualisation
#' @example demo/example_visualise_raster_plots.R
#'
#' @author Sascha Holzhauer
#' @export
visualise_raster_printPlots <- function(simp, rasterdata, dataname = NULL, legendtitle = "",
		factorial= FALSE, omitaxisticks = FALSE, ncol = 1, coloursetname="None", legenditemnames = NULL) {

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
			
			counter = counter + 1
			s <- data.frame(rasterToPoints(l), id = paste(outerListName, "-", names(l)), sep="")
			colnames(s) <- c("X", "Y", "Values", "ID")
			pointDf <- rbind(pointDf,s)
		}
	}

	if (length(rasterdata) != length(unique(pointDf$ID))) {
		throw("Elemens of rasterData must be named differently!")
	}

	if (!is.null(dataname)) {
		levels(pointDf$ID) <- unlist(dataname)
	}

	scaleFillElem <- scale_fill_gradientn(name=legendtitle, colours = c("red", "green"))
	if (factorial) {
		pointDf$Values <- factor(pointDf$Values)
		scaleFillElem <- scale_fill_manual(name=legendtitle, 
				values = settings_colours_getColorSet(set = coloursetname),
				labels = legenditemnames)
	}

	omitaxistickselem <- NULL
	if (omitaxisticks) {
		omitaxistickselem <- theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
	}

	p1 <- ggplot()+
			layer(geom="raster", data=pointDf, mapping=aes(X,Y,fill=Values)) +
			facet_wrap(~ID, ncol = ncol) +
			theme(strip.text.x = element_text(size=8)) +
			scaleFillElem +
			omitaxistickselem +
			#scale_x_continuous(name=expression(paste("Longitude (",degree,")")),limits=c(-4,2),expand=c(0,0))+
			#scale_y_continuous(name=expression(paste("Latitude (",degree,")")),limits=c(4,12),expand=c(0,0))+
			coord_equal()
	print(p1)
	dev.off()
}
#' Writes a list of raster data as single raw (only the raster data) ggplot2 plot files
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
		factorial= FALSE, omitaxisticks = FALSE, ncol = 1, coloursetname="None", legenditemnames = NULL) {

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
			
			s <- data.frame(rasterToPoints(l), id = names(l))
			colnames(s) <- c("X", "Y", "Values", "ID")
			if (!is.null(datanames)) {
				levels(s$ID) <- unlist(datanames)
			}
	
			scaleFillElem <- scale_fill_gradientn(name=legendtitle, colours = c("red", "green"))
			if (factorial) {
				s$Values <- factor(s$Values)
				scaleFillElem <- scale_fill_manual(name=legendtitle, 
						values=settings_colours_getColorSet(set = coloursetname),
						labels = legenditemnames)
			}
	
			p1 <- ggplot()+
				layer(geom="raster", data=s, mapping=aes(X,Y,fill=Values)) +
				scaleFillElem +
				visualisation_raster_printRawPlots_theme_nothing() +
				scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
				coord_equal()
	
			gt <- ggplot_gtable(ggplot_build(p1))
			ge <- subset(gt$layout, name == "panel")
	
			grid.draw(gt[ge$t:ge$b, ge$l:ge$r])
	
			#print(p1)
			dev.off()
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
	theme_bw(base_size = base_size, base_family = base_family) %+replace%
			theme(
					rect             = element_blank(),
					line             = element_blank(),
					text             = element_blank(),
					axis.ticks.margin = unit(0, "lines"),
					legend.position = "none",
					panel.background = element_blank(),
					panel.grid.major = element_blank(),
					panel.grid.minor = element_blank(),
					panel.margin = unit(0,"lines"),
					panel.border = element_blank(),
					plot.margin = unit(rep(0,4),"lines"),
					axis.ticks = element_blank(),
					axis.text.x = element_blank(),
					axis.text.y = element_blank(),
					axis.title.x = element_blank(),
					axis.title.y = element_blank(),
					axis.line = element_blank(),
					axis.ticks.length = unit(0,"null"),
					axis.ticks.margin = unit(0,"null")
			)
}
# check that it is a complete theme
# attr(theme_nothing(), "complete")