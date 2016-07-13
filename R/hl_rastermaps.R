#' Plot land use map series
#' @param simp 
#' @param dataname 
#' @param returnplot if true the ggplot object is returned
#' @param ... passed to visualise_raster_printPlots
#' @return map plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_maps_aft <- function(simp, dataname = "raster_aft", ncol = 5, returnplot = FALSE,...) {
	input_tools_load(simp, dataname)
	raster_aft <- get(dataname)
	
	p1 <- visualise_raster_printPlots(simp, raster_aft,
			title = "AFTs",
			ncol=ncol,
			coloursetname="AFT",
			factorial=TRUE,
			legenditemnames = simp$mdata$aftNames,
			ggplotaddon =  list(ggplot2::theme(legend.position="bottom"),
					ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))),
			returnplot = returnplot,
			...)
	if (returnplot) return(p1)
}