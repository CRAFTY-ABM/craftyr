#' Plot land use map series
#' @param simp 
#' @param dataname 
#' @param ... passed to visualise_raster_printPlots
#' @return map plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_maps_aft <- function(simp, dataname = "raster_aft", ...) {
	input_tools_load(simp, dataname)
	raster_aft <- get(dataname)
	visualise_raster_printPlots(simp, raster_aft,
			title = "AFTs",
			ncol=5,
			coloursetname="AFT",
			factorial=TRUE,
			legenditemnames = simp$mdata$aftNames,
			ggplotaddon =  list(ggplot2::theme(legend.position="bottom"),
					ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))),
			...)
}