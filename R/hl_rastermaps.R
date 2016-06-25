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
#' Calcualtes the changes in land uses in a row of raster data.
#' 
#' @param simp 
#' @param dataname  
#' @return vector of changes (integer)
#' 
#' @author Sascha Holzhauer
#' @export
hl_raster_changes <- function(simp, dataname = "raster_landUseIndex") {
	# read raster maps if not stored
	if (!input_tools_checkexists(simp, dataname)) {
		raster_landUseIndex <- input_raster_output(simp,
				datatype = "Agent", 
				dataname = "SerialID",
				starttick = 2010)

		runids <- as.character(sapply(raster_landUseIndex, function(x) unique(x$Runid)))
		runids <- sapply(strsplit(runids, "-"), function(x) x[[1]])
		if (!is.null(simp$sim$rundesc) && all(runids %in% names(simp$sim$rundesc))) 
			names(raster_landUseIndex) <- simp$sim$rundesc[runids]
		input_tools_save(simp, "raster_landUseIndex")
	} else {
		input_tools_load(simp, dataname)
	}
	raster_aft <- get(dataname)[[1]]
	
	last = raster_aft$Raster[[1]]
	changenums = c()
	for (raster in raster_aft$Raster[2:length(raster_aft$Raster)]) {
		# raster <- raster_aft$Raster[2]
		changes <- (raster[[1]] - last)
		changes[changes != 0] <- 1
		changenums <-  c(changenums, raster::cellStats(changes, "sum"))
		last <- raster[[1]]
	}
	#raster::plot(changes)
	return(changenums)
}