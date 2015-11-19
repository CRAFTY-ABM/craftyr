#' Extracts cell coordinates and land use from stored cell data and generates raster file
#' 
#' @param simp 
#' @param tick
#' @param dataname 
#' @param landuseindexcolname 
#' @param naflag 
#' @return raster file
#' 
#' @author Sascha Holzhauer
#' @export
hl_cell2raster <- function(simp, tick, dataname = "csv_LandUseIndex_rbinded", 
		landuseindexcolname = "LandUseIndex", naflag = -9) {
	input_tools_load(simp, dataname)
	
	data <- get(dataname)
	data <- data[data$Tick == tick, c(simp$csv$cname_x, simp$csv$cname_y, landuseindexcolname)]
	rownames(data) <- NULL
	
	raster <- craftyr::convert_2raster(simp, data, targetCRS = "+init=EPSG:32632", layers=c(1))
	
	filename <- sprintf("%s/%s_LandUseIndex_%d.%s",
			simp$dirs$output$raster,
			output_tools_getDefaultFilename(simp),
			as.integer(tick),
			"asc")
	shbasic::sh.ensurePath(filename, stripFilename = TRUE)
	raster::writeRaster(raster[[1]][[1]], filename = filename, format = "ascii", 
			NAflag=naflag, overwrite=TRUE)
}