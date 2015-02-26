#' Convert data.frame with coordinates into raster object
#' @param simp SIMulation Properties
#' @param data list of data containing columns x and y and data colum(s)
#' @param layers vector or list of layer indices or names. Length may not exceed number of data columns.
#' @return list (e.g. regions) of lists (e.g. capitals) of raster obejcts
#' 
#' @author Sascha Holzhauer
#' @export
convert_2raster <- function(simp, data, targetCRS = "+init=EPSG:32632", layers=c(1)) {
	
	if(is.data.frame(data)) {
		data <- list(data)
	}
	
	data <- lapply(data, function(d) {sp::coordinates(d)=as.formula(paste("~", simp$csv$cname_x, "+", simp$csv$cname_y, 
						sep="")); d})
	data <- lapply(data, function(x) {sp::proj4string(x)=targetCRS; x})
	
	data <- lapply(data, function(x) {sp::spTransform(x, sp::CRS(targetCRS)); x})
	data <- lapply(data, function(x) {sp::gridded(x) = TRUE; x})
	
	rasters <- lapply(data, function(x) {
				lapply(layers, function(y) {
							raster::raster(x, layer=y)})
			})
	# seems to work rather without setting projection...:
	#rasters <- lapply(rasters, function(x) {raster::projection(x)=targetCRS; x})
	rasters
}