#' Generates a ggplot2 addon to draw (country) shapes
#' @param simp 
#' @param filename_shapes  
#' @return ggplot2 addon
#' 
#' @seealso http://spatial.ly/2013/12/introduction-spatial-data-ggplot2/
#' 
#' @author Sascha Holzhauer
#' @export
input_shapes_countries <- function(simp, 
		filename_shapes = system.file("extdata", "various/europeshapes/Europe_Countries_Final_Projection.shp", 
				package = "craftyr"), countries2show = NULL) {

	countries = read.csv(system.file("extdata", 
					"various/CountryCodeNumberMapping.csv", 
					package = "craftyr"))
	
	if (!is.null(countries2show)) {
		countries2show <- countries[countries$Code %in% countries2show, "Name"]
	} else {
		countries2show <- countries$Name
	}
	
	
	shapes <- rgdal::readOGR(dsn = filename_shapes, "Europe_Countries_Final_Projection")	
	shapes <- shapes[shapes@data$COUNTRY %in% countries2show,]
	
	shapes.etrs.1989.laea <- spTransform(shapes, CRS("+init=epsg:3035"))
	
	##
	filename_shapes <- "C:/Data/LURG/Projects/Volante/InputData/shp/shapes/eurostat/CNTR_2014_03M_SH/Data/CNTR_RG_03M_2014.shp"
	shapes <- rgdal::readOGR(dsn = filename_shapes, "CNTR_RG_03M_2014")
	shapes <- shapes[shapes@data$CNTR_ID %in% "NL",]
	shapes.etrs.1989.laea <- spTransform(shapes, CRS("+init=epsg:3035"))
	###
	
	# http://www.gadm.org/download
	filename_shapes <- "C:/Data/LURG/Projects/Volante/InputData/shp/shapes/gadm/NLD_adm0.shp"
	shapes <- rgdal::readOGR(dsn = filename_shapes, "NLD_adm0")
	shapes <- shapes[shapes@data$ISO2 %in% "NL",]
	shapes.etrs.1989.laea <- spTransform(shapes, CRS("+init=epsg:3035"))
	
	load(filename_rdat)
	
	###
	filename_shapes <- "C:/Data/LURG/Projects/Volante/InputData/shp/sandbox/rastervector/NL2.shp"
	shapes <- rgdal::readOGR(dsn = filename_shapes, "NL2")
	shapes.f <- ggplot2::fortify(shapes, region = "DN")
	
	###
	shapes.f <- ggplot2::fortify(shapes.etrs.1989.laea, region = "ISO2")
	shapes.f$long <- (shapes.f$long - 2478000) / 1000
	shapes.f$lat <- (shapes.f$lat - 1178000) / 1000

	proj4string(shapes)
	
	g <- ggplot2::geom_path(data=shapes.f, ggplot2::aes(long, lat, group = group), 
			colour="orange", size = 0.8)
	
	countryshapeelem <- g
	return(g)
}