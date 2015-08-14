#' Generates a ggplot2 addon to draw (country) shapes
#' @param simp 
#' @param filename_shapes  (Default: shapes from Calum > less accurate than shapes from raster but much smaller)
#' @return ggplot2 addon
#' 
#' @seealso http://spatial.ly/2013/12/introduction-spatial-data-ggplot2/
#' 
#' @author Sascha Holzhauer
#' @export
input_shapes_countries <- function(simp, 
		filename_shapes = system.file("extdata", "various/europeshapes/Europe_Countries_Final_Projection.shp", 
				package = "craftyr"), countries2show = NULL, countrycodedatacolumn = "ISO_CC") {
	
	countries = read.csv(system.file("extdata", 
					"various/CountryCodeNumberMapping.csv", 
					package = "craftyr"))
	
	if (is.null(countries2show)) {
		countries2show <- countries$Code
	}
	
	# Shapes from 2nd tiff raster (works):
	#filename_shapes = "C:/Data/LURG/Projects/Volante/InputData/shp/raster2countries/countries.shp"
	#shapes <- rgdal::readOGR(dsn = filename_shapes, "countries")	
	#shapes <- shapes[countries[match(shapes@data$DN,countries$Number), "Code"] %in% countries2show,]
	
	shapes <- rgdal::readOGR(dsn = filename_shapes, tools::file_path_sans_ext(basename(filename_shapes)))	
	shapes <- shapes[shapes@data[[countrycodedatacolumn]] %in% countries2show,]
	
	## Eurostat (does not work without conversion)
	#filename_shapes <- "C:/Data/LURG/Projects/Volante/InputData/shp/shapes/eurostat/CNTR_2014_03M_SH/Data/CNTR_RG_03M_2014.shp"
	#shapes <- rgdal::readOGR(dsn = filename_shapes, "CNTR_RG_03M_2014")
	#shapes <- shapes[shapes@data$CNTR_ID %in% "NL",]
	
	## http://www.gadm.org/download  (does not work without conversion)
	#filename_shapes <- "C:/Data/LURG/Projects/Volante/InputData/shp/shapes/gadm/wgs84/NLD_adm0.shp"
	#shapes <- rgdal::readOGR(dsn = filename_shapes, "NLD_adm0")
	#shapes <- shapes[shapes@data$ISO2 %in% "NL",]
	
	### NL from tiff raster
	#filename_shapes <- "C:/Data/LURG/Projects/Volante/InputData/shp/sandbox/rastervector/NL2.shp"
	#shapes <- rgdal::readOGR(dsn = filename_shapes, "NL2")
	#shapes.f <- ggplot2::fortify(shapes, region = "DN")
	###

	# conversion (does not work! - better transform shapes into LAEA 1989 before):
	#shapes.etrs.1989.laea <- spTransform(shapes, CRS(
	#	"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
	#shapes.etrs.1989.laea <- spTransform(shapes, CRS("+init=epsg:3035"))
	#shapes.f <- ggplot2::fortify(shapes.etrs.1989.laea) #, region = "ISO2")
	
	shapes.f <- ggplot2::fortify(shapes) #, region = "ISO2")
	shapes.f$long <- (shapes.f$long + 2698874) / 1000
	shapes.f$lat <- (shapes.f$lat + 1855465) / 1000
	
	g <- ggplot2::geom_path(data=shapes.f, ggplot2::aes(long, lat, group = group), 
			colour="darkgrey", size = 0.5)
	
	return(g)
}