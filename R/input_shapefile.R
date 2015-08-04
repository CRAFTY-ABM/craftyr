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
		countries2show <- countries[countries$code %in% countries, "Name"]
	} else {
		countries2show <- countries$Name
	}
	
	shapes <- maptools::readShapePoly(filename_shapes)
	shapes <- shapes[shapes$COUNTRY %in% countries2show,]
	shapes.f <- ggplot2::fortify(shapes, region = "COUNTRY")

	g <- ggplot2::geom_path(data=shapes.f, ggplot2::aes(long, lat, group = group), 
			colour="lightgrey")
	return(g)
}