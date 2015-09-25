#' Convert X and Y coordinates from a CSV file into a shapefile
#' 
#' NOTE: not tested yet
#' @param sip 
#' @param indir 
#' @param outdir 
#' @param countryCodesFile 
#' @return shapefile
#' 
#' @author Sascha Holzhauer
#' @export
convert_csv2shapefile <- function(sip, indir, outdir, 
		countryCodesFile = system.file("extdata", "various/CountryCodeNumberMapping.csv", package="crafty")) {
	
	countryCodes <- read.csv(countryCodesFile)

	for (file in list.files(indir, pattern="*.csv")) {
		futile.logger::flog.info("Processing %s...",
					file,
					name = "craftyr_convert2shapefile")
		
		data <- read.csv(paste(indir, file, sep="/"))
		data <- data[,c(simp$csv$cname_x, simp$csv$cname_y)]
		data$z = 1
		r <- raster::rasterFromXYZ(data)
		p <- raster::asterToPolygons(r, dissolve=TRUE)
		
		filenumber <- regmatches(file, regexpr("\\d+", file))
		outfile <- paste(outdir,countryCodes[countryCodes$Number == filenumber & 
								!is.na(countryCodes$Number), "Code"], sep="/")
		
		futile.logger::flog.info("Writing to %s...",
				outfile,
				name = "craftyr_convert2shapefile")
		
		maptools::writePolyShape(p, fn= outfile)
	}
	futile.logger::flog.info("Finished converting!",
			name = "craftyr_convert2shapefile")
}