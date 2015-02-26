#' Reads in CSV files with coordinates and creates polygone shapefiels via raster.
#' Considers only CSV fiels in the indir. They must have a "x" and "y" column, but not "z"!
#' @param indir 
#' @param outdir
#' @param filenamesub table with columns "number" and "sub" where the number must be contained in the original filename
#' @return polygone shapefiles
#' 
#' @author Sascha Holzhauer
#' @export
convert_2vectorfile <- function(indir, outdir, filenamesub = NULL) {
	for (file in list.files(indir, pattern="*.csv")) {
		cat("Processing", file, "...\n")
		
		data <- read.csv(paste(indir, file, sep="/"))
		data <- data[,c("x","y")]
		data$z = 1
		r <- rasterFromXYZ(data)
		p <- rasterToPolygons(r, dissolve=TRUE)
		
		filenumber <- regmatches(file, regexpr("\\d+", file))
		outfile <- paste(outdir, if (!is.null(filenamesub)) filenamesub[filenamesub$Number == filenumber & 
								!is.na(filenamesub$Number), "sub"] else file, sep="/")
		
		cat("Writing to", outfile, "...\n")
		
		writePolyShape(p, fn= outfile)
	}
}