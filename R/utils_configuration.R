#' Change column names of CSV files, e.g. capital files
#' @param sip 
#' @param indir 
#' @param outdir 
#' @param colname_old 
#' @param colname_new 
#' @param filepattern 
#' @return adjusted files
#' 
#' @author Sascha Holzhauer
#' @export
adjust_changecolumnnames <- function(sip,  indir = simp$dirs$param$getparamdir(simp, "capitals"),
		outdir = indir, colname_old, colname_new, filepattern = "*.csv") { 
	
	futile.logger::flog.info("Processing files in %s...",
			indir,
			name = "craftyr.utils.adjust.colnames")
	
	shbasic::sh.ensurePath(outdir)
	for (file in list.files(indir, pattern = filepattern)) {
		#file <- list.files(indir, pattern="*.csv")[1]
		
		futile.logger::flog.info("Processing %s...",
				file,
				name = "craftyr.utils.adjust.colnames")
		
		data <- read.csv(paste(indir, file, sep="/"))
		colnames(data)[colnames(data) == colname_old] <- colname_new
		
		outfile <- paste(outdir, file, sep="/")
		
		futile.logger::flog.info("Writing to %s...",
				outfile,
				name = "craftyr.utils.adjust.colnames")
		
		write.csv(data, outfile, row.names = FALSE)
	}
	futile.logger::flog.info("Processed %d files!",
			length(list.files(indir, pattern = filepattern)),
			name = "craftyr.utils.adjust.colnames")
}
#' Copy first line of CSV file and assign endtick.
#' 
#' @param sip 
#' @param indir 
#' @param outdir 
#' @param endtick 
#' @return altered CSV files
#' 
#' @author Sascha Holzhauer
#' @export
make_demand_static <- function(sip,  indir = simp$dirs$param$getparamdir(simp, "demand"),
		outdir = indir, endtick = simp$sim$endtick, tickcolumnname = "Year", filepattern = "*.csv") {
	
	futile.logger::flog.info("Processing files in %s...",
			indir,
			name = "craftyr.utils.demand.static")
	
	shbasic::sh.ensurePath(outdir)
	for (file in list.files(indir, pattern = filepattern)) {
		#file <- list.files(indir, pattern="*.csv")[1]
		
		futile.logger::flog.info("Processing %s...",
				file,
				name = "craftyr.utils.demand.static")
		
		data <- read.csv(paste(indir, file, sep="/"))
		
		data <- rbind(data[1,], data[1,])
		data[2, tickcolumnname] <- endtick
		
		outfile <- paste(outdir, file, sep="/")
		
		futile.logger::flog.info("Writing to %s...",
				outfile,
				name = "craftyr.utils.demand.static")
		
		write.csv(data, outfile, row.names = FALSE)
	}
	futile.logger::flog.info("Processed %d files!",
			length(list.files(indir, pattern = filepattern)),
			name = "craftyr.utils.demand.static")
}