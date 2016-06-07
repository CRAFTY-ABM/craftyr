#' Store CSV cell data (also in splitted form)
#' @param simp 
#' @param dataname 
#' @param tickinterval  the interval of ticks that are stored 
#' 		(if tickinterval = 10, e.g. data of 2010,2020,2030 will be stored)
#' 
#' @author Sascha Holzhauer
#' @export
hl_store_csvcelldata <- function(simp, dataname = "csv_LandUseIndex", tickinterval = 10) {
	
	cdata <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
			pertick = TRUE, 
			starttick = if(!is.null(simp$sim$starttick)) simp$sim$starttick else simp$tech$mintick,
			endtick = if(!is.null(simp$sim$endtick)) simp$sim$endtick else simp$tech$maxtick,
			tickinterval = tickinterval,
			attachfileinfo = TRUE, bindrows = TRUE)
	assign(dataname, cdata)
	input_tools_save(simp, dataname)
	
	cdata <- split(cdata, list(cdata$Tick,cdata$Runid))
	assign(paste(dataname, "split", sep="_"), cdata)
	input_tools_save(simp, paste(dataname, "split", sep="_"))
}
#' Outputs LandUseIndex data stored as RData object to CSV file(s).
#' 
#' Considers \code{simp$sim$regions}.
#' @param simp 
#' @param dataname 
#' @param filename 
#' @param columns 
#' @param pertick 
#' @param outdir
#' @param identifier part of filename
#' @param useIndices if FALSE, full AFT names will be written
#' @param writeLegend writes a separate CSV file containing the land use index legend
#' @param zip zip CSV file (apart from legend file)
#' @param removeCSV removes CSV file
#' @param tempdir if given, the CSV file will be stored here before zipped to outdir
#' 
#' @return CSV file(s) 
#' 
#' @author Sascha Holzhauer
#' @export
hl_write_csv_landuseindex <- function(simp, dataname = "csv_LandUseIndex_rbinded", filename=NULL, 
		columns = c(simp$csv$cname_x, simp$csv$cname_y, "LandUseIndex"), pertick = TRUE,
		identifier = paste(simp$sim$version, simp$sim$scenario, sep="_"),
		useIndices = TRUE, writeLegend = TRUE, zip = TRUE, removeCSV = zip,
		outdir = simp$dirs$output$reports, tempdir = NULL, ...) {

	#simp$sim$regions				<- c("BE", "LU")
	
	input_tools_load(simp, dataname)
	data <- get(dataname)
	rownames(data) <- NULL
	
	
	landUseData	<- data[data$Region %in% simp$sim$regions, ]
	
	if (!pertick) {
		columns <- c(columns, "Tick")
	} else {
		landUseData <- plyr::dlply(landUseData, "Tick", function(x) {
					x[,columns]
				})
	}
	
	if (!useIndices) {
		landUseData$Landmanager <- as.factor(as.factor(simp$mdata$aftNames[as.character(landUseData$LandUseIndex)]))
		landUseData$LandUseIndex <- NULL
	}
	
	shbasic::sh.ensurePath(outdir)
	if (!is.null(tempdir)) shbasic::sh.ensurePath(tempdir)
	
	for (i in 1 : length(landUseData)) {
		luData <- landUseData[[i]]
		tickid <- if (pertick) names(landUseData)[i] else unique(luData$Tick) 
		
		csvFile <- if (!is.null(tempdir)) 
			paste(tempdir, "/LandUseData_", identifier, "_", tickid, ".csv", sep="")
		else 
			paste(outdir, "/LandUseData_", identifier, "_", tickid, ".csv", sep="") 
		write.csv(luData, csvFile)
		if (zip) {
			zip(paste(outdir, "/LandUseData_", identifier, "_", tickid, ".zip", sep=""), csvFile, flags = "-r9Xj")
		}
	}
	
	if(writeLegend) {
		write.csv(data.frame("Index" = names(simp$mdata$aftNames), "LandManager"= simp$mdata$aftNames), 
				paste(outdir, "/LandUseIndexLegend.csv", sep=""), row.names=FALSE)
	}
}
