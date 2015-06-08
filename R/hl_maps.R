#' AFT map for 2010 and 2040 with differences from stored CSV data
#' @param simp 
#' @param dataname name of stored CSV LandUseIndex data
#' @return map plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftmap <- function(simp, dataname = "csv_LandUseIndex_rbinded",
		firsttick = 2010, secondtick = 2040, ncol = 1, ggplotaddon = NULL) {
	input_tools_load(simp, dataname)
	cdata <- get(dataname)
	
	cdata$Region <- NULL
	cdata$Scenario <- NULL
	
	runid <- unique(cdata$Runid)
	
	if (length(runid) > 1) {
		futile.logger::flog.warn("More than one runid defined. Using %s.", as.character(runid[1]),
				name="craftyr.hl.maps")
		runid <- runid[1]
	}
	
	cdata <- split(cdata, list(cdata$Tick,cdata$Runid))
	
	cdata <- lapply(cdata, function(x) {x$Runid <- NULL; x})
	cdata <- lapply(cdata, function(x) {x$Tick <- NULL; x})
	
	toplot <- list()

	diffcells <- cdata[[1]]
	diffcells$LandUseIndex <- NA
		
	first <- cdata[[paste(firsttick, runid, sep=".")]]
	second <- cdata[[paste(secondtick, runid, sep=".")]]
	diffcells$LandUseIndex[first$LandUseIndex - second$LandUseIndex != 0] <- 3
		
	first$ID <- as.factor(paste(runid, " (", firsttick,")", sep=""))
	second$ID <- as.factor(paste(runid, " (", secondtick,")", sep=""))
	diffcells$ID <- as.factor(paste(runid, " (", secondtick, "-", firsttick, ")", sep=""))
		
	toplot <- c(toplot, list(first), list(second), list(diffcells))
	
	visualise_cells_printPlots(simp, toplot, idcolumn = "ID",
			title = "EU-Hetero", legendtitle = "AFTs",
			factorial= TRUE, omitaxisticks = TRUE, ncol = ncol,
			legenditemnames = simp$mdata$aftNames, coloursetname="AFT",
			ggplotaddon = ggplotaddon)
}
#' AFT map for 2010 and 2040 with differences (placed horicontally) for multiple Runids
#' @param simp 
#' @param dataname 
#' @return map plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftmap_multi <- function(simp, dataname = "csv_LandUseIndex_rbinded",
		firsttick = 2010, secondtick = 2040) {
	
	input_tools_load(simp, dataname)
	cdata <- get(dataname)
	
	cdata$Region <- NULL
	cdata$Scenario <- NULL
	
	runids <- unique(cdata$Runid)
	cdata <- split(cdata, list(cdata$Tick,cdata$Runid))
	
	cdata <- lapply(cdata, function(x) {x$Runid <- NULL; x})
	cdata <- lapply(cdata, function(x) {x$Tick <- NULL; x})
	
	toplot <- list()
	for (runid in runids) {
		diffcells <- cdata[[1]]
		diffcells$LandUseIndex <- NA
		
		first <- cdata[[paste(firsttick, runid, sep=".")]]
		second <- cdata[[paste(secondtick, runid, sep=".")]]
		diffcells$LandUseIndex[first$LandUseIndex - second$LandUseIndex != 0] <- 3
		
		first$ID <- as.factor(paste(runid, " (", firsttick,")", sep=""))
		second$ID <- as.factor(paste(runid, " (", secondtick,")", sep=""))
		diffcells$ID <- as.factor(paste(runid, " (", secondtick, "-", firsttick, ")", sep=""))
		
		toplot <- c(toplot, list(first), list(second), list(diffcells))
	}
	
	visualise_cells_printPlots(simp, toplot, idcolumn = "ID",
			title = "EU-Hetero", legendtitle = "AFTs",
			factorial= TRUE, omitaxisticks = TRUE, ncol = 3,
			legenditemnames = simp$mdata$aftNames, coloursetname="AFT")
}
