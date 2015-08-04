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
#' Plot changes between ticks for a specific (combination of) land use(s).
#' @param simp 
#' @param dataname 
#' @param selectedAFT 
#' @param starttick 
#' @param endtick 
#' @param ncol 
#' @param ggplotaddon 
#' @return figure file / plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftmap_changes <- function(simp, dataname = "csv_LandUseIndex_rbinded", selectedAFT = 1,
		starttick = 2010, endtick = 2040, ncol = 1, title = "AFT-Changes", ggplotaddon = NULL) {

#	# <--- test data:
#	simp$sim$worldname 			<- "world"
#	simp$sim$scenario			<- "scenario"
#	simp$sim$regionalisation	<- "regionalisation"
#	simp$sim$regions			<- c("region")
#	simp$sim$runids				<- c("0-0")
#	simp$sim$hasregiondir			<- TRUE
#	simp$sim$filepartorder			<- c("scenario", "D", "runid", "D", "regions", "D", 
#			"datatype", "D", "dataname", "D", "tick")
#	
#	simp$dirs$output$rdata		<- "C:/Data/LURG/workspace/craftyr/inst/extdata/output/version/rData/"
#	simp$dirs$output$simulation	<- "C:/Data/LURG/workspace/craftyr/inst/extdata/output/version/simulation/"
#	
#	cdata <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
#			pertick = TRUE, starttick = 2000, endtick = 2020, tickinterval = 10,
#			attachfileinfo = TRUE, bindrows = TRUE)
#	rownames(cdata) <- NULL
#	### test data --->
	
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
	
	cdata <- cdata[cdata$Tick >= starttick && cdata$Tick <= endtick, ]
	
	cdata[cdata$LandUseIndex %in% selectedAFT, "LandUseIndex"]  <- 100
	cdata[!cdata$LandUseIndex %in% 100, "LandUseIndex"] <- 0 

	cdata <- split(cdata, list(cdata$Tick,cdata$Runid))
	
	cdata <- lapply(cdata, function(x) {x$Runid <- NULL; x})
	cdata <- lapply(cdata, function(x) {x$Tick <- NULL; x})
	
	toplot <- list()


	resultcells <- list()
	for (i in 2:length(names(cdata))) {
		diffcells <- cdata[[1]]
		diffcells$LandUseIndex <- NA
		diffcells$LandUseIndex <- cdata[[i - 1]]$LandUseIndex - cdata[[i]]$LandUseIndex
		diffcells$LandUseIndex[diffcells$LandUseIndex == 0] <- 5
		diffcells <- list(diffcells)
		names(diffcells) <- paste(names(cdata[i - 1]), "/", names(cdata[i])) 
		resultcells <- c(resultcells, diffcells)
	}
	
	simp$colours$changes <- c("-100" = "red", "100" = "green", "5" = "white")
	
	visualise_cells_printPlots(simp, resultcells, idcolumn = "ID",
			title = title, legendtitle = "AFTs",
			factorial= TRUE, omitaxisticks = TRUE, ncol = ncol,
			legenditemnames = c("-100" = "removed", "100" = "added"), coloursetname="changes",
			ggplotaddon = ggplotaddon)
}
