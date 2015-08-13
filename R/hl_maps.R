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
hl_aftmap_changes_temporal <- function(simp, dataname = "csv_LandUseIndex_rbinded", selectedAFT = 1, regions = simp$sim$regions,
		starttick = 2010, endtick = 2040, ncol = 1, title = "AFT-Changes", ggplotaddon = NULL, 
		addcountryshapes = FALSE, plotunchanged = TRUE) {

#	# <--- test data:
#	simp <- param_getExamplesSimp()
#	starttick = 2000
#	endtick = 2020
#	
#	cdata <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
#			pertick = TRUE, starttick = 2000, endtick = 2020, tickinterval = 10,
#			attachfileinfo = TRUE, bindrows = TRUE)
#	rownames(cdata) <- NULL
#	### test data --->
	
	input_tools_load(simp, dataname)
	cdata <- get(dataname)

	ir <-cdata[cdata$Region %in% "IR", ]
	cdata <- cdata[cdata$Region %in% regions, ]
	
	#regions <- c("NL")
	#simp$sim$id 					<- "EU28-1"
	#cdata_test2 <- cdata
	#input_tools_save(simp, "cdata_test2")
	#input_tools_load(simp, "cdata_test2")
	#cdata <- cdata_test2
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

	hl_aftmap_changes(simp, cdata, ncol = 1, title = "AFT-Changes", ggplotaddon = NULL, 
			addcountryshapes = FALSE, plotunchanged = TRUE)
}
#' Plot changes between rusn of a specific tick for a particular (combination of) land use(s).
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
hl_aftmap_changes_runs <- function(simp, dataname = "csv_LandUseIndex_rbinded", ids, outdirs, 
		selectedAFTGroups = as.list(1:length(simp$mdata$aftNames)), regions = simp$sim$regions,
		tick = 2040, ncol = 1, title = "AFT-Changes", ggplotaddon = NULL, 
		addcountryshapes = FALSE, plotunchanged = TRUE) {
	
	# <--- test data:
#	simp <- craftyr::param_getExamplesSimp()
#	starttick = 2000
#	endtick = 2020
#	tick = 2020
#	
#	simp$sim$runids					<- c("0-0")
#	simp$sim$id <- "Example-0-0"
#	csv_LandUseIndex_rbinded <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
#			pertick = TRUE, starttick = 2000, endtick = 2020, tickinterval = 10,
#			attachfileinfo = TRUE, bindrows = TRUE)
#	rownames(csv_LandUseIndex_rbinded) <- NULL
#	input_tools_save(simp, "csv_LandUseIndex_rbinded")
#	
#	simp$sim$runids					<- c("1-0")
#	simp$sim$id <- "Example-1-0"
#	csv_LandUseIndex_rbinded <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
#			pertick = TRUE, starttick = 2000, endtick = 2020, tickinterval = 10,
#			attachfileinfo = TRUE, bindrows = TRUE)
#	rownames(csv_LandUseIndex_rbinded) <- NULL
#	input_tools_save(simp, "csv_LandUseIndex_rbinded")
#	
#	ids <- c("Example-0-0", "Example-1-0")
#	outdirs <- c(simp$dirs$output$rdata, simp$dirs$output$rdata)
	### test data --->
	
	cdata <- list()
	for (i in 1:length(ids)) {
		simp$sim$id <- ids[i]
		simp$dirs$output$rdata <- outdirs[i]
		
		input_tools_load(simp, dataname)
		dat <- get(dataname)
		dat <- dat[dat$Region %in% regions, ]
		dat$Region <- NULL
		dat$Scenario <- NULL
		dat <- dat[dat$Tick == tick, ]
		
		dat <- list(dat)
		names(dat) <- ids[i]
		cdata <- c(cdata, dat)
	}
	
	cdata <- lapply(cdata, function(x) {x$Runid <- NULL; x})
	cdata <- lapply(cdata, function(x) {x$Tick <- NULL; x})
	
	for (aftgroup in selectedAFTGroups) {
		# aftgroup <- selectedAFTGroups[1]
		aftgroup <- aftgroup[[1]]
		cdata <- sapply(cdata, function(dat) {
					# dat <- cdata[[1]]
					dat[dat$LandUseIndex %in% aftgroup, "LandUseIndex"]  <- 100
					dat[!dat$LandUseIndex %in% 100, "LandUseIndex"] <- 0 
					dat}, simplify=FALSE)
		
		hl_aftmap_changes(simp, cdata, ncol = 1, 
				title = paste(title, "_", paste(simp$mdata$aftNames[aftgroup], collapse="-"), sep=""), 
				ggplotaddon = NULL, 
			addcountryshapes = addcountryshapes, plotunchanged = plotunchanged)
	}
}
#' Plot changes between ticks for a specific (combination of) land use(s).
#' @param simp 
#' @param cdata list of celldata to compare
#' @param ncol 
#' @param ggplotaddon 
#' @return figure file / plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftmap_changes <- function(simp, cdata, ncol = 1, title = "AFT-Changes", ggplotaddon = NULL, 
		addcountryshapes = FALSE, plotunchanged = TRUE) {
	
	resultcells <- list()
	for (i in 2:length(cdata)) {
		diffcells <- cdata[[1]]
		diffcells$LandUseIndex <- NA
		indices <- cdata[[i]]$LandUseIndex != 0 | cdata[[i-1]]$LandUseIndex != 0
		diffcells$LandUseIndex[indices] <- (cdata[[i]]$LandUseIndex - cdata[[i - 1]]$LandUseIndex)[indices]
		
		diffcells$LandUseIndex[diffcells$LandUseIndex == 0] <- if (plotunchanged) 50 else 99
		diffcells$LandUseIndex[is.na(diffcells$LandUseIndex)] <- 99
		diffcells$ID <- paste(names(cdata[i - 1]), "/", names(cdata[i]))
		
		diffcells <- list(diffcells)
		names(diffcells) <- paste(names(cdata[i - 1]), "/", names(cdata[i]))
		
		resultcells <- c(resultcells, diffcells)
	}
	
	simp$colours$changes <- c("-100" = "red", "100" = "green", "50" = "black", "99" = "white")
	
	countryshapeelem = NULL
	if(addcountryshapes) {
		countryshapeelem <- input_shapes_countries(simp, countries2show = regions)
	}
	visualise_cells_printPlots(simp, celldata = resultcells, idcolumn = "ID",
			title = title, legendtitle = "AFTs",
			factorial= TRUE, omitaxisticks = TRUE, ncol = ncol,
			legenditemnames = c("-100" = "removed", "100" = "added", "50" = "remaining", "99" = "other"), 
			coloursetname="changes",
			ggplotaddon = list(ggplotaddon, countryshapeelem, ggplot2::coord_equal()))
}