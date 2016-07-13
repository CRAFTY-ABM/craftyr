#' AFT map for 2010 and 2040 with differences from stored CSV data
#' 
#' @param simp 
#' @param dataname name of stored CSV LandUseIndex data (data.frame)
#' @param returnplot if true the ggplot object is returned
#' @return map plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftmap <- function(simp, dataname = "csv_LandUseIndex_rbinded",
		firsttick = 2010, secondtick = 2040, ncol = 1, ggplotaddon = NULL, returnplot = FALSE) {
	input_tools_load(simp, dataname)
	cdata <- get(dataname)
	
	cdata$Region <- NULL
	cdata$Scenario <- NULL
	
	runid <- unique(cdata$Runid)

	if (!firsttick %in% cdata$Tick) {
		futile.logger::flog.error("Data does not contain first tick (%d)!",
				secondtick,
				name = "craftyr.hl_maps.R")
	}
	
	if (!secondtick %in% cdata$Tick) {
		futile.logger::flog.error("Data does not contain second tick (%d)!",
				firsttick,
				name = "craftyr.hl_maps.R")
	}
	
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
	
	p1 <- visualise_cells_printPlots(simp, toplot, idcolumn = "ID",
			title = simp$fig$maptitle, legendtitle = "AFTs",
			factorial= TRUE, omitaxisticks = TRUE, ncol = ncol,
			legenditemnames = simp$mdata$aftNames, coloursetname="AFT",
			ggplotaddon = ggplotaddon)
	if (returnplot) return(p1)
}
#' AFT map for 2010 and 2040 with differences (placed horicontally) for multiple Runids
#' 
#' @param simp 
#' @param dataname 
#' @param returnplot if true the ggplot object is returned
#' @return map plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftmap_multi <- function(simp, dataname = "csv_LandUseIndex_rbinded",
		firsttick = 2010, secondtick = 2040, returnplot= FALSE) {
	
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
	
	p1 <- visualise_cells_printPlots(simp, toplot, idcolumn = "ID",
			title = "LandUseMap", legendtitle = "AFTs",
			factorial= TRUE, omitaxisticks = TRUE, ncol = 3,
			legenditemnames = simp$mdata$aftNames, coloursetname="AFT")
	if (returnplot) return(p1)
}
#' Plot changes between ticks for a specific (combination of) land use(s)
#' 
#' @param simp 
#' @param dataname 
#' @param selectedAFT 
#' @param starttick 
#' @param endtick 
#' @param ncol 
#' @param ggplotaddon 
#' @param returnplot if true the ggplot object is returned
#' @return figure file / plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftmap_changes_temporal <- function(simp, dataname = "csv_LandUseIndex_rbinded", selectedAFT = 1, 
		regions = simp$sim$regions, starttick = 2010, endtick = 2040, ncol = 1, title = "AFT-Changes", 
		ggplotaddon = NULL, addcountryshapes = FALSE, plotunchanged = TRUE, returnplot = FALSE) {

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

	hl_aftmap_changes(simp, cdata, ncol = ncol, title = title, ggplotaddon = ggplotaddon, regions = regions,
			addcountryshapes = addcountryshapes, plotunchanged = plotunchanged, returnplot = returnplot)
}
#' Plot changes between runs of a specific tick for a particular (combination of) land use(s)
#' 
#' @param simp 
#' @param dataname 
#' @param selectedAFT 
#' @param starttick 
#' @param endtick 
#' @param ncol 
#' @param ggplotaddon 
#' @param returnplot if true the ggplot object is returned
#' @return figure file / plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftmap_changes_runs <- function(simp, dataname = "csv_LandUseIndex_rbinded", ids, outdirs, 
		selectedAFTGroups = as.list(as.numeric(names(simp$mdata$aftNames))), regions = simp$sim$regions,
		tick = 2040, ncol = 1, title = paste(title, "_", paste(simp$mdata$aftNames[as.character(aftgroup)],
		collapse="-"), sep=""), ggplotaddon = NULL, 
		addcountryshapes = FALSE, plotunchanged = TRUE, returnplot = FALSE) {
	
	# <--- test data:
#	simp <- craftyr::param_getExamplesSimp()
#	tick = 2020
#	dataname = "csv_LandUseIndex_rbinded"
#	regions =  simp$sim$regions
#	addcountryshapes = FALSE # TODO configure input_shapes_countries for test data
#	plotunchanged = TRUE
#	selectedAFTGroups = as.list(as.numeric(names(simp$mdata$aftNames)))
#	title = "AFT-Changes-Test"
#	ggplotaddon = NULL
#	ncol = 1
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
#	
#	simp$fig$init			<- craftyr::output_visualise_initFigure
	### test data --->
	
	#regions = c("SE")
	#cdata1 <- csv_LandUseIndex_rbinded <- cdata1
	
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
		#aftgroup <- selectedAFTGroups[6]
		aftgroup <- aftgroup[[1]]
		cdata_aft <- sapply(cdata, function(dat) {
					# dat2 <- cdata[[2]]
					dat[dat$LandUseIndex %in% aftgroup, "LandUseIndex"]  <- 100
					dat[!dat$LandUseIndex %in% 100, "LandUseIndex"] <- 0 
					dat}, simplify=FALSE)
		
		hl_aftmap_changes(simp, cdata_aft, ncol = ncol, 
				title = title, 
				ggplotaddon = ggplotaddon, regions = regions,
			addcountryshapes = addcountryshapes, plotunchanged = plotunchanged, returnplot = returnplot)
	}
}
#' Plot changes between ticks for a specific (combination of) land use(s)
#' 
#' @param simp 
#' @param cdata list of celldata (\code{data.frame)} to compare
#' @param ncol 
#' @param ggplotaddon
#' @param returnplot if true the ggplot object is returned
#' @return figure file / plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftmap_changes <- function(simp, cdata, ncol = 1, title = "AFT-Changes", ggplotaddon = NULL, 
		regions = simp$sim$regions,
		addcountryshapes = FALSE, plotunchanged = TRUE, returnplot = FALSE) {
	
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
	p1 <- visualise_cells_printPlots(simp, celldata = resultcells, idcolumn = "ID",
			title = title, legendtitle = "AFTs",
			factorial= TRUE, omitaxisticks = TRUE, ncol = ncol,
			legenditemnames = c("-100" = "removed", "100" = "added", "50" = "remaining", "99" = "other"), 
			coloursetname="changes",
			theme = if (simp$fig$plottitle) visualisation_raster_legendandtitle else visualisation_raster_legendonlytheme,
			ggplotaddon = list(ggplotaddon, countryshapeelem, ggplot2::coord_equal()))
	if (returnplot) return(p1)
}
#' Ggplot2 theme that plots legend and title only
#' 
#' @param base_size 
#' @param base_family 
#' @return ggplot theme
#' 
#' @author Sascha Holzhauer
visualisation_raster_legendandtitle <- function(base_size = 11, base_family = "Helvetica"){
	library(ggplot2)  # correct (see stack exchange question) for %+replace%
	ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
			ggplot2::theme(
					panel.background = ggplot2::element_blank(),
					panel.grid.major = ggplot2::element_blank(),
					panel.grid.minor = ggplot2::element_blank(),
					panel.margin = grid::unit(0,"lines"),
					panel.border = ggplot2::element_blank(),
					plot.margin = grid::unit(rep(0,4),"lines"),
					axis.ticks = ggplot2::element_blank(),
					axis.text.x = ggplot2::element_blank(),
					axis.text.y = ggplot2::element_blank(),
					axis.title.x = ggplot2::element_blank(),
					axis.title.y = ggplot2::element_blank(),
					axis.line = ggplot2::element_blank(),
					axis.ticks.length = grid::unit(0,"null"),
					axis.text = ggplot2::element_text(margin = grid::unit(0,"null"))
			)
}