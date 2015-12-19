## ---- eval=TRUE, results="hide"------------------------------------------
library(craftyr)
simp <- param_getExamplesSimp()

futile.logger::flog.threshold(futile.logger::DEBUG, name='craftyr.input.csv')

cdata <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
		pertick = TRUE, starttick = 2010, endtick = 2020, tickinterval = 10,
		attachfileinfo = TRUE, bindrows = TRUE)
csv_LandUseIndex_rbinded <- cdata
input_tools_save(simp, "csv_LandUseIndex_rbinded")
cdata <- split(cdata, list(cdata$Tick, cdata$Runid))
csv_LandUseIndex_split <- cdata
input_tools_save(simp, "csv_LandUseIndex_split")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_store_csvcelldata(simp, dataname = "csv_LandUseIndex_rbinded", tickinterval = 10)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  cdata <- input_csv_data(simp, dataname = NULL, datatype = "Cell", colums = "LandUseIndex",
#  		pertick = TRUE, starttick = 2010, endtick = 2040, tickinterval = 30, attachfileinfo = TRUE)
#  
#  cdata$Region <- NULL
#  cdata$Scenario <- NULL
#  
#  cdata <- split(cdata, list(cdata$Tick,cdata$Runid))
#  
#  cdata <- lapply(cdata, function(x) {x$Runid <- NULL; x})
#  cdata <- lapply(cdata, function(x) {x$Tick <- NULL; x})
#  
#  csv_LandUseIndex_split <- cadata
#  input_tools_save(simp, "csv_LandUseIndex_split")
#  rm(list(csv_LandUseIndex_split, cdata)

## ---- eval=TRUE, results="hide"------------------------------------------
aggregationFunction <- function(simp, data) {
	library(plyr) # because '.' function cannot be addressed
	plyr::ddply(data, .(Runid, Region, Tick, LandUseIndex), .fun=function(df) {
				df$Counter <- 1
				with(df, data.frame(
								Runid				= unique(Runid),
								Region				= unique(Region),
								Tick				= mean(Tick),
								LandUseIndex		= mean(LandUseIndex),
								AFT					= sum(Counter),
								Service.Service1	= sum(Service.Service1), 
								Service.Service2	= sum(Service.Service2),
								Service.Service3 	= sum(Service.Service3),
								Capital.Cap1		= sum(Capital.Cap1)) )
			})
}

csv_cell_aggregated <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = c("Service.Service1", "Service.Service2",
				"Service.Service3", "Capital.Cap1", "LandUseIndex", "AFT"), pertick = TRUE,
		starttick = 2000, endtick = 2020, tickinterval = 10,
		attachfileinfo = TRUE, bindrows = TRUE,
		aggregationFunction = aggregationFunction,
		skipXY = TRUE)
rownames(csv_cell_aggregated) <- NULL
input_tools_save(simp, "csv_cell_aggregated")

## ---- eval=FALSE---------------------------------------------------------
#  library(craftyr)
#  simp <- param_getExamplesSimp()
#  simp$sim$filepartorder	<- c("regions", "D", "datatype")
#  csv_preAllocTable <- input_csv_prealloccomp(simp)
#  input_tools_save(simp, "csv_preAllocTable")

## ---- eval=FALSE---------------------------------------------------------
#  library(craftyr)
#  simp <- param_getExamplesSimp()
#  dataAggregateAFTComposition <- input_csv_data(simp, dataname = NULL, datatype = "AggregateAFTComposition",
#  		pertick = FALSE,
#  		bindrows = TRUE,
#  		skipXY = TRUE)
#  input_tools_save(simp, "dataAggregateAFTComposition")

## ---- eval=TRUE, results="hide"------------------------------------------
library(craftyr)
simp <- param_getExamplesSimp()
csv_aggregateServiceDemand <- input_csv_data(simp, dataname = NULL, datatype = "AggregateServiceDemand",
		pertick = FALSE, bindrows = TRUE)
input_tools_save(simp, "csv_aggregateServiceDemand")

## ---- eval=TRUE, results="hide"------------------------------------------
library(craftyr)
simp <- param_getExamplesSimp()
csv_aggregateTakeOvers <- input_csv_data(simp, dataname = NULL, datatype = "TakeOvers", pertick = FALSE,
		bindrows = TRUE,
		skipXY = TRUE)
input_tools_save(simp, "csv_aggregateTakeOvers")

## ---- eval=TRUE, results="hide"------------------------------------------
library(craftyr)
simp <- param_getExamplesSimp()
csv_aggregateGiStatistics <- input_csv_data(simp, dataname = NULL, datatype = "GivingInStatistics",
		pertick = FALSE,
		bindrows = TRUE,
		skipXY = TRUE)
input_tools_save(simp, "csv_aggregateGiStatistics")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  library(craftyr)
#  simp <- param_getExamplesSimp()
#  csv_MarginalUtilitites_melt <- input_marginalutilities(simp)
#  input_tools_save(simp, "csv_MarginalUtilitites_melt")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  simp <- param_getDefaultSimp()
#  input_tools_load(simp, "csv_LandUseIndex_split")
#  cdata <- csv_LandUseIndex_split
#  cdata$"2010.2"$ID <<- as.factor("Homo_2010")
#  cdata$"2040.2"$ID <<- as.factor("Homo_2040")
#  
#  diffcells <<- cdata$"2010.2"
#  diffcells$LandUseIndex <<- NA
#  diffhomo$LandUseIndex[cdata$"2040.2"$LandUseIndex - cdata$"2010.2"$LandUseIndex != 0] <- 3
#  diffhomo$ID <- as.factor("Diff_Homo_2040-2010")
#  
#  toplot <- list(cdata$"2010.2", cdata$"2040.2", diffhomo)
#  rm(diffhomo)
#  rm(cdata)
#  visualise_cells_printPlots(simp, toplot, idcolumn = "ID",
#  		title = "EU-Homo", legendtitle = "AFTs",
#  		factorial= TRUE, omitaxisticks = TRUE, ncol = 3,
#  		legenditemnames = simp$mdata$aftNames, coloursetname="AFT")

## ---- eval=TRUE, dev="png", fig.show='hold', results="hide"--------------
library(craftyr)
simp <- param_getExamplesSimp()
input_tools_load(simp, "csv_LandUseIndex_split")
visualise_cells_printRawPlots(simp, csv_LandUseIndex_split,
		factorial= TRUE, ncol = 1, id="None")		

## ---- eval=TRUE, dev="png", fig.width=7, fig.show='hold', results="hide"----
library(craftyr)
simp <- param_getExamplesSimp()
input_tools_load(simp, "csv_LandUseIndex_rbinded")

########### aggregate regions:
csv_LandUseIndex_rbinded$AftNumbers <- csv_LandUseIndex_rbinded$LandUseIndex
aftData <- aggregate(subset(csv_LandUseIndex_rbinded, select=c("AftNumbers")),
		by = list(ID = csv_LandUseIndex_rbinded[,"Runid"],
				Tick=csv_LandUseIndex_rbinded[, "Tick"],  AFT=csv_LandUseIndex_rbinded[,"LandUseIndex"]),
		FUN=sum)
aftData$AFT <- as.factor(aftData$AFT)
aftData$Proportion <- ave(aftData$AftNumbers, aftData$ID, aftData$Tick, FUN =  function(.x) .x/sum(.x))
aftData$Number <- NULL

visualise_lines(simp, aftData, "Proportion", title = "Total AFT composition",
		colour_column = "AFT",
		colour_legenditemnames = simp$mdata$aftNames,
		linetype_column = "ID",
		linetype_legendtitle = simp$sim$rundesclabel,
		linetype_legenditemnames = simp$sim$rundesc,
		filename = paste("TotalAftComposition", 
				shbasic::shbasic_condenseRunids(data.frame(aftData)[, "ID"]), sep="_"),
		alpha=0.7)


## ---- eval=FALSE, results="hide"-----------------------------------------
#  simp1 <- simp
#  simp1$sim$shortid <- "G9/C4"
#  simp2 <- simp
#  simp2$sim$shortid <- "G10/C5"
#  hl_comp_cell_aftcomposition(simp, simps = list(simp1, simp2), dataname = "csv_cell_aggregated")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_competitivenessPerRegion(simp1, dataname = "dataAgg")

## ---- eval=FALSE, dev="png", fig.width=7, fig.show='hold', results="hide"----
#  library(craftyr)
#  simp <- param_getExamplesSimp()
#  input_tools_load(simp, objectName = "csv_LandUseIndex_rbinded")
#  data <- get("csv_LandUseIndex_rbinded")
#  
#  scoresdata <- do.call(rbind, lapply(seq(from=2010, to=2040, by=10), function(tick, datac, regions, type) {
#  					do.call(rbind,lapply(regions, function(tick, region, data, type) {
#  			tickdata <- data[data$Tick == tick & data$Region %in% region,
#  					c(simp$csv$cname_x, simp$csv$cname_y, "LandUseIndex")]
#  			raster <- craftyr::convert_2raster(simp, tickdata, targetCRS = "+init=EPSG:32632", layers=c(1))
#  			data.frame(Value = analyse_statistics_sa_local(raster[[1]], type = type),
#  					Tick = tick, type = type
#  					Region = region)}, tick = tick, data = data, type = type))
#  		}, data = data, type = "Moran", regions = simp$sim$regions))
#  
#  visualise_lines(simp, scoresdata, "Value", title = paste("Spatial Autocorrelation (Moran)", simp$sim$rundesc[simp$sim$runid]),
#  		linetype_column = "Region",
#  		filename = paste("Spatial Autocorrelation (Moran)", "_", simp$sim$rundesc[simp$sim$runid], sep=""))

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_aggregate_sa(simp, celldataname = "csv_LandUseIndex_rbinded",
#  	starttick = simp$sim$starttick, tickinterval=10, endtick = simp$sim$endtick,
#  	linetypecol = "Region", type = "Moran", regions = simp$sim$regions,
#  	titleprefix = paste("SpatialAutocorrelation (", type, ")", sep=""),
#  	filenameprefix = paste("SpatialAutocorrelation", type, sep="_"))

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_connectedness(simp, dataname = "csv_aggregated_connectivity",
#  		datatype = "LandUseConnectivity", aftcolumns = simp$mdata$aftNames[-1],
#  		percent = NULL)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_volatility(simp, dataname = "csv_aggregated_cellvolatility",
#  		datatype = "AggregateCellVolatility", datacolumns = c("CellVolatility", "NumVolatileCells"),
#  		percent = NULL)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_lines_from_csv(simp, dataname = "dataobjectname",
#  		datatype = "Datatype", datacolumns = c("ColumnA","ColumnB"), linetypecol = "Region",
#  		colourcol = "Type", titleprefix = "Title", filenameprefix = "FilenamePrefix",
#  		percent = NULL)

## ---- eval=FALSE---------------------------------------------------------
#  library(craftyr)
#  simp <- param_getDefaultSimp()
#  input_tools_load(simp, "dataAggregateAFTComposition")
#  dataComp <- dataAggregateAFTComposition
#  dataComp[,grep("AFT.", colnames(dataComp))] <- as.numeric(do.call(cbind,
#  	lapply(dataComp[,grep("AFT.", colnames(dataComp))], as.character)))
#  dataComp <- dataComp[complete.cases(dataComp),]
#  colnames(dataComp) <- gsub("AFT.", "", colnames(dataComp))
#  	
#  data <-reshape2::melt(dataAggregateAFTComposition, variable.name="Agent", id.vars= c("Region", "Tick", "Runid", "Scenario"), direction="long")
#  d <- aggregate(subset(data, select=c("value")), by = list(Agent = data$Agent, Tick= data$Tick, Runid=data$Runid, Scenario=data$Scenario), "mean", na.rm = TRUE)
#  
#  ############### substitute AFT names by AFT ID
#  aftNumbers <- names(simp$mdata$aftNames)
#  names(aftNumbers) <- simp$mdata$aftNames
#  d$AFT <- aftNumbers[as.character(d$AFT)]
#  
#  visualise_lines(simp, d, "value", title = "AftCompositionInvalid",
#  		colour_column = "AFT", colour_legenditemnames = simp$mdata$aftNames,
#  		filename = "AftCompositionInvalid",
#  		alpha=0.7)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_aggregate_aftcompositions(simp)

## ---- eval=TRUE, dev="png", fig.width=7, fig.show='hold', results="hide"----
library(craftyr)
simp <- param_getExamplesSimp()
input_tools_load(simp, "csv_aggregateServiceDemand")

data <- convert_aggregate_meltsupplydemand(simp, csv_aggregateServiceDemand)

############## Aggregate regions:
data <- aggregate(subset(data, select=c("Value")),
		by = list(ID = data[,"Runid"],
				Tick=data[, "Tick"],  Scenario = data[,"Scenario"],
				Service=data[,"Service"], Type=data[,"Type"]),
		FUN=sum)

visualise_lines(simp, data, "Value", title = "Aggregated Service Supply & Demand",
		colour_column = "Service",
		linetype_column = "Type",
		filename = paste("AggregateServiceDemand", 
				shbasic::shbasic_condenseRunids(data.frame(data)[, "ID"]), sep="_"),
		alpha=0.7)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_aggregate_demandsupply(simp)

## ---- eval=FALSE, dev="png", fig.width=7, fig.show='hold', results="hide"----
#  #################### TODO correct output_visualise_takeovers to work with gradient2sided (bezierArrowGradient2sided.R:304)
#  library(craftyr)
#  simp <- param_getExamplesSimp()
#  input_tools_load(simp, "csv_aggregateTakeOvers")
#  input_tools_load(simp, "csv_cell_aggregated")
#  	
#  startPopulation <- data.frame(names(simp$mdata$aftNames), 0)
#  names(startPopulation) <- c("Agent", "AFT")
#  sp <- aggregate(subset(csv_cell_aggregated, select=c("AFT"),
#  		subset = csv_cell_aggregated$Tick==2000 && csv_cell_aggregated$Runid == "0-0"),
#  		by = list(Agent = csv_cell_aggregated[csv_cell_aggregated$Tick == 2000 &&
#  					csv_cell_aggregated$Runid == "0-0","LandUseIndex"]), FUN=sum)
#  startPopulation[startPopulation$Agent %in% sp$Agent,"AFT"] <- sp$AFT
#  startPopulation$Agent <- simp$mdata$aftNames[as.character(startPopulation$Agent)]
#  	
#  simp$mdata$aftNames <- simp$mdata$aftNames[-1]
#  dat <- aggregate(subset(csv_aggregateTakeOvers, select=simp$mdata$aftNames), by = list(
#  		Tick=csv_aggregateTakeOvers[, "Tick"],
#  		Runid=csv_aggregateTakeOvers[, "Runid"],
#  		AFT=csv_aggregateTakeOvers[,"AFT"]),
#  		FUN=sum)
#  
#  startPopulation <- startPopulation[match(simp$mdata$aftNames, startPopulation$Agent),]
#  colnames(startPopulation)[colnames(startPopulation) == "AFT"] <- "Number"
#  	
#  output_visualise_takeovers(simp,
#  		data = dat,
#  		startpopulation = startPopulation,
#  		starttick = 2000,
#  		endtick = 2020,
#  		tickinterval= 1,
#  		type_of_arrow = "simple", #"gradient2sided",
#  		transitionthreshold = 1)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_takeovers(simp)

## ---- eval=FALSE, dev="png", fig.width=7, fig.show='hold', results="hide"----
#  library(craftyr)
#  simp <- param_getExamplesSimp()
#  
#  dataTakeOversAll <- convert_aggregate_takeovers(simp,
#  	landusedataname = "csv_LandUseIndex_rbinded")
#  input_tools_save(simp, "dataTakeOversAll")
#  	
#  hl_takeovers(simp, runid = simp$sim$runids[1],
#  		dataname 			= "csv_cell_aggregated",
#  		starttick 			= 2000,
#  		tickinterval 		= 5,
#  		endtick 			= 2020,
#  		datanametakeovers 	= "csv_aggregateTakeOver")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_takeovers_all(simp, landusedataname = "csv_LandUseIndex_rbinded",
#  	starttick = 2000, tickinterval=5, endtick = 2020)

## ---- eval=TRUE, dev="png", fig.width=7, fig.show='hold', results="hide"----

library(craftyr)
library(reshape2)
simp <- param_getExamplesSimp()
input_tools_load(simp, "csv_aggregateGiStatistics")

regions = simp$sim$regions
csv_aggregateGiStatistics <- csv_aggregateGiStatistics[csv_aggregateGiStatistics[,"Region"] %in% regions,]

dat <- aggregate(subset(csv_aggregateGiStatistics, select=simp$mdata$aftNames[-1]), by = list(
				Trials=csv_aggregateGiStatistics[, "Trials"],
				Runid=csv_aggregateGiStatistics[, "Runid"]),
		FUN=sum)

melteddat <- reshape2::melt(dat, variable.name="AFT", id.vars= c("Trials", "Runid"), 
		direction="long", value.name = "Number")

visualise_bars(simp, data = melteddat, y_column = "Number", title = "Giving In Statistics",
		facet_column = "AFT", facet_ncol = 1, fill_column = "AFT",
		alpha=1.0, x_column = "Trials", ggplotaddons = ggplot2::theme(legend.position="none"))

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_gistatistics_singleRegion(simp, dataname = "csv_aggregateGiStatistics",
#  		regions = simp$sim$regions, facet_ncol = 1)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_competitiveness_prealloc(simp, dataname = "csv_preAlloc_rbinded",
#  		facet_ncol = length(simp$mdata$aftNames), filename = paste("PreAllocationCompetition",
#  		simp$sim$id, sep="_b_"),
#  		maxcompetitiveness = "100%", numbins = 15, title = NULL, ggplotaddons = NULL, checkexists = FALSE)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_competitiveness_preallocPerAft(simp, dataname = "csv_preAlloc_rbinded",
#  		facet_ncol = length(simp$mdata$aftNames), filename = paste("PreAllocationCompetition",
#  		simp$sim$id, sep="_b_"),
#  		maxcompetitiveness = "100%", numbins = 15, title = NULL, ggplotaddons = NULL, checkexists = FALSE)

## ---- eval=FALSE, dev="png", fig.width=7, fig.show='hold', results="hide"----
#  cellnumber <- input_csv_param_capitals_cellnumbers(simp, capitals = simp$mdata$capitals,
#  		regionpartfromend = 2, regionpartdevider = "_")
#  

## ---- eval=FALSE, results="hide"-----------------------------------------
#  numcells <- cellnumbers <- input_csv_param_capitals_cellnumbers(simp, regionpartfromend = 2, regionpartdevider = "_")
#  hl_volatility(simp, dataname = "csv_aggregated_cellvolatility",
#  		datatype = "AggregateCellVolatility", datacolumns = c("CellVolatility", "NumVolatileCells"),
#  		percent = numcells)

## ---- eval=FALSE, dev="png", fig.width=7, fig.show='hold', results="hide"----
#  input_tools_load(simp, "csv_MarginalUtilitites_melt")
#  visualise_lines(simp, data, "value", title = "Marginal Utilities",
#  		colour_column = "Service",
#  		filename = paste("MarginalUtilities", sep=""),
#  		alpha=0.7)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_marginalutilities(simp)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_normalisedutilities(simp,
#  		filenamemarginalutils = paste(simp$dirs$output$rdata, "MarginalUtilitiesPerCell.csv", sep = "/"),
#  		filenameNormalisedResiduals = paste("NormalisedResiduals",
#  				shbasic::shbasic_condenseRunids(data.frame(data)[, "ID"]), sep="_"),
#  		capitalfilepartorder = c("regionalisation", "U", "regions", "U", "datatype"))

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_aftcomposisition_file(simp, csvfilename, title = "AftComposition",
#  		figurefilename = "AftComposition")
#  
#  ### Comparisons
#  To compare different data sets, according sip lists need to be defined:
#  

## ---- eval=FALSE, results="hide"-----------------------------------------
#  source("./simp44.R")
#  simp1 <- simp
#  simp1$sim$shortid <- "SD.9"
#  
#  source("./simp45.R")
#  simp2 <- simp
#  simp2$sim$shortid <- "SD.4"
#  
#  source("./simp46.R")
#  simp3 <- simp
#  simp3$sim$shortid <- "SD.55"
#  simps <- list(simp1, simp2, simp3)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_comp_aggregate_aftcompositions(simp, simps, dataname="dataAggregateAFTComposition")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_comp_demandsupply(simp, simps, dataname="dataAggregateSupplyDemand")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_landindiceskey_csv(simp)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_aggregate_demandsupply_csv(simp)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_comp_demandsupplygap_agentparams(simp, simps = input_tools_buildsimplist(111:116),
#  		dataname = "dataAggregateSupplyDemand",
#  		filename = paste("SupplyDemandGap_",
#  				if(!is.null(simp$sim$rundesc))paste(simp$sim$rundesc, collapse = "-") else simp$sim$id, sep=""),
#  		title = "Demand/Supply Gap",
#  		agentparam = "givingUpProb", aft = simp$mdata$aftNames[2],
#  		ggplotparams = ggplot2::xlab("Probability of Giving up"))

## ---- eval=FALSE, results="hide"-----------------------------------------
#  simp <- param_getExamplesSimp()
#  input_tools_load(simp, dataname = "csv_LandUseIndex_rbinded")
#  
#  data <- get(dataname)
#  data <- data[data$Tick == 2010, c(simp$csv$cname_x, simp$csv$cname_y, "LandUseIndex")]
#  rownames(data) <- NULL
#  
#  raster <- craftyr::convert_2raster(simp, data, targetCRS = "+init=EPSG:32632", layers=c(1))
#  
#  filename <- sprintf("%s/%s_LandUseIndex_%d.%s",
#  		simp$dirs$output$raster,
#  		output_tools_getDefaultFilename(simp),
#  		as.integer(tick),
#  		"asc")
#  shbasic::sh.ensurePath(filename, stripFilename = TRUE)
#  raster::writeRaster(raster[[1]][[1]], filename = filename, format = "ascii",
#  		NAflag=naflag, overwrite=TRUE)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_cell2raster(simp, tick, dataname = "csv_LandUseIndex_rbinded",
#  		landuseindexcolname = "LandUseIndex", naflag = -9)

