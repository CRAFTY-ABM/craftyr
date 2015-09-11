## ---- eval=TRUE, results="hide"------------------------------------------
library(craftyr)
simp <- param_getExamplesSimp()

futile.logger::flog.threshold(futile.logger::DEBUG, name='craftyr.input.csv')

cdata <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
		pertick = TRUE, starttick = 2010, endtick = 2020, tickinterval = 10,
		attachfileinfo = TRUE, bindrows = TRUE)
csv_LandUseIndex <- cdata
input_tools_save(simp, "csv_LandUseIndex")
cdata <- split(cdata, list(cdata$Tick, cdata$Runid))
csv_LandUseIndex_split <- cdata
input_tools_save(simp, "csv_LandUseIndex_split")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_store_csvcelldata(simp, dataname = "csv_LandUseIndex", tickinterval = 10)

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
input_tools_load(simp, "csv_LandUseIndex")

# aggregate regions:
csv_LandUseIndex$AftNumbers <- csv_LandUseIndex$LandUseIndex
aftData <- aggregate(subset(csv_LandUseIndex, select=c("AftNumbers")),
		by = list(ID = csv_LandUseIndex[,"Runid"],
				Tick=csv_LandUseIndex[, "Tick"],  AFT=csv_LandUseIndex[,"LandUseIndex"]),
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

## ---- eval=FALSE---------------------------------------------------------
#  library(craftyr)
#  simp <- param_getDefaultSimp()
#  input_tools_load(simp, "dataAggregateAFTComposition")
#  
#  data <-reshape2::melt(dataAggregateAFTComposition, variable.name="Agent", id.vars= c("Region", "Tick", "Runid", "Scenario"), direction="long")
#  data$value <- as.numeric(levels(data$value)[data$value])
#  d <- aggregate(subset(data, select=c("value")), by = list(Agent = data$Agent, Tick= data$Tick, Runid=data$Runid, Scenario=data$Scenario), "mean", na.rm = TRUE)
#  
#  # substitute AFT names by AFT ID
#  aftNumbers <- names(simp$mdata$aftNames)
#  names(aftNumbers) <- simp$mdata$aftNames
#  d$AFT <- aftNumbers[as.character(d$AFT)]
#  
#  visualise_lines(simp, d, "value", title = "AftCompositionInvalid",
#  		colour_column = "AFT", colour_legenditemnames = simp$mdata$aftNames,
#  		filename = "AftCompositionInvalid",
#  		alpha=0.7)

## ---- eval=TRUE, dev="png", fig.width=7, fig.show='hold', results="hide"----
library(craftyr)
simp <- param_getExamplesSimp()
input_tools_load(simp, "csv_aggregateServiceDemand")

data <- convert_aggregate_meltsupplydemand(simp, csv_aggregateServiceDemand)

# Aggregate regions:
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
#  # TODO correct output_visualise_takeovers to work with gradient2sided (bezierArrowGradient2sided.R:304)
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

## ---- eval=FALSE, dev="png", fig.width=7, fig.show='hold', results="hide"----
#  input_tools_load(simp, "csv_MarginalUtilitites_melt")
#  visualise_lines(simp, data, "value", title = "Marginal Utilities",
#  		colour_column = "Service",
#  		filename = paste("MarginalUtilities", sep=""),
#  		alpha=0.7)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_marginalutilities(simp)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_hl_landindiceskey_csv(simp)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_aggregate_demandsupply_csv(simp)

