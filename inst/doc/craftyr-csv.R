## ---- eval=TRUE, results="hide"------------------------------------------
library(craftyr)
simp <- param_getExamplesSimp()
cdata <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
		pertick = TRUE, starttick = 2010, endtick = 2020, tickinterval = 10,
		attachfileinfo = TRUE, bindrows = TRUE)
csv_LandUseIndex <- cdata
input_tools_save(simp, "csv_LandUseIndex")
cdata <- split(cdata, list(cdata$Tick,cdata$Runid))
csv_LandUseIndex_split <- cdata
input_tools_save(simp, "csv_LandUseIndex_split")

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
library(reshape2)
simp <- param_getExamplesSimp()
input_tools_load(simp, "csv_aggregateServiceDemand")

data <- convert_aggregate_meltsupplydemand(simp, csv_aggregateServiceDemand)

#### Aggregate regions:
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

