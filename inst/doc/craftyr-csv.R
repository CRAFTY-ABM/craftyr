## ----, eval=TRUE---------------------------------------------------------
library(craftyr)
simp <- param_getExamplesSimp()
cdata <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
		pertick = TRUE, starttick = 2010, endtick = 2020, tickinterval = 10,
		attachfileinfo = TRUE, bindrows = TRUE)
cdata <- split(cdata, list(cdata$Tick,cdata$Runid))
csv_LandUseIndex_split <- cdata
input_tools_save(simp, "csv_LandUseIndex_split")

## ----, eval=FALSE--------------------------------------------------------
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

## ----, eval=FALSE--------------------------------------------------------
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

## ----, eval=TRUE, dev="png", fig.show='hold'-----------------------------
library(craftyr)
simp <- param_getExamplesSimp()
input_tools_load(simp, "csv_LandUseIndex_split")
visualise_cells_printRawPlots(simp, csv_LandUseIndex_split,
		factorial= TRUE, ncol = 1, id="None")		

