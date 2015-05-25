## ---- eval=FALSE---------------------------------------------------------
#  library(craftyr)
#  simp <- param_getExamplesSimp()
#  dataAggregateAFTComposition <- input_csv_data(simp, dataname = NULL, datatype = "AggregateAFTComposition",
#  		pertick = FALSE,
#  		bindrows = TRUE,
#  		skipXY = TRUE)
#  input_tools_save(simp, "dataAggregateAFTComposition")

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

