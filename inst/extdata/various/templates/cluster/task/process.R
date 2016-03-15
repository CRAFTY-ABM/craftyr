#######################################################################
# ApplicationScript for Storing CRAFTY ouptut as R data (both
# raw and aggregated).
#
# Project:		CRAFTY-ImpressionsEU
# Last update: 	25/09/2015
# Author: 		Sascha Holzhauer
#######################################################################

# Only contained when the particular script is only executed on a specific maschine!
# Otherwise. the maschine=specific file needs to be executed before.
source("./config/R/simp-machine_cluster.R")

# Usually also in simp.R, but required here to find simp.R
simp$sim$folder 	<- "_setA/_staticBL02"	


simp$sim$task		<- "29-0_preAlloc"			# Name of surounding folder, usually a description of task 

# simp$dirs$simp is set by maschine-specific file:
setwd(paste(simp$dirs$simp, simp$sim$folder, "cluster/", simp$sim$task, sep="/"))
# usually, the setting/scenario specific simp.R is two levels above:
source("../../simp.R")

simp$sim$id			<- "29-0" 	# ID to identify specific data collections (e.g. regions)
simp$sim$runids 	<- c("29-0")			# run to deal with

library(plyr)

#######################################################################
futile.logger::flog.threshold(futile.logger::INFO, name='crafty')

simp$sim$rundesc 		<- c("29-0"="smoother-comp")
simp$sim$rundesclabel	<- "Runs"


###########################################################################
### Store PreAlloc Data for Maps etc. 
###########################################################################
simp$sim$filepartorder	<- c("runid", "D", "tick", "D", "regions", "D", "datatype")
simp$sim$regions <- c("UK", "IR")

data <- input_csv_data(simp, dataname = NULL, datatype = "PreAlloc",
		columns = c("PreAllocCompetitiveness", "PreAllocLandUseIndex", "PreAllocGivingUpThreshold"),
		pertick = FALSE, attachfileinfo = FALSE)

csv_LandUseIndex_rbinded <- do.call(rbind.data.frame, data)
input_tools_save(simp, "csv_preAlloc_rbinded")

