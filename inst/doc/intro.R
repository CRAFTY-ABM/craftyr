## ---- eval=FALSE---------------------------------------------------------
#  ###############################################################################
#  # Machine=specific SIMP definition
#  # NOTE: Changes in super-level parameters that are used to derive further
#  # parameters need to trigger a re-evaluation of the derived parameters!
#  ### Environment ###############################################################
#  # craftyr demo
#  ### Remove existing definition ################################################
#  rm(list=ls(all=TRUE))
#  rm(list=ls(all=TRUE, envir=globalenv()), envir=globalenv())
#  library(craftyr)
#  #### Set path to itself #######################################################
#  simp <- list()
#  simp$simpDefinition <- "../demo/simp-machine.R"
#  #### Load project default SIMP ################################################
#  source("../demo/simp.R")
#  #### Merge crafty default SIMP ################################################
#  simp <- param_mergeDefaultSimp(simp)
#  
#  ### Directories ###############################################################
#  
#  simp$dirs <- list()
#  simp$dirs$project			<- "C:/Data/LURG/workspace/CRAFTY_ConsVis-ToyWorld/"
#  simp$dirs$data 				<- paste(simp$dirs$project, "data/", sep="")
#  simp$dirs$outputdir			<- "C:/Data/LURG/Projects/Volante/Output/"
#  simp$dirs$simp				<- paste(simp$dirs$project, "./config/R/", sep="")
#  [...]

## ---- eval=FALSE---------------------------------------------------------
#  ################################################################
#  # General SIMulation Properties:
#  ################################################################
#  if (!exists("simp")) simp <- list()
#  simp$sim <- list()
#  simp$sim$worldname 				<- "ToyWorld"
#  simp$sim$version				<- "V001"
#  simp$sim$allocversion			<- "V001AllocGen"
#  simp$sim$scenario				<- "Scenario"
#  [...]

## ---- eval=FALSE---------------------------------------------------------
#  # Only contained when the particular script is only executed on a specific maschine!
#  # Otherwise. the maschine=specific file needs to be executed before.
#  source("/PATH-TO/simp-machine_cluster.R")
#  
#  # Usually also in simp.R, but required here to find simp.R
#  simp$sim$folder 	<- " "parentFolder/_version""	
#  
#  simp$sim$runids 	<- c("0-0")			# run to deal with
#  simp$sim$id			<- "template-0-0" 	# ID to identify specific data collections (e.g. regions)
#  simp$sim$task		<- "task1"			# Name of surrounding folder, usually a description of task
#  
#  # simp$dirs$simp is set by maschine-specific file:
#  setwd(paste(simp$dirs$simp, simp$sim$folder, "cluster/", simp$sim$task, sep="/"))
#  # usually, the setting/scenario specific simp.R is two levels above:
#  source("../../simp.R")

## ---- eval=FALSE---------------------------------------------------------
#  futile.logger::flog.threshold(futile.logger::DEBUG, name='crafty')
#  futile.logger::flog.threshold(futile.logger::TRACE, name='crafty.input')

## ---- eval=FALSE---------------------------------------------------------
#  futile.logger::flog.appender(appender.file(filename), name='test.logger')

## ---- eval=FALSE---------------------------------------------------------
#  csv_LandUseIndex_rbinded <- data
#  input_tools_save(simp, "csv_LandUseIndex_rbinded")
#  rm (csv_LandUseIndex_rbinded)

## ---- eval=FALSE---------------------------------------------------------
#  input_tools_load(simp, "csv_LandUseIndex_rbinded")

## ---- eval=FALSE---------------------------------------------------------
#  data(package="craftyr")
#  source("../demo/simp-machine.R")
#  simp$fig$init(simp, filename = "example/hist_aft.png")
#  hist(cellData$LandUseIndex, breaks=3, col="red")
#  dev.off()
#  

## ---- eval=FALSE---------------------------------------------------------
#  Error in if (simp$fig$plottitle > 0) { : argument is of length zero

