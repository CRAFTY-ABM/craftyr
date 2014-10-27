## ----, eval=FALSE--------------------------------------------------------
#  ###############################################################################
#  # Machine=specific SIMP definition
#  ### Environment ###############################################################
#  # craftyr demo
#  ### Remove existing definition ################################################
#  rm(list=ls(all=TRUE))
#  rm(list=ls(all=TRUE, envir=globalenv()), envir=globalenv())
#  #### Set path to itself #######################################################
#  simp <- list()
#  simp$simpDefinition <- "../demo/simp-machine.R"
#  #### Load gneral SIMP #########################################################
#  source("./simp.R")
#  
#  ### Directories ###############################################################
#  simp$dirs <- list()
#  simp$dirs$project			<- "./"
#  simp$dirs$data 				<- paste(simp$dirs$project, "data/", sep="")
#  simp$dirs$outputdir			<- "./"
#  dimp$dirs$simp				<- paste(simp$dirs$project, "./demo", sep="")
#  [...]

## ----, eval=FALSE--------------------------------------------------------
#  ################################################################
#  # General SIMulation Properties:
#  ################################################################
#  if (is.null(simp)) simp <- list()
#  simp$sim <- list()
#  simp$sim$worldname 				<- "ToyWorld"
#  simp$sim$version				<- "V001"
#  simp$sim$allocversion			<- "V001AllocGen"
#  simp$sim$scenario				<- "Scenario"
#  [...]

## ----, eval=TRUE---------------------------------------------------------
print(getwd())
#source("../demo/simp-machine.R")
#source(paste(simp$dirs$simp, "simp.R", sep=""))

# read capital data from CSV into raster
#data <- input_csv_param_capitals(simp)
#rasters <- convert_2raster(simp, data, layers = simp$mdata$capitals)
#names(rasters) <- "RegionA"
#visualise_raster_printRawPlots(simp, rasters, legendtitle = "Capitals Example", 
#		factorial= FALSE, omitaxisticks = FALSE, ncol = 1, id="None")

