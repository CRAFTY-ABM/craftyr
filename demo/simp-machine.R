###############################################################################
# Machine=specific SIMP definition
# NOTE: Changes in super-level parameters that are used to derive further
# parameters need to trigger a re-evaluation of the derived parameters!
### Environment ###############################################################
# craftyr demo
### Remove existing definition ################################################
rm(list=ls(all=TRUE))
rm(list=ls(all=TRUE, envir=globalenv()), envir=globalenv())
#### Set path to itself #######################################################
simp <- list()
simp$simpDefinition <- "../demo/simp-machine.R"
#### Load default SIMP #########################################################
source("../demo/simp.R")
simp <- param_mergeDefaultSimp(simp)


### Directories ###############################################################
simp$dirs <- list()

simp$dirs$project			<- "PathToProject/"
simp$dirs$data 				<- paste(simp$dirs$project, "data/", sep="")
simp$dirs$outputdir			<- "./Output/"
simp$dirs$simp				<- paste(simp$dirs$project, "config/R/", sep="")

simp$dirs$output <- list()
simp$dirs$output$simulation	<- paste(simp$dirs$outputdir, "simulation/", sep="")
simp$dirs$output$rdata		<- paste(simp$dirs$outputdir, "rData/", sep="") 
simp$dirs$output$raster		<- paste(simp$dirs$outputdir, "raster/", sep="") 
simp$dirs$output$figures	<- paste(simp$dirs$outputdir, "figures/", sep="")
simp$dirs$output$reports	<- paste(simp$dirs$outputdir, "reports/", sep="")

cat("Working Directory: ", getwd())
