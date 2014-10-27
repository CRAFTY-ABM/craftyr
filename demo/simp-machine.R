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

simp$dirs$project			<- "C:/Data/LURG/workspace/CRAFTY_ConsVis-ToyWorld/"
simp$dirs$data 				<- paste(simp$dirs$project, "data/", sep="")
simp$dirs$outputdir			<- "C:/Data/LURG/Projects/Volante/Output/"
simp$dirs$simp				<- paste(simp$dirs$project, "config/R/", sep="")

simp$dirs$output <- list()
simp$dirs$output$data		<- paste(simp$dirs$outputdir, "Data/", sep="")
simp$dirs$output$rdata		<- paste(simp$dirs$outputdir, "RData/", sep="") 
simp$dirs$output$raster		<- paste(simp$dirs$outputdir, "Raster/", sep="") 
simp$dirs$output$figures	<- paste(simp$dirs$outputdir, "Figures/", sep="")
simp$dirs$output$reports	<- paste(simp$dirs$outputdir, "Reports/", sep="")

cat("Working Directory: ", getwd())
