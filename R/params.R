#' Merge the given list into the default SIMP parameter list.
#' Applies \code{\link[utils]{mergeList}}.
#' @param simp 
#' @return given list modified by adding undefined parameter of SIMP default paramter list.
#' 
#' @author Sascha Holzhauer
#' @export
param_mergeDefaultSimp <- function(simp = list()) {
	################################################################
	# General SIMulation Properties:
	################################################################
	
	# TODO see shdoe
	
	if (!exists("defsimp")) defsimp <- list()
	defsimp$sim <- list()
	defsimp$sim$worldname 				<- "world"
	defsimp$sim$version					<- "version"
	defsimp$sim$allocversion			<- "allocversion"
	defsimp$sim$scenario				<- "scenario"
	defsimp$sim$regionalisation			<- "regionalisation"
	defsimp$sim$regions					<- c("region")
	defsimp$sim$runids					<- c("0-0")
	defsimp$sim$hasregiondir			<- TRUE
	defsimp$sim$filepartorder			<- c("scenario", "D", "runid", "D", "regions", "D", 
											"datatype", "D", "dataname", "D", "tick")
	
	### Directories ################################################################
	defsimp$dirs <- list()
	defsimp$dirs$project			<- "./"
	defsimp$dirs$data 				<- paste(defsimp$dirs$project, "data/", sep="")
	defsimp$dirs$outputdir			<- paste(defsimp$dirs$project, "output/version/", sep="")
	
	defsimp$dirs$output <- list()
	defsimp$dirs$output$simulation	<- paste(defsimp$dirs$outputdir, "simulation/", sep="")
	defsimp$dirs$output$rdata		<- paste(defsimp$dirs$outputdir, "rData/", sep="") 
	defsimp$dirs$output$raster		<- paste(defsimp$dirs$outputdir, "raster/", sep="") 
	defsimp$dirs$output$figures		<- paste(defsimp$dirs$outputdir, "figures/", sep="")
	defsimp$dirs$output$reports		<- paste(defsimp$dirs$outputdir, "reports/", sep="")
	
	
	### CSV Column Names ###########################################################
	defsimp$csv <- list()
	defsimp$csv$cname_region 		<- "Region"
	defsimp$csv$cname_tick 			<- "Tick"
	defsimp$csv$cname_aft 			<- "Agent"
	defsimp$csv$cname_x				<- "Y"
	defsimp$csv$cname_y				<- "X"
	
	### Model Data ################################################################
	
	defsimp$mdata <- list()
	defsimp$mdata$capitals 			<- c("Cprod", "Fprod", "Infra", "Grass", "Nat", "Econ")
	defsimp$mdata$services			<- c("Meat", "Cereal" ,"Conservation", "Timber","Biofuel")
	defsimp$mdata$aftNames			<- c("-1" = "Unmanaged", "0" = 'C_Cereal', "1" = 'NC_Cereal', "2" = 'C_Livestock', "3" = 'NC_Livestock',
			"4" = 'Forester', "5" = 'Conservationist', "6" = 'BiofuelFarmer')	
	
	defsimp$mdata$conversion$aft <- c("AFT.C_Cereal" = 0, "AFT.NC_Cereal" = 1,
			"AFT.C_Livestock" = 2, "AFT.NC_Livestock" = 3,
			"AFT.Forester" = 4, "AFT.Conservationist" = 5)
	
	defsimp$mdata$conversion$services <- c("Service.Meat"="Meat", "Service.Cereal"="Cereal", 
			"Service.Recreation"="Conservation", "Service.Timber"="Timber", "Recreation"="Conservation",
			"Meat"="Meat", "Cereal"="Cereal", 
			"Conservation"="Conservation", "Timber"="Timber")
	
	### Figure Settings ###########################################################
	defsimp$fig <- list()
	defsimp$fig$resfactor		<- 3
	defsimp$fig$outputformat 	<- "png" #"jpeg"
	defsimp$fig$init			<- craftyr::output_visualise_initFigure
	defsimp$fig$close			<- dev.off
	defsimp$fig$numfigs			<- 1
	defsimp$fig$numcols			<- 1
	defsimp$fig$height			<- 700
	defsimp$fig$width			<- 1000
	defsimp$fig$splitfigs		<- FALSE
	defsimp$fig$plottitle		<- TRUE
	defsimp$fig$facetlabelsize	<- 12
	
	### Colour Settings ###########################################################
	defsimp$colours <- list()
	defsimp$colours$AFT 			<- settings_colours_getAftColours()
	defsimp$colours$Service 		<- settings_colours_getServiceColours()
	defsimp$colours$Capital 		<- settings_colours_getCapitalColours()
	defsimp$colours$Region 			<- settings_colours_getColorSet(NULL, number = 12, set= "Set3")
	defsimp$colours$Runid 			<- settings_colours_getColorSet(NULL, number = 12, set= "Set3")
	defsimp$colours$GenericFun		<- settings_colours_getColorSet
	defsimp$colours$binarycolours 	<- c("skyblue1", "black")
	defsimp$colours$defaultset		<- "Dark2"
	
	### Technical Settings ###########################################################
	defsimp$tech <- list()
	defsimp$tech$maxtick <- 3000
	
	### Debug Settings ############################################################
	defsimp$debug <- list()
	# the higher, the more verbose
	defsimp$debug$global 	<- 0
	defsimp$debug$db		<- NA
	defsimp$debug$input		<- NA
	defsimp$debug$output	<- NA
	
	
	result <- modifyList(defsimp, simp)
	result
}
#' Get the default SIMP parameter list
#' @return list of default SIMP parameters
#' 
#' @author Sascha Holzhauer
#' @export
param_getDefaultSimp <- function() {
	param_mergeDefaultSimp()
}

#' Get the SIMP parameter list for working with example data
#' @return list of SIMP parameters to run examples
#' 
#' @author Sascha Holzhauer
#' @export
param_getExamplesSimp <- function() {
	simp <- param_mergeDefaultSimp()
	
	simp$dirs$outputdir			<- system.file("extdata", "output/version", package = "craftyr")
	
	simp$dirs$output <- list()
	simp$dirs$output$simulation	<- paste(simp$dirs$outputdir, "/simulation/", sep="")
	simp$dirs$output$rdata		<- paste(simp$dirs$outputdir, "/rData/", sep="") 
	simp$dirs$output$raster		<- paste(simp$dirs$outputdir, "/raster/", sep="") 
	simp$dirs$output$figures	<- paste(simp$dirs$outputdir, "/figures/", sep="")
	simp$dirs$output$reports	<- paste(simp$dirs$outputdir, "/reports/", sep="")
	
	simp$fig$init <- function(simp, outdir, filename) {}
	simp$fig$close<- function() {}
	
	simp$sim$id <- "Example"
	simp
}
