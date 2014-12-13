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
	defsimp$sim$worldname 				<- "ToyWorld"
	defsimp$sim$version					<- "V001"
	defsimp$sim$allocversion			<- "V001AllocGen"
	defsimp$sim$scenario				<- "Scenario"
	defsimp$sim$regionalisation			<- "1"
	defsimp$sim$regions					<- c("Region")
	defsimp$sim$runid					<- 0
	defsimp$sim$runids					<- NULL
	
	### Directories ################################################################
	defsimp$dirs <- list()
	defsimp$dirs$project			<- "./"
	defsimp$dirs$data 				<- paste(defsimp$dirs$project, "data/", sep="")
	defsimp$dirs$outputdir			<- NA
	
	defsimp$dirs$output <- list()
	defsimp$dirs$output$data		<- paste(defsimp$dirs$outputdir, "Data/", sep="")
	defsimp$dirs$output$rdata		<- paste(defsimp$dirs$outputdir, "RData/", sep="") 
	defsimp$dirs$output$raster		<- paste(defsimp$dirs$outputdir, "Raster/", sep="") 
	defsimp$dirs$output$figures		<- paste(defsimp$dirs$outputdir, "Figures/", sep="")
	defsimp$dirs$output$reports		<- paste(defsimp$dirs$outputdir, "Reports/", sep="")
	
	
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
	defsimp$mdata$aftNames			<- c("0" = 'C_Cereal', "1" = 'NC_Cereal', "2" = 'C_Livestock', "3" = 'NC_Livestock',
			"4" = 'Forester', "5" = 'Conservationist', "6" = 'BiofuelFarmer')
	
	### Figure Settings ###########################################################
	defsimp$fig <- list()
	defsimp$fig$resfactor		<- 3
	defsimp$fig$outputformat 	<- "png" #"jpeg"
	defsimp$fig$init			<- craftyr::output_visualise_initFigure
	defsimp$fig$numfigs			<- 1
	defsimp$fig$numcols			<- 1
	defsimp$fig$height			<- 700
	defsimp$fig$width			<- 1000
	defsimp$fig$splitfigs		<- FALSE
	
	### Colour Settings ###########################################################
	defsimp$colours <- list()
	defsimp$colours$AFT 		<- settings_colours_getAftColours()
	defsimp$colours$Region 		<- settings_colours_getColorSet(12, "Set3")
	defsimp$colours$Runid 		<- settings_colours_getColorSet(12, "Set3")
	
	### Technical Settings ###########################################################
	defsimp$tech <- list()
	defsimp$tech$maxtick <- 3000
	
	### Debug Settings ############################################################
	defsimp$debug <- list()
	# the higher, the more verbose
	defsimp$debug$global 	<- 0
	defsimp$debug$db		<- defsimp$debug$global
	defsimp$debug$input		<- defsimp$debug$global
	defsimp$debug$output		<- defsimp$debug$global
	
	
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