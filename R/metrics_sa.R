#source(pix$path_names["dataReadingRaster"][[1]])


#' Spatial autocorrelation score for given raster
#' 
#' @param raster 
#' @param data 
#' @param type 
#' @return sa score
#' 
#' @author Sascha Holzhauer
#' @export
analyse_statistics_sa_local <- function(raster, data =  c(1), type = c("Moran", "Geary", "MoranLocal", "GearyLocal")) {
	if (!((type == "Moran") | (type == "Geary") | (type == "MoranLocal") | (type == "GearyLocal"))) {
		throw("Type not supported!")
	}
	safunc <- get(type, envir=asNamespace("raster"))
	if (is.atomic(raster)) {
		result <-safunc(raster)
	} else {
		result <- c()
		for (r in raster) {
			result <- append(result, safunc(r))
		}
	}
	result
	
	#Geary(raster)
#	raster <- rasterData[[1]]
#	raster_crd <- coordinates(raster)
#	
#	raster_nb <- poly2nb(raster, queen=T)
#	W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)
}


statistics_sa_moran_local_multi <- function(
		startYear = lix$startTick, 
		endYear = lix$endTick,
		everyYear = lix$everyTick,
		dataName = "Agent-SocialFactor",
		runIds = lix$run_ids,
		useRegionDir =TRUE,
		legendName = NULL,
		rasterFunction = NULL,
		main = NULL,
		basedir = pix$path_names["dataSource"][[1]]) {
	
	scores <- data.frame()
	for (runId in runIds) {
		rasters = readRasterDataYears(startYear=startYear, endYear=endYear, everyYear=everyYear, dataName=dataName, 
				useRegionDir=useRegionDir, run_id = runId, basedir = basedir)
		
		if (!is.null(rasterFunction)) {
			rasters <- rasterFunction(rasters)
		}
		
		values = statistics_sa_moran_local(rasters)
		scores <- rbind(scores, data.frame(ID = runId, Value=values, Tick=seq(startYear, endYear, everyYear), Scenario=lix$scenario))
	}
	
	for (runId in lix$comp$runIds) {
		rasters = readRasterDataYears(startYear=lix$comp$startYear, endYear=lix$comp$endYear, everyYear=lix$comp$everyYear, dataName=dataName, 
				useRegionDir=useRegionDir, run_id = runId, scenario = lix$comp$scenario)
		
		if (!is.null(rasterFunction)) {
			rasters <- rasterFunction(rasters)
		}
		
		values = statistics_sa_moran_local(rasters)
		scores <- rbind(scores, data.frame(ID = runId, Value=values, Tick=seq(lix$comp$startYear, lix$comp$endYear, lix$comp$everyYear), Scenario=lix$comp$scenario))
	}
	visualisation_sa_multi(dataName, runIds = unique(c(runIds, lix$comp$runIds)) , scores, main = main)
}

visualisation_sa_multi <- function(dataName,
		runIds,
		scores,
		legendName = "Scenarios",
		legendLabels = NULL, 
		main = NULL,
		returnplot = FALSE) {
	lix$outdir <<- paste(pix$path_names["out_dir_figure"][[1]], "/", paste(c(lix$scenario, lix$comp$scenario), 
					collapse="~") , sep="")
	lix$filename <<- paste(if (length(runIds)==1) runIds[1] else "MultiRun", "_", dataName, "_CompareSpA",
			if (!is.null(main)) paste("-" , main, sep=""), "_", 
			paste(c(lix$scenario, lix$comp$scenario), collapse="~"), sep="")
	source(pix$path_names["figSetStd"][[1]])
	
	
	g <- ggplot(scores, aes(x=Tick,y=Value,group=interaction(ID,Scenario), color=ID, linetype=Scenario)) + 
			geom_line(alpha=0.7, size=1) +
			#stat_summary(aes(group=ID,color=Scenario),geom="line",size=0.9,fun.y=mean, linetype=2) +
			#stat_summary(aes(group=ID,color=Scenario),geom="line",size=0.9,fun.y=sd,linetype=1) +
			theme(panel.background = element_blank(),  panel.border= element_blank(), plot.background = element_blank()) + 
			xlab("Year") + 
			if (!is.null(main)) labs(title = main) else NULL
	
#	if (!is.null(legendName)){
#		g <- g + scale_colour_discrete(name = legendName, labels=legendLabels)
#	}
	print(g); 
	dev.off()
	if (returnplot) return(g)
}
