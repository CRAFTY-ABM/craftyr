#' Calcualte and plot competitiveness for every AFT on every cell
#' @param simp 
#' @param dirtocapitals 
#' @param capitalfilename 
#' @param dirtodemand 
#' @param demandfilename 
#' @param dirtoproduction 
#' @param productionfilenamepattern 
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_plotCompetitiveness <- function(simp,
			dirtocapitals = paste(simp$dirs$alloc, "/worlds/", simp$sim$world,  "/regionalisations/", 
					simp$sim$regionalisation, "/", simp$sim$scenario, sep=""),
			capitalfilename = paste(simp$sim$regionalisation, "_", simp$sim$regions, "_Capitals.csv", sep=""),
			dirtodemand = paste(simp$dirs$data, "/worlds/", simp$sim$world,  "/regionalisations/", 
					simp$sim$regionalisation, "/", simp$sim$scenario, sep=""),
			demandfilename = paste(simp$sim$regionalisation, "_", simp$sim$scenario, "_", simp$sim$regions, 
					"_demand.csv", sep=""),
			dirtoproduction = paste(simp$dirs$data, "/production/", sep=""),
			productionfilenamepattern = "<AFT>/AftProduction_<AFT>.csv"
		) {
	capitals <- read.csv(paste(dirtocapitals, capitalfilename, sep="/"))

	demand	<- read.csv(paste(dirtodemand, demandfilename, sep="/"))
	demand 	<- demand[demand$Year == simp$sim$starttick,-length(demand)]

	celldata <- data.frame()
	for (aft in simp$mdata$aftNames[-1]) {
		production = read.csv(paste(dirtoproduction, gsub("<AFT>", aft, productionfilenamepattern, fixed=TRUE),
						sep=""), row.names = 1)
		
		#capitals = capitals[1:5,]
		compet <- t(apply(capitals, MARGIN=1, function(x) {
							caps <- x[ -c(1,2)]
							
							caps <- caps - 1
							
							caps <- caps[names(production[-length(production)])]
							
							product <- apply(production[-length(production)], MARGIN=1, function(x,y) {
										prod(y^x)
									}, caps)
							product <- product * production[,length(production)]
							
							cellDemand <- demand/nrow(capitals)
							cellResidual <- cellDemand # no supply in first tick
							comp <- mapply(function(x, name) {
										simp$submodels$comp$sfuncs[[name]](x)
									}, cellResidual, names(cellResidual))
							competitiveness = sum(comp * product)
							c(x[1], x[2], Competitiveness = competitiveness)
						}))
		
		celldata <- rbind(celldata, cbind(as.data.frame(compet), AFT = aft))
	}
	
	visualise_cells_printPlots(simp, list(celldata), idcolumn = "AFT", valuecolumn = "Competitiveness",
			title = "Competitiveness", ncol = 3)
}