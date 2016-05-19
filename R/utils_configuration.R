#' Change column names of CSV files, e.g. capital files
#' @param sip 
#' @param indir 
#' @param outdir 
#' @param colname_old 
#' @param colname_new 
#' @param filepattern 
#' @return adjusted files
#' 
#' @author Sascha Holzhauer
#' @export
adjust_changecolumnnames <- function(sip,  indir = simp$dirs$param$getparamdir(simp, "capitals"),
		outdir = indir, colname_old, colname_new, filepattern = "*.csv") { 
	
	futile.logger::flog.info("Processing files in %s...",
			indir,
			name = "craftyr.utils.adjust.colnames")
	
	shbasic::sh.ensurePath(outdir)
	for (file in list.files(indir, pattern = filepattern)) {
		#file <- list.files(indir, pattern="*.csv")[1]
		
		futile.logger::flog.info("Processing %s...",
				file,
				name = "craftyr.utils.adjust.colnames")
		
		data <- read.csv(paste(indir, file, sep="/"))
		colnames(data)[colnames(data) == colname_old] <- colname_new
		
		outfile <- paste(outdir, file, sep="/")
		
		futile.logger::flog.info("Writing to %s...",
				outfile,
				name = "craftyr.utils.adjust.colnames")
		
		write.csv(data, outfile, row.names = FALSE)
	}
	futile.logger::flog.info("Processed %d files!",
			length(list.files(indir, pattern = filepattern)),
			name = "craftyr.utils.adjust.colnames")
}
#' Copy first line of CSV file and assign endtick.
#' 
#' @param sip 
#' @param indir 
#' @param outdir 
#' @param endtick 
#' @return altered CSV files
#' 
#' @author Sascha Holzhauer
#' @export
make_demand_static <- function(sip,  indir = simp$dirs$param$getparamdir(simp, "demand"),
		outdir = indir, endtick = simp$sim$endtick, tickcolumnname = "Year", filepattern = "*.csv") {
	
	futile.logger::flog.info("Processing files in %s...",
			indir,
			name = "craftyr.utils.demand.static")
	
	shbasic::sh.ensurePath(outdir)
	for (file in list.files(indir, pattern = filepattern)) {
		#file <- list.files(indir, pattern="*.csv")[1]
		
		futile.logger::flog.info("Processing %s...",
				file,
				name = "craftyr.utils.demand.static")
		
		data <- read.csv(paste(indir, file, sep="/"))
		
		data <- rbind(data[1,], data[1,])
		data[2, tickcolumnname] <- endtick
		
		outfile <- paste(outdir, file, sep="/")
		
		futile.logger::flog.info("Writing to %s...",
				outfile,
				name = "craftyr.utils.demand.static")
		
		write.csv(data, outfile, row.names = FALSE)
	}
	futile.logger::flog.info("Processed %d files!",
			length(list.files(indir, pattern = filepattern)),
			name = "craftyr.utils.demand.static")
}
#' Calculate optimal linear competition function parameters (according to document
#' "How to calibrate competition functions in CRAFTY?")
#' 
#' @param simp simp$sim$id must specify the run to source production from
#' @return data.frame of parameters 
#' 
#' @author Sascha Holzhauer
#' @export
getCompetitionFunctionParams <- function(simp, production = NULL, producingCells = NULL, paramAfactor = 0.75,
		paramBfactor = 1.0, datanameAFTcomposition = "csv_aggregateAFTComposition") {
	params <- data.frame()
	
	#  datanameAFTcomposition = "dataAggregateAFTComposition"
	input_tools_load(simp, "csv_aggregated_supply")
	proddata <- csv_aggregated_supply[csv_aggregated_supply$Tick == simp$sim$starttick,]
	
	# undo service name conversion:
	serviceconv <- simp$mdata$conversion$services[-grep("Service", names(simp$mdata$conversion$services), fixed = TRUE)]
	proddata$Service <- names(serviceconv)[match(proddata$Service, serviceconv)]
	if (is.null(production)) {
		production <- aggregate(subset(proddata, 
						select=c("TotalProduction")),
				by =list(Service = proddata[,"Service"]), FUN=sum)
	}
	
	aftComposition <- input_processAftComposition(simp, dataname = datanameAFTcomposition)
	aftComposition <- aftComposition[aftComposition$Tick == simp$sim$starttick,]
	
	for (service in simp$mdata$services) {
		# service = simp$mdata$services[1]
		sparams = data.frame()
		sparams[1,"service"] <-  service
		
		# get production of AFT with highest production of particular service
		max_prod = -Inf
		producingAFTs = c()
		for (aft in simp$mdata$aftNames[-1]) {
			# aft = simp$mdata$aftNames[4]
			prods <- input_csv_param_productivities(simp, aft)
			if (max_prod < prods[prods$X==service, "Production"]) {
				max_prod <- prods[prods$X==service, "Production"]
				primaryAFT <- aft
			}
			if (prods[prods$X==service, "Production"] > 0) {
				producingAFTs <- append(producingAFTs, aft)
			}
		}
		
		producingCellsService <- if (!is.null(producingCells)) producingCells[service] else
					sum(aftComposition[aftComposition$AFT %in% producingAFTs, "value"])
			
		aftData <- input_csv_param_agents(simp, primaryAFT)
		givingUp <- aftData[if(!is.null(simp$sim$aftParamId)) aftData$aftParamId == simp$sim$aftParamId else 1, "givingUp"]
		sparams[1, "a"] <- paramAfactor * givingUp * 
				production[production$Service==service,"TotalProduction"] / producingCellsService
		sparams[1, "b"] <- paramBfactor / (production[production$Service==service,"TotalProduction"] / producingCellsService)
		params <- rbind(params, sparams)
	}
	return(params)
}