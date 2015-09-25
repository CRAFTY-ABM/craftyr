#' Aggregate raster data with the given aggregate function
#' 
#' @param inforasterdata 
#' @param aggregatefunction 
#' @return data.frame
#' 
#' @author Sascha Holzhauer
#' @export
convert_raster_flatlist <- function(inforasterdata, aggregatefunction="sum") {
	flatdata <- lapply(inforasterdata, function(infodata, aggregatefunction) {
					idata <- apply(infodata, MARGIN=1, function(infodataitem, aggregatefunction) {
							infodataitem$Aggregate <- raster::cellStats(infodataitem$Raster, stat=aggregatefunction)
							infodataitem$Raster <- NULL
							infodataitem
						}, aggregatefunction = aggregatefunction)
					plyr::ldply(idata, as.data.frame, stringsAsFactors = FALSE)
				}, aggregatefunction = aggregatefunction)
	result <- do.call(rbind, flatdata)
}
#' Compute AFt numbers
#' 
#' @param aft_raster 
#' @return data.farme with numbers and info
#' 
#' @author Sascha Holzhauer
#' @export
convert_raster_getAftNumbers <- function(aft_raster) {
	aftNumbers <- lapply(aft_raster, function(aft_raster_items) {
				idata <- apply(aft_raster_items, MARGIN=1, function(aft_raster_item) {
							aft_raster <- aft_raster_item$Raster
							total_agents <- 
							aft_raster_item$Raster <- NULL
							
							# get colnames right
							raster_names <- names(aft_raster_item)
							raster <- data.frame(aft_raster_item, raster::freq(aft_raster))
							names(raster) <- c(raster_names, "AFT", "Value")
							raster
						})
				do.call(rbind, idata)
			})
	result <- do.call(rbind, aftNumbers)
}
#' Determine the fraction of AFTs in the given rasters whose state corresonds with the given state
#' 
#' @param aft_raster list (Regions) of list (Tick) of raster including information of AFT IDs
#' @param state_raster list (Regions) of list (Tick) of raster including information of AFT states
#' @param state the state of interest
#' @return data.farme of fractions
#' 
#' @author Sascha Holzhauer
#' @export
convert_raster_aftFractions <- function(aft_raster, state_raster, state) {
	adoptionPerAft <- mapply(function(aft_raster_items, state_raster_items, state) {
				aft_raster_items$state_raster <- state_raster_items$Raster
				
#				if (length(aft_raster_items) != length(state_raster_items)) {
#					R.oo::throw.default("Lengths (supposedly number of ticks) oft AFT and State rasters do not match!")
#				}
				idata <- apply(aft_raster_items, MARGIN=1, function(aft_raster_item, state) {
							aft_raster <- aft_raster_item$Raster
							state_raster <- aft_raster_item$state_raster
							
							total_agents <- raster::freq(aft_raster)
							
							# filter agents without given state:
							aft_raster[state_raster != state] <- NA
							
							adopted_aft <- raster::freq(aft_raster)
							
							# filter NA afts
							adopted_aft <- adopted_aft[!is.na(adopted_aft[,"value"]),]
							
							adopted_aft[,"count"] <- adopted_aft[,"count"]/total_agents[total_agents[, "value"] 
											%in% adopted_aft[, "value"],"count"]
							
							# delete unrequried data
							aft_raster_item$Raster <- NULL
							aft_raster_item$state_raster <- NULL
							
							# get colnames right
							raster_names <- names(aft_raster_item)
							raster <- data.frame(aft_raster_item, adopted_aft)
							names(raster) <- c(raster_names, "AFT", "Adopted")
							raster$AFT  <- as.factor(raster$AFT)
							raster
						}, state)
				do.call(rbind, idata)
			}, aft_raster, state_raster, state = state, SIMPLIFY = FALSE)
}