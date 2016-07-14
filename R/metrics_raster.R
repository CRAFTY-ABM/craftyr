#' Determine the number of patches of a single raster
#' 
#' @param simp 
#' @param raster 
#' @param directions 
#' @param relevantindices raster values not included in \code{relevantindices} are considered 
#' to be background values and separate clumps.
#' @param relevantafts vector of AFT names. Alternative to \code{relevantindices} which has precedence.
#' @return number of patches
#' 
#' @author Sascha Holzhauer
#' @export
metric_raster_global_patches <- function(simp, raster, directions = 8, relevantindices = NULL,
		relevantafts = NULL) {
	if (is.null(relevantindices) & is.null(relevantafts)) {
		R.oo::throw.default("Either relevantindices or relevantafts must be specified!")
	}
	
	if (is.null(relevantindices)) {
		relevantindices <- as.numeric(names(simp$mdata$aftNames)[simp$mdata$aftNames %in% relevantafts])
	}
	
	countnonna <- function(x) {
		sum(is.na(x))
	}
	raster[!(raster::match(raster, relevantindices))] <- NA
	rasterLayer <- raster::clump(raster, directions=directions, gaps=TRUE)
	return(length(raster::freq(rasterLayer)))
}
#' Determine the number of patches of a list of rasters
#' 
#' @inheritParams metric_raster_global_patches
#' @param dataname
#' @param asvector if \code{TRUE} metric is returned as named vector
#' @return data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric (changes (integer)) when \code{asvector == TRUE}
#' 
#' @author Sascha Holzhauer
#' @export
metric_rasters_global_patches <- function(simp, dataname = "raster_landUseIndex", 
		directions = 8, relevantindices = NULL, relevantafts = NULL, asvector = FALSE) {
	# read raster maps if not stored
	rasters <- input_raster_get(simp = simp, dataname= dataname)[[1]]
	metric <- sapply(rasters$Raster, metric_raster_global_patches, simp= simp, directions = directions,
					relevantindices = relevantindices, relevantafts = relevantafts)
	
	return(if(asvector) setNames(metric, simp$sim$starttick:simp$sim$endtick) else data.frame(
			Metric = paste("ConsPatches", paste(relevantindices, relevantafts, collapse="-"), sep="_"), 
			Tick =  simp$sim$starttick:simp$sim$endtick, Value = metric))	
}
#' Calculates the changes in land uses in a row of raster data.
#' TODO test
#' TODO limit to specific AFT(s)
#' 
#' @param simp
#' @param aft if \code{NULL}, all AFTs are considered
#' @param dataname
#' @param asvector if \code{TRUE} metric is returned as named vector
#' @return data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric (changes (integer)) when \code{asvector == TRUE}
#' 
#' @author Sascha Holzhauer
#' @export
metric_rasters_changes <- function(simp, aft = NULL, dataname = "raster_landUseIndex",
		asvector = FALSE) {
	# read raster maps if not stored
	raster_aft <- input_raster_get(simp = simp, dataname= dataname)[[1]]
	
	last = raster_aft$Raster[[1]]
	metric = c()
	for (raster in raster_aft$Raster[2:length(raster_aft$Raster)]) {
		# raster <- raster_aft$Raster[2]
		changes <- (raster[[1]] - last)
		changes[changes != 0] <- 1
		metric <-  c(metric, raster::cellStats(changes, "sum"))
		last <- raster[[1]]
	}
	#raster::plot(changes)
	return(if(asvector) setNames(metric, (simp$sim$starttick + 1):simp$sim$endtick) else data.frame(
							Metric = paste("VarChangesLu", if (!is.null(aft)) "_", aft, sep=""),
							Tick =  (simp$sim$starttick + 1):simp$sim$endtick, Value = metric))
}
#' Calculates the number of changed cells in land uses in a series of raster data.
#' TODO test
#' TODO limit to specific AFT(s)
#' 
#' @param simp
#' @param aft if \code{NULL}, all AFTs are considered
#' @param dataname
#' @param asvector if \code{TRUE} metric is returned as named vector
#' @return data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric (number of changed cells (integer)) when \code{asvector == TRUE}
#' 
#' @author Sascha Holzhauer
#' @export
metric_rasters_changedcells <- function(simp, aft = NULL, dataname = "raster_landUseIndex",
		asvector = FALSE) {
	# read raster maps if not stored
	raster_aft <- input_raster_get(simp = simp, dataname= dataname)[[1]]
	
	last = raster_aft$Raster[[1]]
	metric <- c()
	aggchanges <- last
	aggchanges[] <- 0
	for (raster in raster_aft$Raster[2:length(raster_aft$Raster)]) {
		# raster <- raster_aft$Raster[2]
		changes <- (raster[[1]] - last)
		changes[changes != 0] <- 1
		aggchanges <- max(aggchanges, changes)
		metric <-  c(metric, raster::cellStats(aggchanges, "sum"))
		last <- raster[[1]]
	}
	#raster::plot(changes)
	return(if(asvector) setNames(metric, (simp$sim$starttick + 1):simp$sim$endtick) else data.frame(
								Metric = paste("VarChangesCells", if (!is.null(aft)) "_", aft, sep=""),
								Tick =  (simp$sim$starttick + 1):simp$sim$endtick, Value = metric))
}