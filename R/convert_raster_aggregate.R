convert_raster_aggregate <- function(rasters, aggregatefunction) {
	if (!is.null(func) && !is.na(func)) {
		data <- data.frame(value = eval(parse(text=func)))
	} else if (aggregate) {
		data <- data.frame(value = mean(data))
	}
	
	data$Tick <- year
	data$Scenario <- scenario
	data$ID <- run_id
	
	output <- rbind(output, data)
}