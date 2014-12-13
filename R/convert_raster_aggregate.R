convert_raster_flatlist <- function(inforasterdata, aggregatefunction) {
	flatdata <- lapply(raster_adoption, function(infodata) {
				idata <- apply(infodata, MARGIN=1, function(infodataitem) {
							infodataitem$Adoption <- cellStats(infodataitem$Raster, stat="sum")
							infodataitem$Raster <- NULL
							infodataitem
						})
				do.call(rbind, idata)})
			

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