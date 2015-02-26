#######################################################################
# crafty demo for csv data visualisation of AFT allocation
# Last update: 	27.10.2014
# Author: 		Sascha Holzhauer
# TODO adapt to sample files
#######################################################################
source(simp$simpDefinition)   # read simp

data <- input_csv_data(simp, dataname = NULL, datatype = "Cell",pertick = TRUE,
		starttick = 2010, endtick = 2040, tickinterval = 30,
		attachfileinfo = TRUE, bindrows = TRUE)

d <- data[, colnames(data) %in% c("Tick", "Service.Meat", "Service.Cereal", "Service.Recreation",
				"Service.Timber", "Runid")]
d <- reshape2::melt(d, id.vars=c("Runid", "Tick"))
d <- aggregate(subset(d, select=c("value")), by = 
				list(Tick=d[, "Tick"], ID=d[,"Runid"], Service = d[,"variable"]), FUN=sum)

d$Service <- simp$mdata$conversion$services[d$Service]
colnames(d)[which(colnames(d)=="value")] <- "TotalProduction"
visualise_lines(simp, d, "TotalProduction", title = "Total Production",
		colour_column = "Service",
		linetype_column = "ID",
		linetype_legendtitle = simp$sim$rundesclabel,
		linetype_legenditemnames = simp$sim$rundesc,
		filename = paste("TotalProduction", 
				shbasic::shbasic_condenseRunids(data.frame(d)[, "ID"]), sep="_"),
		alpha=0.7)