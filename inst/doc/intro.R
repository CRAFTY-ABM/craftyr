## ----, eval=FALSE--------------------------------------------------------
#  source("./demo/simp-machine.R")
#  source(paste(simp$dirs$simp, "simp.R", sep=""))
#  
#  # read capital data from CSV into raster
#  data <- input_csv_param_capitals(simp)
#  rasters <- convert_2raster(simp, data, layers = simp$mdata$capitals)
#  names(rasters) <- "RegionA"
#  visualise_raster_printRawPlots(simp, rasters, legendtitle = "Capitals Example",
#  		factorial= FALSE, omitaxisticks = FALSE, ncol = 1, id="None")

