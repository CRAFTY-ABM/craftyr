library(roxygen2)
library("devtools")
full <- TRUE

# has_devel() 	# does not work because of 'C:\Program' is not recognized as an internal or 
# external command, operable program or batch file.

pkgsName <- "craftyr"
setwd("C:/Data/LURG/workspace/")

#create(pkgsName)

setwd(paste("./", pkgsName, sep=""))
#devtools::use_vignette("craftyr-intro")
#devtools::use_vignette("craftyr-raster")

if (full) {
	document()
}

setwd("..")

devtools::build(pkgsName)

if (full) {
	devtools::build_vignettes(pkgsName)
}


install(pkgsName)
# devtools::install_bitbucket("geoslurg/craftyr@default")
# for eddie (use qlogin session!):
# module load R/3.0.1
# devtools::install_bitbucket("craftyr", username="geoslurg", ref="default")
if (full) {
	browseVignettes("craftyr")
}


## prepare data objects
#cellData <- read.csv("./inst/extdata/NEEDS ADAPTATION/Scenario-0-0-Region-Cell-2010.csv")
#save(cellData, file="./data/cellData.rda")