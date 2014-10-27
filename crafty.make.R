library(roxygen2)
library("devtools")

# has_devel() 	# does not work because of 'C:\Program' is not recognized as an internal or 
# external command, operable program or batch file.

pkgsName <- "craftyr"
setwd("C:/Data/LURG/workspace/")

#create(pkgsName)

setwd(paste("./", pkgsName, sep=""))
#devtools::use_vignette("craftyr-intro")
#devtools::use_vignette("craftyr-raster")

document()

setwd("..")


devtools::build_vignettes(pkgsName)
devtools::build(pkgsName)

install(pkgsName)
# devtools::install_bitbucket("geoslurg/craftyr")
browseVignettes("craftyr")