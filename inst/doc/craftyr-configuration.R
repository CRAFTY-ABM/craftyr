## ---- eval=FALSE, results="hide"-----------------------------------------
#  library(craftyr)
#  simp <- craftyr::param_getDefaultSimp()
#  simp$sim$filepartorder	<- c("regionalisation", "U", "regions", "U", "datatype")
#  cellnumdf <- input_csv_param_capitals_cellnumbers(simp = simp,
#  			regionpartfromend = 2, regionpartdevider = "_")
#   # order decreasingly according to number of cells:
#  cellnumdf[order(cellnumdf, decreasing = TRUE)]

## ---- eval=FALSE, results="hide"-----------------------------------------
#  library(craftyr)
#  simp <- craftyr::param_getDefaultSimp()
#  simp$sim$filepartorder	<- c("regionalisation", "U", "regions", "U", "datatype")
#  slotgroups <- input_csv_param_capitals_slotassignment(simp = simp, slotnums = 16,
#  			regionpartfromend = 2, regionpartdevider = "_")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  library(craftyr)
#  simp <- craftyr::param_getDefaultSimp()
#  simp$sim$folder 	<- ""
#  adjust_changecolumnnames(simp,  indir = simp$dirs$param$getparamdir(simp, "capitals"),
#  		colname_old = "Pasture", colname_new = "Grass", filepattern = "*.csv")

