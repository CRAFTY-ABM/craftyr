#' Store CSV cell data (also in splitted form)
#' @param simp 
#' @param dataname 
#' @param tickinterval  
#' 
#' @author Sascha Holzhauer
#' @export
hl_store_csvcelldata <- function(simp, dataname = "csv_LandUseIndex", tickinterval = 10) {
	cdata <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
			pertick = TRUE, 
			starttick = if(!is.null(simp$sim$starttick)) simp$sim$starttick else simp$tech$mintick,
			endtick = if(!is.null(simp$sim$endtick)) simp$sim$endtick else simp$tech$maxtick,
			tickinterval = tickinterval,
			attachfileinfo = TRUE, bindrows = TRUE)
	assign(dataname, cdata)
	input_tools_save(simp, dataname)
	
	cdata <- split(cdata, list(cdata$Tick,cdata$Runid))
	assign(paste(dataname, "split", sep="_"), cdata)
	input_tools_save(simp, paste(dataname, "split", sep="_"))
}