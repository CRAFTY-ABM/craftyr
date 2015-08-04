#' Reads marginal utilties and prepares the data for plotting. Stores data and returns.
#' @param simp 
#' @param filename 
#' @return data.frame
#' 
#' @author Sascha Holzhauer
#' @export
hl_marginalutilities <- function(simp, filename = paste(simp$dirs$output$rdata, "MarginalUtilities.csv", sep="/"),
		storerdata = TRUE) {
	utilities = read.csv(filename, colClasses = "numeric")
	csv_MarginalUtilitites_melt = reshape2::melt(utilities, variable.name="Service", 
			id.vars= c("Year"), 
			direction="long")
	colnames(csv_MarginalUtilitites_melt)[colnames(csv_MarginalUtilitites_melt) == "Year"] <- "Tick"
	csv_MarginalUtilitites_melt$Runid <- 0
	
	if (storerdata) {
		input_tools_save(simp, "csv_MarginalUtilitites_melt")
	}
	
	visualise_lines(simp, csv_MarginalUtilitites_melt, "value", title = "Marginal Utilities",
			colour_column = "Service",
			filename = paste("MarginalUtilities", sep=""),
			alpha=0.7)
}
#' Print LaTeX table including run information for the version in \code{simp$sim$version} and
#' the '1st Run ID' in \code{simp$sim$runids[1]}.
#' @param simp 
#' @param filename 
#' @param rows
#' @return xtable plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_compileruninfos <- function (simp, filename = simp$dirs$output$runinfo, rows = NULL) {
	if (tools::file_ext(filename) == "ods") {
		require(readODS)
		runinfo <- read.ods(filename)[[1]][,1:simp$tech$runinfocolnumber]
		colnames(runinfo) <- runinfo[2,]
		runinfo <- runinfo[-c(1,2),]
		rinfo <- runinfo[runinfo["Version"] == simp$sim$version & 
						runinfo["1st Run ID"] == simp$sim$runids[1], ]
	} else if(tools::file_ext(filename) == "csv") {
		runinfo <- read.csv(filename, skip = 1)
		rinfo <- runinfo[runinfo$Version == simp$sim$version,]
	} else {
		Roo::throw.default("File extension ", tools::file_ext(filename)," not supported!")
	}
	
	if (length(rinfo) == 0) {
		Roo::throw.default("Runinfo table ", filename," does not contain a row for version " + 
						simp$sim$version, "!", sep="")
	}
	
	if (!is.null(rows)) {
		rinfo <- rinfo[,1:rows]
	}
	table <- xtable::xtable(t(rinfo),
			label="model.run.information", 
			caption="Model run information",
			align=c("r", "p{13cm}")
	)
	
	print(table, sanitize.colnames.function = identity,
			sanitize.rownames.function = identity)
}
#' Generates AFT key as CSV with columns 'Index' and 'LandUse'
#' 
#' @param simp  
#' \begin{itemize}
#' 	\item \code{simp$mdata$aftNames}
#'  \item \code{simp$dirs$output$data}
#'	\end{itemize}
#' 
#' @return csv file
#' 
#' @author Sascha Holzhauer
#' @export
hl_landindiceskey_csv <- function(simp) {
	filename <- sprintf("%s/LandUseIndicesKey.csv",
			simp$dirs$output$data)
	data <- data.frame("Index" = names(simp$mdata$aftNames), "LandUse" = simp$mdata$aftNames)
	shbasic::sh.ensurePath(filename, stripFilename = TRUE)
	write.csv(data, file = filename, row.names = FALSE)
}