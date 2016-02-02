#' Visualise takeovers of land uses as transition plot
#' 
#' @param simp SIMulation Properties
#' \itemize{
#' 	\item evtl. simp$sim$starttick or simp$tech$mintick
#' 	\item evtl. simp$sim$endtick or simp$tech$maxtick
#' 	\item simp$sim$runids
#'  \item simp$mdata$aftNames
#' 	\item simp$dirs$output$figures
#' 	\item simp$fig$init
#' 	\item simp$sim$id
#' 	\item simp$colours$AFT
#' 	\item simp$fig$close
#' }
#' @param data takeover data with column "Tick" and one for each AFT (afts flow from row to column)
#' @param startpopulation needs to contain a column 'Number' which contains agent numbers of the start tick
#' @param starttick first tick to consider 
#' @param endtick  last tick to consider
#' @param tickinterval interval between transition columns. Note: tickinterval is based on starttick which defaults to 0!
#' @param type_of_arrow 
#' @param transitionthreshold minimum number of transitions to show an arrow
#' @return plots figure
#' 
#' @family takeovers
#' @author Sascha Holzhauer
#' @export
output_visualise_takeovers <- function(simp,
		data,
		startpopulation = NULL,
		starttick = if(!is.null(simp$sim$starttick)) simp$sim$starttick else simp$tech$mintick,
		endtick = if(!is.null(simp$sim$endtick)) simp$sim$endtick else simp$tech$maxtick, 
		tickinterval = 1,
		type_of_arrow = "grid",
		transitionthreshold = 0,
		aftnames = simp$mdata$aftNames[-1]) {
	
	# TODO allow for multiple runids (changes to startpopulation and plot as facet eg.)
	ticks = seq(starttick, endtick, tickinterval)
	data[data <= transitionthreshold] <- 0
	
	if (length(unique(data[["AFT"]])) != length(startpopulation$Agent)) {
		R.oo::throw.default(sprintf("Agents in start population (%s) do not match agents in data (%)!"),
				paste(startpopulation$Agent, collapse="/"),
				paste(unique(data[["AFT"]]), collapse="/"))
	}
	
	for (run_id in simp$sim$runids) {
		cols <- colnames(data)
		trans <- aggregate(data[,colnames(data) %in% aftnames], by=list( 
						Tick=data[["Tick"]], AFT=data[["AFT"]]), FUN=sum)
		
		population <- startpopulation$Number
		
		transitions <- list()
		populations <- data.frame(population)

		validticks <- ticks[ticks %in% unique(data[["Tick"]])]
		
		if (length(validticks) == 0) {
			R.oo:throw.default(sprintf("Requested ticks (%s) do not match available ticks (%s)!",
							paste(ticks, collapse=","),
							paste(unique(data[["Tick"]]), collapse=",")
							))
		}
		
		for (tick in validticks) {
			t <- aggregate(subset(trans, Tick >= tick & Tick < tick + tickinterval, select=aftnames), 
					by = list(
							AFT=trans[trans$Tick >= tick & trans$Tick < tick + tickinterval,"AFT"]),
					FUN=sum)
			
			rownames(t) <- t$AFT
			
			aftNumbers <- names(aftnames)
			names(aftNumbers) <- aftnames
			
			# order data according to AFT numbers
			t <- t[order(aftNumbers[as.character(t$AFT)]),]
			t <- as.table(as.matrix(t[, -c(1)]))
			
			transitions[[as.character(tick)]] <-  t
			
			pop1 <- population
			pop2 <- population - rowSums(t) + colSums(t)
			maxPop <- max(sum(pop1), sum(pop2))
			population <- pop2
			
			populations <- cbind(populations, pop2)
		}
		populations <- populations / maxPop
		
		filename <- output_tools_getDefaultFilename(simp, postfix = 
						paste("AftTakeOvers_", shbasic::shbasic_condenseRunids(
								unique(data$Region)),"_", starttick, "-", endtick, "_", simp$sim$id, sep="")) 
		simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "takeovers", sep="/"), filename = filename)
		
		# TODO integerate absolute AFT numbers
		# TODO enable the combination of AFTs into groups
		aftindex <- names(simp$colours$AFT) %in% names(aftnames)[aftnames %in% colnames(data)]
		
		shGmisc::transitionPlot(transitions,
				cex = 1.2,
				overlap_add_width = 1.3,
				type_of_arrow = type_of_arrow, 
				#min_lwd = unit(2, "mm"), 
				#max_lwd = unit(10, "mm"),
				fill_start_box = as.matrix(data.frame(simp$colours$AFT[aftindex], simp$colours$AFT[aftindex])),
				box_prop = as.matrix(populations),
				
				box_label = c(ticks, endtick),
				box_label_pos = "bottom",
				box_label_cex = 1.2,
				box_width = 1/(length(transitions)*4),
				main = "AFT Transitions")
		
#			transitionPlot(transitions[[1]],
#					overlap_add_width = 1.3,
#					type_of_arrow = "grid", 
#					#min_lwd = unit(2, "mm"), 
#					#max_lwd = unit(10, "mm"),
#					fill_start_box = as.matrix(data.frame(lix$aftColours, lix$aftColours)),
#					box_prop = as.matrix(populations[,c(1,2)]),
#					
#					box_label = c(tick, tick + lix$everyTick),
#					box_label_pos = "bottom",
#					box_width = 1/6)
		
	simp$fig$close()
	}
}
#' Visualise AFT fluctuations
#' @param simp 
#' @param data 
#' @param starttick 
#' @param endtick 
#' @param tickinterval 
#' @param title 
#' @param filename  
#' @return plot
#' 
#' @family takeovers
#' @author Sascha Holzhauer
#' @export
output_visualise_aftFluctuations <- function(simp,
		data,
		starttick = if(!is.null(simp$sim$starttick)) simp$sim$starttick else simp$tech$mintick,
		endtick = if(!is.null(simp$sim$endtick)) simp$sim$endtick else simp$tech$maxtick, 
		tickinterval = 1,
		title = "AFT Fluctuations",
		filename = title) {
	
	df <- data[data$Tick == 2010,]
	fluctuations <- plyr::ddply(data, Runid~Tick, function(df) {
				m <- df[,simp$mdata$aftNames]
				netto <- colSums(m) - rowSums(m)
				data.frame(AFT = names(netto), sum = netto)
			})
	
	fluctuations <- fluctuations[fluctuations$Tick >= starttick & fluctuations$Tick <= endtick,]
	# replace AFT names by AFT serial ids (for correct colours)
	aftNumbers <- names(simp$mdata$aftNames)
	names(aftNumbers) <- simp$mdata$aftNames
	fluctuations$AFT <- aftNumbers[levels(fluctuations$AFT)[fluctuations$AFT]]

	visualise_lines(simp, fluctuations, "sum", title = title,
			colour_column = "AFT", colour_legenditemnames = simp$mdata$aftNames,
			linetype_column = "Runid",
			filename = filename,
			alpha=0.7)
}