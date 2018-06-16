#' Plot competition functions for services
#' @param simp 
#' @param functions 
#' @param xrange Vector of two. The x range to plot
#' @param yrange Vector of two. The y range to plot
#' @param filename filename for figure
#' @param returnplot if true the ggplot object is returned
#' @return plot if \code{returnplot == true}
#' 
#' @author Sascha Holzhauer
#' @export
visualise_competition_funcs <- function(simp, functions, xrange = c(-3,3), yrange = c(-1,1), 
		filename = "competitionFunctions",
		returnplot = FALSE,
		ggplotAddons = NULL) {
	
	futile.logger::flog.debug("Print competition functions for services...",
			name="craftyr.visualise.competition")
	
	simp$fig$numcols <- 1
	simp$fig$numfigs <- 1
	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "param", sep="/"), 
			filename = filename)
	
	stat_functions <- unlist(mapply(function(fun, name) {
						eval(substitute(
										expr = {
											ggplot2::stat_function(fun = fun, ggplot2::aes(colour = service))
										}, env = list(service=name)))
					}, functions, names(functions)))
	
	f <- ggplot2::ggplot(data.frame(x = xrange, y = yrange), ggplot2::aes(x))
	f <- f + stat_functions +
			ggplot2::xlab("Residual demand") +
			ggplot2::ylab("Benefit") +
			ggplot2::scale_colour_manual("Services", values = simp$colours$Service) +
			ggplotAddons
	print(f)
	simp$fig$close()
	if (returnplot) return(f)
}
#' Histogram of competitiveness per AFT: stacked: below/above GU threshold
#' 
#' @param simp 
#' @param data
#' @param facet_ncol if \code{NULL} ticks will be as rows, agents as columns. Otherwise, facets will 
#' 		  distinguished by ticks only (and facet_cols specifies the number of columns)
#' @param filename 
#' @param numbins number of bins to bin data
#' @param title plot title
#' @param ggplotaddons list of ggplot2 elements
#' @param setfigdims if \code{TRUE} \code{simp$fig$height} and \code{simp$fig$width} are set appropriately
#' @param returnplot if true the ggplot object is returned
#' @return facet histogram plot 
#' 
#' @author Sascha Holzhauer
#' @export
visualise_competition_prealloc <- function(simp, data, facet_ncol = length(simp$mdata$aftNames) - 1,
		filename = paste("PreAllocationCompetition", simp$sim$id, sep="_"),
		numbins = 20, binwidth = diff(range(data$PreAllocCompetitiveness))/numbins, title = NULL,
		ggplotaddons = NULL, setfigdims = TRUE, storename = NULL, returnplot = FALSE) {
	
	if (!is.data.frame(data)) {
		data <- do.call(rbind, data)
	}
	
	if(length(data) == 0) {
		R.oo:throw.default("Data is empty!")
	}
	
	if(setfigdims) {
		simp$fig$height	<- simp$fig$height * if(is.null(facet_ncol)) length(unique(data$Tick)) else length(unique(data$Tick))/facet_ncol
		simp$fig$width	<- simp$fig$width * if(is.null(facet_ncol)) length(simp$mdata$aftNames) - 1 else facet_ncol 
	}
	
	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "bars", sep="/"), filename = filename)
	
	scaleFillElem <- ggplot2::scale_fill_manual(name="GivingUp", 
			values = c("1"="red", "0" = "green"),
			labels = c("1"="Giving up", "0"="Persisting"))
	
	if (!is.null(facet_ncol)) {
		facetElem = ggplot2::facet_wrap(as.formula("~ Tick"), ncol = facet_ncol)	
	} else {
		facetElem = ggplot2::facet_grid(as.formula(paste("Tick ~", "AFT")), scales="free_y")	
	}
	
	p1 <- ggplot2::ggplot(data, ggplot2::aes(x=PreAllocCompetitiveness, fill=GU)) + 
			ggplot2::geom_bar(binwidth = binwidth) + 
			facetElem  +
			scaleFillElem +
			{if (!is.null(title)) ggplot2::labs(title = title) else NULL} +
			ggplotaddons
	
	if(!is.null(storename)) {
		assign(storename, p1)
		input_tools_save(simp, storename)
	}
	
	print(p1)
	simp$fig$close()
	if (returnplot) return(p1)
}
#' Histogram of competitiveness per AFT: stacked: below/above GU threshold
#' 
#' @param simp 
#' @param data 
#' @param facet_ncol 
#' @param filename 
#' @param numbins 
#' @param binwidth 
#' @param title 
#' @param ggplotaddons 
#' @param setfigdims 
#' @param storename 
#' @param returnplot if true the ggplot object is returned
#' @return bar plot
#' 
#' @author Sascha Holzhauer
#' @export
visualise_competition_preallocTable <- function(simp, data, facet_ncol = length(simp$mdata$aftNames) - 1,
		filename = paste("PreAllocationCompetition", simp$sim$id, sep="_"),
		numbins = 20, binwidth = NULL, title = NULL,
		ggplotaddons = NULL, setfigdims = TRUE, storename = NULL, returnplot = FALSE) {
	
	if (!is.data.frame(data)) {
		data <- do.call(rbind, data)
	}
	
	if(length(data[,1]) == 0) {
		R.oo::throw.default("Data is empty!")
	}
	
	names(data)[names(data)=="PreAllocLandUseIndex"] <- "AFT"
	# bin data according to numbins
	data$Comp <- as.numeric(data$Comp)
	
	# determine breaks:
	dx <- diff(rx <- range(data$Comp, na.rm = TRUE))
	if (dx == 0) {
		dx <- abs(rx[1L])
		breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000, 
				length.out = numbins + 1)
	} else {
		breaks <- seq.int(rx[1L], rx[2L], length.out = numbins + 1)
		breaks[c(1L, numbins + 1)] <- c(rx[1L] - dx/1000, rx[2L] + 
						dx/1000)
	}
	labels <- sprintf("%.2f", breaks[-1] - dx/(2*numbins))
	
	# NOTE: If the range of values is too narrow, two decimal places might not sufficient to satisfy number of bins
	binneddata <- plyr::ddply(.data = data, c("Tick","AFT"), function(df){
			# df <- data[data$Tick == 2011 & data$AFT == "1",]
			df$bin <- cut(df$Comp, breaks = breaks, labels = labels)
			df <- aggregate(subset(df, select=c("Above", "Below")), 
					by = list("PreAllocCompetitiveness" = df$bin), FUN = "sum")
			df	
		})
	
	melteddata <- reshape2::melt(binneddata, variable.name="GU", id.vars= c("Tick", "AFT", 
					"PreAllocCompetitiveness"), 
		direction="long", value.name = "Number")

	if(setfigdims) {
		simp$fig$height	<- simp$fig$height * if(is.null(facet_ncol)) length(unique(data$Tick)) else length(unique(data$Tick))/facet_ncol
		simp$fig$width	<- simp$fig$width * if(is.null(facet_ncol)) length(simp$mdata$aftNames) - 1 else facet_ncol 
	}
	
	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "bars", sep="/"), filename = filename)
	
	scaleFillElem <- ggplot2::scale_fill_manual(name="GivingUp", 
			values = c("Above" = "green", "Below"="red"),
			labels = c("Above" = "Persisting", "Below"="Giving up"))
	
	if (!is.null(facet_ncol)) {
		facetElem = ggplot2::facet_wrap(as.formula("~ Tick"), ncol = facet_ncol)	
	} else {
		facetElem = ggplot2::facet_grid(as.formula(paste("Tick ~", "AFT")), scales="free_y")	
	}
	
	#binwidth = binwidth, 
	p1 <- ggplot2::ggplot(melteddata, ggplot2::aes(x=PreAllocCompetitiveness, y=Number, fill=GU)) + 
			ggplot2::geom_bar( stat="identity") + 
			facetElem  +
			scaleFillElem +
			ggplot2::scale_x_discrete(breaks = function(x) x[seq(1, length(x), 
										length.out=simp$fig$numticks)]) +
			{if (!is.null(title)) ggplot2::labs(title = title) else NULL} +
			ggplotaddons
	
	if(!is.null(storename)) {
		assign(storename, p1)
		input_tools_save(simp, storename)
	}
	
	print(p1)
	simp$fig$close()
	if (returnplot) return(p1)
}