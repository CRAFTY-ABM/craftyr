#' Determine number changes between ticks for a specific (combination of) land use(s)
#' 
#' @param simp 
#' @param cdata list of celldata (\code{data.frame)} to compare
#' @param ncol 
#' @param ggplotaddon
#' @param returnplot if true the ggplot object is returned
#' @return vector of number of changes
#' 
#' @author Sascha Holzhauer
#' @export
metric_celldata_changes <- function(simp, cdata, ncol = 1, title = "AFT-Changes", ggplotaddon = NULL, 
		regions = simp$sim$regions,
		addcountryshapes = FALSE, plotunchanged = TRUE, returnplot = FALSE) {
	
	results <- c()
	for (i in 2:length(cdata)) {
		diffcells <- cdata[[1]]
		diffcells$LandUseIndex <- NA
		
		# excluding cells which are or were zero not necessary?
		#indices <- cdata[[i]]$LandUseIndex != 0 | cdata[[i-1]]$LandUseIndex != 0
		#diffcells$LandUseIndex[indices] <- (cdata[[i]]$LandUseIndex - cdata[[i - 1]]$LandUseIndex)[indices]
		
		diffcells$LandUseIndex <- (cdata[[i]]$LandUseIndex - cdata[[i - 1]]$LandUseIndex)
		results <- c(results, sum(diffcells$LandUseIndex==0))
	}
}

# Functions
# TODO
metric_celldata_connectivity_global <- function(sip, data, type) {
	
	
	datatemp<-data
	names(datatemp)[names(datatemp) == "LandUseIndex"] <- "Agent"
	datatemp$homo[datatemp$Agent==type]<-1
	
	matrixdata<-data.frame(datatemp$X,datatemp$Y,datatemp$homo)
	names(matrixdata)<-c("x","y","homo")
	
	typematrix<-acast(matrixdata, x~y, value.var="homo") 
	
	# Extract cells of correct type and remove cells at edge, to prevent subscripts going out of bounds
	
	# TODO How to do that for arbitrary areas?
	dtx<-matrixdata$x[matrixdata$homo==1 & matrixdata$x>1 & matrixdata$y>1 & matrixdata$x<200 & matrixdata$y<200]  
	dty<-matrixdata$y[matrixdata$homo==1 & matrixdata$x>1 & matrixdata$y>1 & matrixdata$x<200 & matrixdata$y<200]  
	
	
	no.type<-length(dtx)
	props<-rep(0,no.type)
	
	for(i in c(1:no.type)) {
		x<-dtx[i]
		y<-dty[i]
		
		nghbrs<-sum(typematrix[x,y+1],typematrix[x+1,y+1],typematrix[x+1,y],typematrix[x+1,y-1],
				typematrix[x,y-1],typematrix[x-1,y-1],typematrix[x-1,y],typematrix[x-1,y+1])
		
		props[i]<-nghbrs/8
	}
	
	homo.prop<-mean(props)
	return(homo.prop)
}