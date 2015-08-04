#' TODO to complete
#' TODO think about un-regular regions (real-world) (use shapefiles?)
#' 
#' @param simp 
#' @param data 
#' @param regionwidth 
#' @param regionheight 
#' @return regional diversity metric
#' 
#' @author Calum Brown
#' @author Sascha Holzhauer
#' 
#' @export
metric_regional_diversity <- function(simp, data, regionwidth = 50, regionheight = regionwidth) {

	min_x <- min(data[simp$csv$cname_x])
	max_x <- max(data[simp$csv$cname_x])
	
	min_y <- min(data[simp$csv$cname_y])
	max_y <- max(data[simp$csv$cname_y])
	
	
	xlims <- seq(from = min_x, to=max_x, by=regionwidth)
	xlims <- seq(from = min_y, to=max_y, by=regionheight)
	
	diversity<-rep(0, length(xlims) * length(ylims))
	
	k <- 0
	for(i in c(1:length(xlims))) {
		for (j in c(1:length(ylims))) {
			k <- k+1
			
			region<-subset(data,data$X>xlims[i] & data$X<=xlims[i+1] & data$Y>ylims[j] & data$Y<=ylims[j+1])
			
			for (aft in simp$sim$AftNames) {
				# TODO
			}
			NCCereal<-length(region$Agent[region$Agent=="NC_Cereal"])
			CCereal<-length(region$Agent[region$Agent=="C_Cereal"])
			NCLivestock<-length(region$Agent[region$Agent=="NC_Livestock"])
			CLivestock<-length(region$Agent[region$Agent=="C_Livestock"])
			Forester<-length(region$Agent[region$Agent=="Forester"])
			Conservationist<-length(region$Agent[region$Agent=="Conservationist"])
			Biofuel<-length(region$Agent[region$Agent=="BiofuelFarmer"])
			tot<-length(region$Agent)
			
			region.Sdiv<-1-(((NCCereal/tot)^2)+((CCereal/tot)^2)+((NCLivestock/tot)^2)+((CLivestock/tot)^2)+
						((Forester/tot)^2)+((Conservationist/tot)^2)+((Biofuel/tot)^2))     
			diversity[k]<-region.Sdiv
		}
	}
	result<-mean(diversity)
	return(result)
}

# Functions
metric_global_connectivity <- function(sip, data, type) {
	
	
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