# Test for transition plot (original data)
# 
# Author: Sascha Holzhauer
###############################################################################


set.seed(1)
library(magrittr)
n <- 100
data <- 
		data.frame(
				Sex = sample(c("Male", "Female"),
						size = n,
						replace = TRUE,
						prob = c(.4, .6)),
				Charnley_class = sample(c("A", "B", "C"), 
						size = n, 
						replace = TRUE))
getProbs <- function(Chrnl_name){
	prob <- data.frame(
			A = 1/3 +
					(data$Sex == "Male") * .25 +
					(data$Sex != "Male") * -.25 +
					(data[[Chrnl_name]] %in% "B") * -.5 +
					(data[[Chrnl_name]] %in% "C") * -2 ,
			B = 1/3 +
					(data$Sex == "Male") * .1 + 
					(data$Sex != "Male") * -.05 +
					(data[[Chrnl_name]] == "C") * -2,
			C = 1/3 +
					(data$Sex == "Male") * -.25 +
					(data$Sex != "Male") * .25)
	
	# Remove negative probabilities
	t(apply(prob, 1, function(x) {
						if (any(x < 0)){
							x <- x - min(x) + .05
						}
						x
					}))
}

Ch_classes <- c("Charnley_class")
Ch_classes %<>% c(sprintf("%s_%dyr", Ch_classes, c(1,2,6)))
for (i in 1:length(Ch_classes)){
	if (i == 1)
		next;
	
	data[[Ch_classes[i]]] <- 
			apply(getProbs(Ch_classes[i-1]), 1, function(p)
						sample(c("A", "B", "C"), 
								size = 1, 
								prob = p)) %>%
			factor(levels = c("A", "B", "C"))
}


library(Gmisc)
transitions <- table(data$Charnley_class, data$Charnley_class_1yr) %>%
		getRefClass("Transition")$new(label=c("Before surgery", "1 year after"))
transitions$render()
