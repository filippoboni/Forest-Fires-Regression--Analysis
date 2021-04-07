#set options
options(max.print = 100)

#read data
fires <- read.csv(file = "forestfires.csv",header = TRUE,stringsAsFactors = TRUE)
fires$X <- as.factor(fires$X)
fires$Y <- as.factor(fires$Y)

fires

#create column fire for logistic
fires$fire <- rep("No",length(fires$X))
fires$fire[fires$area>0] <-"Yes"