library(ape) #utility fns
library(geiger) #utilty fns
library(OUwie)

#You can use code you wrote for the correlation exercise here.


VisualizeData.continuous <- function(phy, data) {
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
	quartz(height=7,width=5)
	layout(matrix(c(1,2,1,3,4,4), 3,2, byrow=T))
	plot.phylo(phy)
	if(phy$Nnode == length(phy$tip.label)-1){
		print("Tree is fully resolved")
	}else{
		print("Tree is not fully resolved")
		}
	hist(as.numeric(data$data[,1]), main="histogram of trait 1")
	print("Shapiro-Wilk Normality Test for trait 1:"); print(normalTest(data$data[,1]))
	hist(as.numeric(data$data[,2]), main="histogram of trait 2")
	print("Shapiro-Wilk Normality Test for trait 2:"); print(normalTest(data$data[,2]))
	plot(as.numeric(data$data[,1]), as.numeric(data$data[,2]), main="scatterplot of traits 1 and 2")
}

VisualizeData.discrete <- function(phy, data) {
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
	quartz(height=7,width=5)
	layout(matrix(c(1), 1,1)); par(mar=c(0.5,0.5,3,0.5))
	plot.phylo(phy, x.lim=c(0, 2.25), y.lim=c(0,25))
	tiplabels(data $data[,1], frame="n", adj=-15)
	tiplabels(data $data[,2], frame="n", adj=-21)
	text(c(1.7, 2), length(phy$tip.label)+1, c("trait 1", "trait 2"))
}


CleanData <- function(phy, data) {
	treedata(phy, data, sort=TRUE)
}

RunSingleOUwieModel<-function(model, phy, data) {
	print(paste("Now starting model",model))
	return(OUwie(phy, data, model, simmap.tree=FALSE, diagn=FALSE))	
}
