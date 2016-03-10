#You can use code you wrote for the correlation exercise here.
setwd("~/Desktop/2016Spring/Phylometh/ContinuousTraits")

source("ContinuousFunctions.R")
tree <- read.tree("eucTree.tre")
tree$tip.label <- gsub("_"," ", tree$tip.label); tree$tip.label[8] <- "E. tenuiramis"
discrete.data <- read.csv(file="eucDataDiscretized.csv", stringsAsFactors=FALSE, row.names=1) #death to factors.
continuous.data <- read.csv(file="eucDataContinuous.csv", stringsAsFactors=FALSE, row.names=1) #death to factors.

cleaned.continuous <- CleanData(tree, continuous.data[,c(2,4)])
cleaned.discrete <- CleanData(tree, discrete.data)
VisualizeData.continuous(tree, cleaned.continuous)
VisualizeData.discrete(tree, cleaned.discrete)

#First, start basic. What is the rate of evolution of your trait on the tree? 

BM1 <- fitContinuous(tree, log(cleaned.continuous$data), model="BM")

print(paste("The rate of evolution of mean elevation is", round(BM1$ala.Elevation$opt$sigsq, 2), "change in log meters per arbitrary time unit"))
print(paste("The rate of evolution of range envelope is", round(BM1$range.envelope$opt$sigsq, 2), "change in log 10 square kilometers per arbitrary time unit"))
#Important: What are the rates of evolution? In what units?




#OUwie runs:
#This takes longer than you may be used to. 
#We're a bit obsessive about doing multiple starts and in general
#performing a thorough numerical search. It took you 3+ years
#to get the data, may as well take an extra five minutes to 
#get an accurate answer

discreteTrait <- data.frame(species=tree$tip.label, elevation= as.integer(cleaned.discrete$data[,2]+1)) 

treeAncER <- rayDISC(tree, discreteTrait, model="ER", node.states="marginal")
treeAncSYM <- rayDISC(tree, discreteTrait, model="SYM", node.states="marginal")
treeAncARD <- rayDISC(tree, discreteTrait, model="ARD", node.states="marginal")
c(treeAncER$AICc, treeAncSYM $AICc, treeAncARD $AICc) # Use Equal Rates model

trait <- data.frame(species=tree$tip.label, elevation= as.integer(cleaned.discrete$data[,2]+1), aggregation=as.numeric(cleaned.continuous$data[,1]))

nodeBased.OUMV <- OUwie(treeAncER$phy, trait, model="OUMV")
print(nodeBased.OUMV)
#What do the numbers mean?

#Now run all OUwie models:
models <- c("BM1","BMS","OU1","OUM","OUMV","OUMA","OUMVA")
results <- lapply(models, RunSingleOUwieModel, phy=treeAnc$phy, data=trait)


modelfit <- data.frame(model = c(results[[1]]$model, results[[2]]$model, results[[3]]$model, results[[4]]$model, results[[5]]$model, results[[6]]$model, results[[7]]$model), deltaAIC = c(results[[1]]$AICc, results[[2]]$AICc, results[[3]]$AICc, results[[4]]$AICc, results[[5]]$AICc, results[[6]]$AICc, results[[7]]$AICc)-min(c(results[[1]]$AICc, results[[2]]$AICc, results[[3]]$AICc, results[[4]]$AICc, results[[5]]$AICc, results[[6]]$AICc, results[[7]]$AICc)))

print(paste("The", modelfit$model[which(modelfit$deltaAIC==0)], "fits the best.", "The next best-fitting model is", modelfit$model[min(modelfit$deltaAIC[-which(modelfit$deltaAIC==0)])], "with a deltaAIC of", round(modelfit$deltaAIC[which(rank(modelfit$deltaAIC)==2)],2)))
