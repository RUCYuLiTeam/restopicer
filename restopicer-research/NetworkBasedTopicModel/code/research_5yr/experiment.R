rm(list = ls(envir = globalenv()))
# not forget setwd("F:/Desktop/restopicer/restopicer-research/NetworkBasedTopicModel")
#####
# required data
#####
load(file = "rdata/research_2011_2013.RData")
source("code/periodDataFetch.R")
# if no data, pls run
fetchdata(from_year = 2011,to_year = 2013) 
#####
# required methods
#####
source("code/methods.R")
foldername <- "research_3yr"
plotPath=paste("output",foldername,sep="/")
addPersistentObjects("plotPath")
addPersistentObjects("foldername")