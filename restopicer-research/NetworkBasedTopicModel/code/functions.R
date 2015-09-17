# required library
library(plyr)
library(tm)
library(bipartite)
library(tnet)
library(igraph)
library(slam)
library(wordcloud)
library(MASS)
library(pROC)
#library(ROCR)
library(ggplot2)
library(entropy)
#####
# for matrix/edgelist form network processing
# mostly not used
#####
source("code/functions_for_network_processing.R")
#####
# self-defined community detection method or similarity calculations
# 1.self-defined community detection method
# 2.self-defined similarity method
#####
source("code/functions_for_community_detection.R")
#####
# community is like topic (from community to topic)
# 1.community_member is a bianry bipartite matrix
# 2.topic_member is a weighted community_member (also a bipartite matrix)
# 3.one doc belongs to many communities/topics
# 4.in some methods, one member belongs to many communities/topics
# 5.supported network form are edgelist, matrix and belongto_list
#####
source("code/functions_for_community_to_topic.R")
######
# community evaluation functions and plot report
# on community_member test,topic_member test and doc_topic test
######
source("code/functions_for_community_evaluation.R") # still bug
#####
# for general matrix plotreport or network drawing
#####
source("code/functions_for_report_plot.R")
source("code/functions_for_network_plot.R")
#####
# for network form transformation
# 1.transform in edgelist, matrix and belongto_list form
# 2.convert data(edgelist or matrix) to graph object (from igraph)
#####
source("code/functions_for_network_transformation.R")
#####
# the last part (must be the last to import)
#####
source("code/utilities.R")
#####
# THE FIN
#####