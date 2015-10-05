#####
# experiment result analysis BEGIN
#####
source("code/methods.R")
save(result_linkcomm,result_cnm,
     result_linkcomm_0.1_percolation,
     result_linkcomm_0.15_percolation,
     result_linkcomm_0.2_percolation,
     file = "rdata/research_2011_2013/myanalysis.RData")
linkcomm <- result_linkcomm$linkcomm
CNM <- result_cnm$fastgreedy
linkcomm_0.1_percolation <- result_linkcomm_0.1_percolation$linkcomm
linkcomm_0.15_percolation <- result_linkcomm_0.15_percolation$linkcomm
linkcomm_0.2_percolation <- result_linkcomm_0.2_percolation$linkcomm
plotCompositePerformance(linkcomm=linkcomm,linkcomm_0.2_percolation,linkcomm_0.15_percolation,linkcomm_0.1_percolation,CNM=CNM)
#####
# experiment END
#####