#####
# for matrix/edgelist form network processing
#####
# for matrix
runMaxCompartOfMatrix <- function(bi_matrix){
  bi_matrix <- as.matrix(bi_matrix)
  bi_compart <- compart(bi_matrix)
  compart.belong <- data.frame(row=row.names(bi_compart$cweb),compart=-apply(bi_compart$cweb,1,FUN = min))
  size.compart <-  ddply(compart.belong,.(compart),.fun = nrow)
  colnames(size.compart) <- c("compart","cnt")
  max.size.compart <- size.compart[which.max(size.compart$cnt),]$compart
  row.compart <- compart.belong[compart.belong$compart == max.size.compart,]
  bi_MaxCompart <- bi_matrix[row.names(bi_matrix) %in% row.compart$row,]
  bi_MaxCompart <- bi_MaxCompart[,colSums(bi_MaxCompart)!=0]
  #bi_MaxCompart[which(bi_MaxCompart!=0)] <- 1 # transform to binary
  bi_MaxCompart
}
# for edgelist
runBipartiteProjecting <- function (net, method = "length", directed = F){
  if (is.null(attributes(net)$tnet)) {
    if (ncol(net) == 3) {
      net <- as.tnet(net, type = "weighted two-mode tnet")
    }
    else {
      net <- as.tnet(net, type = "binary two-mode tnet")
    }
  }
  if (attributes(net)$tnet != "binary two-mode tnet" & attributes(net)$tnet != "weighted two-mode tnet") 
    stop("Network not loaded properly")
  net2 <- net
  if (attributes(net)$tnet == "binary two-mode tnet") 
    net2 <- cbind(net2, w = 1)
  # i is the projected part
  # w is the weight of i-p
  net2 <- net2[order(net2[, "i"], net2[, "p"]), ]
  np <- table(net2[, "p"])
  # np is the degree of p
  net2 <- merge(net2, cbind(p = as.numeric(row.names(np)),np = np))
  net1 <- merge(net2, cbind(j = net2[, "i"], p = net2[, "p"], w_j = net2[, "w"]))
  colnames(net1) <- c("p","i","w_i","np","j","w_j")
  net1 <- net1[net1[, "i"] != net1[, "j"], c("i", "w_i", "j", "w_j", "np")]
  net1 <- net1[order(net1[, "i"], net1[, "j"]), ]
  index <- !duplicated(net1[, c("i", "j")])
  w <- switch(method, binary = rep(1, sum(index)), 
              length = tapply(net1[,"np"], cumsum(index), length), 
              sumchoose = tapply(net1[,"np"], cumsum(index), function(a) sum(1/choose(a,2))))
  net1 <- cbind(net1[index, c("i", "w_i", "j", "w_j")], w = as.numeric(w))
  if(!directed){
    dup_Bool <- net1$i>net1$j
    tmp <- net1[dup_Bool,]$i
    net1[dup_Bool,]$i <- net1[dup_Bool,]$j
    net1[dup_Bool,]$j <- tmp
    net1 <- net1[order(net1[, "i"], net1[, "j"]), ]
    index <- !duplicated(net1[, c("i", "j")])
    w <- switch(method, binary = rep(1, sum(index)),
                length = tapply(net1[,"w"], cumsum(index), unique),
                sumchoose = tapply(net1[,"w"], cumsum(index), unique))
    net1 <- cbind(net1[index, c("i", "w_i", "j", "w_j")], w = as.numeric(w))
  }
  return(net1)
}