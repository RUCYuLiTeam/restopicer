networkdata <- NULL
networkdata$edgelist <- read.table(file = 'data/Cit-HepPh.txt',header = F,stringsAsFactors = F,blank.lines.skip = T,skipNul = T,
                                   colClasses = c("character","character"),col.names = c("to","from"))
head(networkdata$edgelist,10)
networkdata$nodeinfo <- read.table(file = 'data/cit-HepPh-dates.txt',header = F,stringsAsFactors = F,blank.lines.skip = T,skipNul = T,
                                   colClasses = c("character","Date"),col.names = c("node","date"))
head(networkdata$nodeinfo,10)
