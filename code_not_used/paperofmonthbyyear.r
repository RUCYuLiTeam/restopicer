library(ggplot2)
#Cit.HepPh.Unique<-unique(Cit.HepPh)
#cit.HepPh.dates.Unique<-unique(cit.HepPh.dates)

paperMonth<-data.frame(month=1:12,paperCount=0)

year<-2000
for(m in 1:12){
  if(m<10){
    month<-as.character(paste('0',m,sep = ""))
  }else{
    month<-as.character(m)
  }
  pattern <- paste(year,'-',month,'-*', sep = "")
  group <- grep(pattern, cit.HepPh.dates.Unique[,2], 
                ignore.case = FALSE, perl = FALSE, value = FALSE, 
                fixed = FALSE, useBytes = FALSE, invert = FALSE)
  paperMonth[paperMonth$month==m,]$paperCount <- length(group)
}
myplot<-ggplot(paperMonth,aes(x=as.character(month),y=paperCount,fill=month))
myplot+geom_bar(stat="identity")+geom_point()+
  geom_text(aes(label=paperCount),hjust=0.5, vjust=-0.5 )+
  labs(x='Month', y=paste('paperCount of ',year, sep = ""))