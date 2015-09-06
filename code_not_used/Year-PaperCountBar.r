library(ggplot2)
Cit.HepPh.Unique<-unique(Cit.HepPh)
cit.HepPh.dates.Unique<-unique(cit.HepPh.dates)

paperYear<-data.frame(year=1992:2002,paperCount=0)

for(year in 1992:2002){
  pattern <- paste(year,'-*-*', sep = "")
  group <- grep(pattern, cit.HepPh.dates.Unique[,2], 
       ignore.case = FALSE, perl = FALSE, value = FALSE, 
       fixed = FALSE, useBytes = FALSE, invert = FALSE)
  paperYear[paperYear$year==year,]$paperCount 
                  = nrow(cit.HepPh.dates.Unique[group,])
}
myplot<-ggplot(paperYear,aes(x=as.character(year),y=paperCount,fill=year))
myplot+geom_bar(stat="identity")+geom_point()+
  geom_text(aes(label=paperCount),hjust=0.5, vjust=-0.5 )+
  labs(x='year', y='paperCount')