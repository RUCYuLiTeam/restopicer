Cit.HepPh.Unique<-unique(Cit.HepPh)
cit.HepPh.dates.Unique<-unique(cit.HepPh.dates)

Cit.HepPh.WithDates<-data.frame()

dateOfPaper <- function(paperNodeId){
  cit.HepPh.dates.Unique[(cit.HepPh.dates.Unique$V1==paperNodeId),]$V2[1]
}
#dateOfPaper(112008)301097
j<-1
for(i in 1:nrow(Cit.HepPh.Unique)){
  date1<-dateOfPaper(Cit.HepPh.Unique[i,1])
  date2<-dateOfPaper(Cit.HepPh.Unique[i,2])
  if((!is.na(date1))&&(!is.na(date2))){
    Cit.HepPh.WithDates[j,1]<-Cit.HepPh.Unique[i,1]
    Cit.HepPh.WithDates[j,2]<-date1
    Cit.HepPh.WithDates[j,3]<-Cit.HepPh.Unique[i,2]
    Cit.HepPh.WithDates[j,4]<-date2
    j=j+1
  }
}