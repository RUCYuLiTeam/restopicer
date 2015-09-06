library(VennDiagram)
year<-2002
pattern <- paste(year,'-*-*', sep = "")
group2Only <- grep(pattern, Cit.HepPh.WithDates[,2], 
              ignore.case = FALSE, perl = FALSE, value = FALSE, 
              fixed = FALSE, useBytes = FALSE, invert = FALSE)
group4Only <- grep(pattern, Cit.HepPh.WithDates[,4], 
               ignore.case = FALSE, perl = FALSE, value = FALSE, 
               fixed = FALSE, useBytes = FALSE, invert = FALSE)
venn.diagram(list(Written=group2Only,Cited=group4Only),fill=c("red","blue"),main=paste(year,'Venn Details', sep = ""),paste(year,'out.tiff', sep = ""))
