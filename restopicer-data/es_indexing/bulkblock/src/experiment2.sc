def stripQuotes(s: String): String = {
  if(s.startsWith("\"") && s.endsWith("\"")) s.dropRight(1).drop(1)
  else s
}
val dataFile = io.Source.fromFile("C:\\Users\\zezzhang\\Desktop\\restopicer\\restopicer-data\\sample_data\\es_paper_with_issue.csv").getLines.toArray
val dataFile_authors = io.Source.fromFile("C:\\Users\\zezzhang\\Desktop\\restopicer\\restopicer-data\\sample_data\\paper_author_info.csv").getLines.toArray
val dataHeader = dataFile.apply(0).split("\t")
val dataHeader_authors = dataFile_authors.apply(0).split("\t")
val data_authors=dataFile_authors.map(x=>x.split("\t").map(x => "\""+stripQuotes(x).replaceAll("\"","")+"\""))

var dataLine = dataFile.apply(2).split("\t").map(x => "\""+stripQuotes(x).replaceAll("\"","")+"\"").zip(dataHeader)

var authors = data_authors.filter(x=>x.apply(0).equals(dataLine.apply(0)._1)).map(x=>x.zip(dataHeader_authors).drop(1))

authors.map(x => "{ "+ x.map(pair=>pair._2+" : "+pair._1).mkString(" , ")+" }").mkString(" , ")