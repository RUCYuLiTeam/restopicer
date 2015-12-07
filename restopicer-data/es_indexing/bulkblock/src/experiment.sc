val dataFile = io.Source.fromFile("C:\\Users\\zezzhang\\Desktop\\restopicer\\restopicer-data\\sample_data\\es_paper_with_issue.csv").getLines.toArray
val dataFile_keywords = io.Source.fromFile("C:\\Users\\zezzhang\\Desktop\\restopicer\\restopicer-data\\sample_data\\paper_keyword.csv").getLines.toArray
def stripQuotes(s: String): String = {
  if(s.startsWith("\"") && s.endsWith("\"")) s.dropRight(1).drop(1)
  else s
}
val dataHeader = dataFile.apply(0).split("\t")
var dataLine = dataFile.apply(2).split("\t").map(x => "\""+stripQuotes(x).replaceAll("\"","")+"\"").zip(dataHeader)
var keywords = dataFile_keywords
  .map(x=>x.split("\t").map(x => "\""+stripQuotes(x).replaceAll("\"","")+"\""))
  //.find(x=>x.apply(0).equals(dataLine.apply(0)._1))
  .filter(x=>x.apply(0).equals(dataLine.apply(0)._1))
  .map(x=>x.apply(1))
