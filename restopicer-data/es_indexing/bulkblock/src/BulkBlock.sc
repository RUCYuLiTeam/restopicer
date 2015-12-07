import java.io.PrintWriter
def stripQuotes(s: String): String = {
  if(s.startsWith("\"") && s.endsWith("\"")) s.dropRight(1).drop(1)
  else s
}
val dataFile = io.Source.fromFile("C:\\Users\\zezzhang\\Desktop\\restopicer\\restopicer-data\\sample_data\\es_paper_with_issue.csv").getLines.toArray
val data_keywords = io.Source.fromFile("C:\\Users\\zezzhang\\Desktop\\restopicer\\restopicer-data\\sample_data\\paper_keyword.csv").getLines.toArray
  .map(x=>x.split("\t").map(x => "\""+stripQuotes(x).replaceAll("\"","")+"\""))
val dataFile_authors = io.Source.fromFile("C:\\Users\\zezzhang\\Desktop\\restopicer\\restopicer-data\\sample_data\\paper_author_info.csv").getLines.toArray
val out = new PrintWriter("C:\\Users\\zezzhang\\Desktop\\restopicer\\restopicer-data\\es_indexing\\output\\es_paper_requests")
val dataHeader = dataFile.apply(0).split("\t")
val dataHeader_authors = dataFile_authors.apply(0).split("\t")
val data_authors=dataFile_authors.map(x=>x.split("\t").map(x => "\""+stripQuotes(x).replaceAll("\"","")+"\""))
for(i <- 1 to (dataFile.length-1)){
  var dataLine = dataFile.apply(i).split("\t").map(x => "\""+stripQuotes(x).replaceAll("\"","")+"\"").zip(dataHeader)
  var keywords = data_keywords.filter(x=>x.apply(0).equals(dataLine.apply(0)._1)).map(x=>x.apply(1))
  var authors = data_authors.filter(x=>x.apply(0).equals(dataLine.apply(0)._1)).map(x=>x.zip(dataHeader_authors).drop(1))
  //action_and_meta_data
  out.print("{ \"index\" : { \"_index\" : \"restopicer\", \"_type\" : \"paper\", \"_id\" : "+ dataLine.apply(0)._1 +" } }\n")
  //optional_source
  out.print("{ "+dataLine.map(pair => pair._2+" : "+pair._1).mkString(" , ")+
    " , \"authors\" : ["+authors.map(x => "{ "+ x.map(pair=>pair._2+" : "+pair._1).mkString(" , ")+" }").mkString(" , ")+"]"
    +" , \"keywords\" : ["+keywords.mkString(" , ")+"] }\n")
}
out.flush()
out.close()
