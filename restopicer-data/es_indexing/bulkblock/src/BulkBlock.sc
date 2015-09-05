import java.io.PrintWriter
val dataFile = io.Source.fromFile("F:\\Desktop\\restopicer\\restopicer-data\\sample_data\\es_paper_with_issue.csv").getLines.toArray
def stripQuotes(s: String): String = {
  if(s.startsWith("\"") && s.endsWith("\"")) s.dropRight(1).drop(1)
  else s
}
val out = new PrintWriter("F:\\Desktop\\restopicer\\restopicer-data\\es_indexing\\output\\es_paper_with_issue_requests")
val dataHeader = dataFile.apply(0).split(",")
for(i <- 1 to (dataFile.length-1)){
  var dataLine = dataFile.apply(i).split(",").zip(dataHeader)
  //action_and_meta_data
  out.print("{ \"index\" : { \"_index\" : \"restopicer\", \"_type\" : \"paper\", \"_id\" : "+ dataLine.apply(0)._1 +" } }\n")
  //optional_source
  out.print("{ "+dataLine.map(pair => pair._2+" : "+pair._1).mkString(" , ")+" }\n")
}
out.flush()
out.close()