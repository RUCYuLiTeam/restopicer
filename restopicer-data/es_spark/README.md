# Spark and ElasticSearch Integretion
-------------------------------------------
### installation
1. maven repository
```
<dependency>
  <groupId>org.elasticsearch</groupId>
  <artifactId>elasticsearch-hadoop</artifactId>
  <version>2.1.1</version>
</dependency>
```

2. minimalistic jars for spark integration
```
<dependency>
  <groupId>org.elasticsearch</groupId>
  <artifactId>elasticsearch-spark_2.10</artifactId>
  <version>2.1.1</version>
</dependency>
```
or SBT
```
libraryDependencies += "org.elasticsearch" % "elasticsearch-spark_2.10" % "2.1.1"
```

3. elasticsearch configuration and writing in spark integration
The basic information are from [here](https://www.elastic.co/guide/en/elasticsearch/hadoop/current/configuration.html)!

```scala
import org.apache.spark.SparkContext    
import org.apache.spark.SparkContext._

import org.elasticsearch.spark._
import org.elasticsearch.spark.rdd.EsSpark

//configuration
val conf = new SparkConf().setAppName(appName).setMaster(master)
conf.set("es.index.auto.create", "true")
val sc = new SparkContext(conf)
//RDD can be saved to Elasticsearch
val numbers = Map("one" -> 1, "two" -> 2, "three" -> 3)
val airports = Map("arrival" -> "Otopeni", "SFO" -> "San Fran")
sc.makeRDD(Seq(numbers, airports)).saveToEs("spark/docs")
//through EsSpark
//define a case class
case class Trip(departure: String, arrival: String)
val upcomingTrip = Trip("OTP", "SFO")
val lastWeekTrip = Trip("MUC", "OTP")
val rdd = sc.makeRDD(Seq(upcomingTrip, lastWeekTrip))
EsSpark.saveToEs(rdd, "spark/docs", Map("es.mapping.id" -> "id"))
//Writing existing JSON to Elasticsearch
val json1 = """{"reason" : "business", "airport" : "SFO"}"""
val json2 = """{"participants" : 5, "airport" : "OTP"}"""
sc.makeRDD(Seq(json1, json2)).saveJsonToEs("spark/json-trips")
//Writing to dynamic/multi-resources
val game = Map("media_type"->"game","title" -> "FF VI","year" -> "1994")
val book = Map("media_type" -> "book","title" -> "Harry Potter","year" -> "2010")
val cd = Map("media_type" -> "music","title" -> "Surfing With The Alien")
sc.makeRDD(Seq(game, book, cd)).saveToEs("my-collection/{media-type}")
```

4. elasticsearch reading in spark integration
```scala
sc.esRDD("radio/artists")
sc.esRDD("radio/artists", "?q=me*")
```

5. elasticsearch by using spark SQL
```scala
import org.apache.spark.sql.SQLContext
import org.apache.spark.sql.SQLContext._

import org.elasticsearch.spark.sql._

...
// sc = existing SparkContext
val sqlContext = new SQLContext(sc)
// case class used to define the DataFrame
case class Person(name: String, surname: String, age: Int)
//  create DataFrame
val people = sc.textFile("people.txt")    
        .map(_.split(","))
        .map(p => Person(p(0), p(1), p(2).trim.toInt))
        .toDF()
people.saveToEs("spark/people")
```

for more detail, please check [document](https://www.elastic.co/guide/en/elasticsearch/hadoop/current/spark.html).
### reference
- https://www.elastic.co/guide/en/elasticsearch/hadoop/current/install.html
- https://github.com/barnybug/spark-elasticsearch-blogpost
- http://stackoverflow.com/questions/25253102/elasticsearch-to-spark-rdd
- https://www.elastic.co/guide/en/elasticsearch/hadoop/current/spark.html
- https://www.elastic.co/guide/en/elasticsearch/hadoop/current/arch.html
