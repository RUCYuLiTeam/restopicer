ElasticSearch Configuration
================
### oracle java
#####Option 1: install without apt-get
- wget --no-check-certificate --no-cookies --header "Cookie: oraclelicense=accept-securebackup-cookie" http://download.oracle.com/otn-pub/java/jdk/7u79-b15/jdk-7u79-linux-x64.tar.gz
- tar xf jdk-7u79-linux-x64.tar.gz
- export JAVA_HOME=/usr/java/jdk1.7.0_79
- export PATH=$PATH:$JAVA_HOME/bin:$JAVA_HOME/jre/bin

#####Option 2: install with apt-get
- sudo apt-get install python-software-properties
- sudo add-apt-repository ppa:webupd8team/java
- sudo apt-get update
- sudo apt-get install oracle-java7-installer

##### managing java
- sudo update-alternatives --install "/usr/bin/java" "java" "/usr/java/jdk1.7.0_79/bin/java" 1
- sudo update-alternatives --install "/usr/bin/javac" "javac" "/usr/java/jdk1.7.0_79/bin/javac" 1
- sudo update-alternatives --install "/usr/bin/javaws" "javaws" "/usr/java/jdk1.7.0_79/bin/javaws" 1
- sudo update-alternatives --config java
- sudo update-alternatives --config javac

### install elasticsearch and plugin
- wget https://download.elastic.co/elasticsearch/elasticsearch/elasticsearch-1.6.2.tar.gz
- tar xf elasticsearch-1.6.2.tar.gz
- create a new file in /etc/init.d/ named elasticsearch
- chmod +x /etc/init.d/elasticsearch
- execute command: update-rc.d elasticsearch defaults
- sudo ln -s /etc/init.d/elasticsearch /bin/elasticsearch
- create a file at /etc/elasticsearch/ named elasticsearch.yml and config
- for cluster, data node can use unicast to search for the master node by checking the master url
- plugin -install mobz/elasticsearch-head
- plugin -install lukas-vlcek/bigdesk
- plugin -i elasticsearch/marvel/latest
- plugin -install elasticsearch/elasticsearch-lang-javascript/2.6.0
- plugin -u https://github.com/NLPchina/elasticsearch-sql/releases/download/1.3.4/elasticsearch-sql-1.3.4.zip --install sql
- kibana

### run elasticsearch
- under the folder run : elasticsearch -Xmx15g -Xms15g
- run: sudo elasticsearch start

### using plugin and restful service
- http://127.0.0.1:9200/_plugin/head/
- http://127.0.0.1:9200/_plugin/bigdesk
- http://127.0.0.1:9200/_cat

### useful scripts and resources
- ulimit -l unlimited
- "TSKILL R" in windows cmd
- killall -9 elasticsearch
- POSTMAN

### reference
- http://xapian.org/docs/bm25.html
- https://www.elastic.co/guide/en/elasticsearch/reference/1.6/modules-plugins.html
- https://github.com/mobz/elasticsearch-head
- https://github.com/lukas-vlcek/bigdesk
- https://github.com/elastic/elasticsearch-lang-javascript
- https://github.com/NLPchina/elasticsearch-sql/
- https://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-update-settings.html#_balanced_shards
- https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-percentile-aggregation.html#search-aggregations-metrics-percentile-aggregation-compression
- http://blog.csdn.net/an74520/article/details/8219814
