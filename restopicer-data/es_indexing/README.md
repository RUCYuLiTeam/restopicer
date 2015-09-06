Indexing on ElasticSearch
================
### Run Shell for Small Data (one split without _index and _type in bulkblock)
- `curl -s -XPUT http://127.0.0.1:9200/restopicer --data-binary @indexmodel.json`
- `curl -s -H 'content-type': 'application/json;charset=utf8' -XPOST http://127.0.0.1:9200/restopicer/paper/_bulk --data-binary @F:/Desktop/restopicer/restopicer-data/es_indexing/output/es_paper_with_issue_requests > /dev/null`

### Run Shell for Small Data (one split with _index and _type in bulkblock)
- `curl -s -XPUT http://127.0.0.1:9200/restopicer --data-binary @indexmodel.json`
- `curl -s -H 'content-type': 'application/json;charset=utf8' -XPOST http://127.0.0.1:9200/restopicer/_bulk --data-binary @F:/Desktop/restopicer/restopicer-data/es_indexing/output/es_paper_with_issue_requests > /dev/null`

### Run DataFileSplit.sh and BulkIndexing.sh
- each split should smaller than 20MB
- modify the two paths (/data and /data_split) in the DataFileSplit.sh code
- `bash ./FileSplit.sh`
- modify the two paths (ip location: http://127.0.0.1:9200 and /data_split) in the BulkIndexing.sh code
- `bash ./BulkIndexing.sh output > mylog &`
