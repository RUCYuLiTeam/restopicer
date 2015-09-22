rm(list = ls(envir = globalenv()))
# setwd("F:/Desktop/restopicer/restopicer-research/NetworkBasedTopicModel")
#####
# required library
#####
source(file = "code/utilities.R")
library(RMySQL)
##############
# demo data Fetching From MySQL DB
##############
conn <- dbConnect(MySQL(), dbname = "restopicer_resource_info")
dbListTables(conn)
#####
# meta data of paper (research object is papers and keywords)
#####
dbListFields(conn, "paper")# not used
dbListFields(conn, "paper_with_issue")### most useful
dbListFields(conn, "paper_author_info")## very useful
dbListFields(conn, "paper_issue_subject_category")### most useful
dbListFields(conn, "paper_keyword")### most useful
dbListFields(conn, "paper_keyword_author")# useful but not used
dbListFields(conn, "paper_keyword_plus")# not used
dbListFields(conn, "paper_reference")## very useful
#####
# DEMO
# generate research data for model explaination (small size)
#####
# select demo papers from paper_with_issue
res <- dbSendQuery(conn, "SELECT * FROM paper_with_issue WHERE abstract <> '' AND publication_year >= 2014")
demoPapers <- dbFetch(res,n = -1)
dbClearResult(res)
# select demo papers-keywords from paper_keyword in demo papers
res <- dbSendQuery(conn, paste("SELECT * FROM paper_keyword WHERE item_ut in","('",paste(demoPapers$item_ut,collapse = "','"),"')"))
demoPapersKeywords <- dbFetch(res,n = -1)
dbClearResult(res)
# select demo papers issue_subject_category from paper_issue_subject_category of demo papers
res <- dbSendQuery(conn, paste("SELECT * FROM paper_issue_subject_category WHERE item_ut in","('",paste(demoPapers$item_ut,collapse = "','"),"')"))
demoPapersSubjectCategory <- dbFetch(res,n = -1)
dbClearResult(res)
# select demo papers-authors from paper_author_info in demo papers
res <- dbSendQuery(conn, paste("SELECT * FROM paper_author_info WHERE item_ut in","('",paste(demoPapers$item_ut,collapse = "','"),"')"))
demoPapersAuthors <- dbFetch(res,n = -1)
dbClearResult(res)
# select demo papers reference from paper_reference in demo papers
res <- dbSendQuery(conn, paste("SELECT * FROM paper_reference WHERE item_ut in","('",paste(demoPapers$item_ut,collapse = "','"),"')"))
demoPapersReference <- dbFetch(res,n = -1)
dbClearResult(res)
# close
dbDisconnect(conn)
# clean useless object
addPersistentObjects("demoPapers")
addPersistentObjects("demoPapersKeywords")
addPersistentObjects("demoPapersSubjectCategory")
addPersistentObjects("demoPapersAuthors")
addPersistentObjects("demoPapersReference")
rmTempObject()
# save .RData
save(file = "rdata/demo.RData",list = memoryWhiteList)
##############
# END FOR demo data Fetching From MySQL DB
##############