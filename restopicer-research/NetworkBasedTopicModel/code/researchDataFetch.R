rm(list = ls(envir = globalenv()))
setwd("F:/Desktop/restopicer/restopicer-research/LinkTopicModel")
#####
# required library
#####
source(file = "code/utilities.R")
library(RMySQL)
##############
# LTM research data Fetching From MySQL DB
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
# research
# generate research data for model explaination (small size)
#####
# select research papers from paper_with_issue
res <- dbSendQuery(conn, "SELECT * FROM paper_with_issue WHERE abstract <> '' AND publication_year >= 2014")
researchPapers <- dbFetch(res,n = -1)
dbClearResult(res)
# select research papers-keywords from paper_keyword in research papers
res <- dbSendQuery(conn, paste("SELECT * FROM paper_keyword WHERE item_ut in","(",paste(researchPapers$item_ut,collapse = ","),")"))
researchPapersKeywords <- dbFetch(res,n = -1)
dbClearResult(res)
# select research papers issue_subject_category from paper_issue_subject_category of research papers
res <- dbSendQuery(conn, paste("SELECT * FROM paper_issue_subject_category WHERE item_ut in","(",paste(researchPapers$item_ut,collapse = ","),")"))
researchPapersSubjectCategory <- dbFetch(res,n = -1)
dbClearResult(res)
# select research papers-authors from paper_author_info in research papers
res <- dbSendQuery(conn, paste("SELECT * FROM paper_author_info WHERE item_ut in","(",paste(researchPapers$item_ut,collapse = ","),")"))
researchPapersAuthors <- dbFetch(res,n = -1)
dbClearResult(res)
# select research papers reference from paper_reference in research papers
res <- dbSendQuery(conn, paste("SELECT * FROM paper_reference WHERE item_ut in","(",paste(researchPapers$item_ut,collapse = ","),")"))
researchPapersReference <- dbFetch(res,n = -1)
dbClearResult(res)
# close
dbDisconnect(conn)
# clean useless object
addPersistentObjects("researchPapers")
addPersistentObjects("researchPapersKeywords")
addPersistentObjects("researchPapersSubjectCategory")
addPersistentObjects("researchPapersAuthors")
addPersistentObjects("researchPapersReference")
rmTempObject()
# save .RData
save(file = "rdata/research.RData",list = memoryWhiteList)
##############
# END FOR LTM research data Fetching From MySQL DB
##############