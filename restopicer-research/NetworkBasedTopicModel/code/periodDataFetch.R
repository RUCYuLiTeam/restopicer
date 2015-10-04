fetchdata <- function(from_year=1994,to_year=2013){
  #rm(list = ls(envir = globalenv()))
  # setwd("F:/Desktop/restopicer/restopicer-research/NetworkBasedTopicModel/")
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
  magazines <- c("MIS QUARTERLY","INFORMATION SYSTEMS RESEARCH","INFORMATION & MANAGEMENT",
                 "DECISION SUPPORT SYSTEMS","ACM TRANSACTIONS ON INFORMATION SYSTEMS",
                 "JOURNAL OF MANAGEMENT INFORMATION SYSTEMS","ELECTRONIC COMMERCE RESEARCH",
                 "INTERNATIONAL JOURNAL OF ELECTRONIC COMMERCE","JOURNAL OF ELECTRONIC COMMERCE RESEARCH")

  # select research papers from paper_with_issue
  res <- dbSendQuery(conn, paste("SELECT * FROM paper_with_issue WHERE abstract <> '' AND publication_year >= ",from_year," AND publication_year <= ",to_year," AND full_source_title in ","('",paste(magazines,collapse = "','"),"')",sep=""))
  researchPapers <- dbFetch(res,n = -1)
  dbClearResult(res)
  # select research papers-keywords from paper_keyword in research papers
  res <- dbSendQuery(conn, paste("SELECT * FROM paper_keyword WHERE item_ut in","('",paste(researchPapers$item_ut,collapse = "','"),"')"))
  researchPapersKeywords <- dbFetch(res,n = -1)
  dbClearResult(res)
  # select research papers issue_subject_category from paper_issue_subject_category of research papers
  res <- dbSendQuery(conn, paste("SELECT * FROM paper_issue_subject_category WHERE item_ut in","('",paste(researchPapers$item_ut,collapse = "','"),"')"))
  researchPapersSubjectCategory <- dbFetch(res,n = -1)
  dbClearResult(res)
  # select research papers-authors from paper_author_info in research papers
  res <- dbSendQuery(conn, paste("SELECT * FROM paper_author_info WHERE item_ut in","('",paste(researchPapers$item_ut,collapse = "','"),"')"))
  researchPapersAuthors <- dbFetch(res,n = -1)
  dbClearResult(res)
  # select research papers reference from paper_reference in research papers
  res <- dbSendQuery(conn, paste("SELECT * FROM paper_reference WHERE item_ut in","('",paste(researchPapers$item_ut,collapse = "','"),"')"))
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
  save(file = paste("rdata/research_",from_year,"_",to_year,".RData",sep = ""),list = memoryWhiteList)
  ##############
  # END FOR LTM research data Fetching From MySQL DB
  ##############
}