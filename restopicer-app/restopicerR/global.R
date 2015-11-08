rm(list = ls(envir = globalenv()))
#dependencies
library(shiny)
library(shinythemes)
library(shinydashboard)
library(RMySQL)
library(RJSONIO)
library(RCurl)
library(dplyr)
#sourcing code
source("functions.R")

# elastic search config
elastic_search_server <- "http://127.0.0.1:9200"
indexname <- "restopicer"
typename  <- "paper"
es_location <- paste(elastic_search_server,indexname,typename,sep = "/")
searchLocation <- paste(es_location,"_search",sep = "/")
# database config

#initial parameter
relevent_N <- 5
preference_w <- 0.5
quality_w <- 0.5
composite_N <- 5
explore_N <- 0
