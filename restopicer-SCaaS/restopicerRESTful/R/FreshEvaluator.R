yearDiffReciprocalFreshEval <- function(publication_year,max_year=2014,offset=1){
  delta <- log(max_year - as.integer(publication_year) + 1) - offset
  exp(1-delta)/(1+exp(1-delta))
}