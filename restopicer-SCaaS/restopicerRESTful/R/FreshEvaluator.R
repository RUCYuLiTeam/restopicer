yearDiffReciprocalFreshEval <- function(publication_year,max_year=2013){
  1/(max_year - as.integer(publication_year)+1)
}