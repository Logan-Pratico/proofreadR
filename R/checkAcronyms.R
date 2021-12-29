check.acronyms <- function(doc){
  
  matches <- regmatches(doc, gregexpr("\\([A-Z]+\\)", doc))
  val <- unlist(matches)
  
  found <- rep(NA, length(val))
  
  i <- 1
  while(i <= length(val)){
    found[i] <- grepl(paste0("[^\\(]",stringr::str_sub(val[i],2,-2), "[^\\)]"), 
                       doc, ignore.case = F)
    i <- i + 1
  }
  
  df <- cbind(val, found)
  
  df <-
    dplyr::filter(as.data.frame(df), found == F)
  
  df <- 
    dplyr::mutate(df, found="Acronym not used")
  
  return(df)
}