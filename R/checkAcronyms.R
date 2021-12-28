check.acronyms <- function(doc){
  
  matches <- regmatches(doc, gregexpr("\\([A-Z]+\\)", doc))
  val <- unlist(matches)
  
  i <- 1
  while(i <= length(val)){
    return[i] <- grepl(paste0("[^\\(]",stringr::str_sub(val[i],2,-2), "[^\\)]"), 
                       doc, ignore.case = F)
    i <- i + 1
  }
  
  df <- cbind(val, return)
  
  df <-
    dplyr::filter(as.data.frame(df), return == F)
  
  df <- 
    dplyr::mutate(df, return="Acronym not used")
  
  return(df)
}