searchDashSpace <- function(dash, string) {
  bool <- grepl(paste0(
    "[[:space:]]+",
    dash,
    "[[:space:]]*|[[:space:]]*",
    dash,
    "[[:space:]]+"
  ),
  string,
  ignore.case = T
  )
  
  
  return(bool)
}

searchDashNoSpace <- function(dash, string) {
  bool <- grepl(paste0(
    "[^[:space:]]",
    dash,
    "|",
    dash,
    "[^[:space:]]"
  ),
  string,
  ignore.case = T
  )
  
  
  return(bool)
}





checkDash <- function(doc, ignoreSpace) {
  sentenceArray <- tokenizers::tokenize_sentences(doc)
  sentenceArray <- unlist(sentenceArray)
  
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  i <- 1
  while (i <= length(sentenceArray)) {
    if (!ignoreSpace) {
      if (searchDashSpace("–", sentenceArray[i]) | searchDash("-", sentenceArray[i])) {
        x <- c("Space beside en dash", i, sentenceArray[i])
        df <- rbind(df, x)
      }
      
      if (searchDashSpace("—", sentenceArray[i])) {
        x <- c("Space beside em dash", i, sentenceArray[i])
        df <- rbind(df, x)
      }
    }else{
      if (searchDashNoSpace("–", sentenceArray[i]) | searchDash("-", sentenceArray[i])) {
        x <- c("No space beside en dash", i, sentenceArray[i])
        df <- rbind(df, x)
      }
      
      if (searchDashNoSpace("—", sentenceArray[i])) {
        x <- c("No space beside em dash", i, sentenceArray[i])
        df <- rbind(df, x)
      }
    }
    if (grepl("--", sentenceArray[i], fixed=T)) {
      x <- c("\"--\" used, did you mean \"—\"?", i, sentenceArray[i])
      df <- rbind(df, x)
    }
    
    i <- i + 1
  }
  colnames(df) <- c("Error", "sentence_index", "sentence")
  return(df)
}