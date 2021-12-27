getTotalWordCountFrequency <- function(doc, ...){
  doc_no_punctuation <- gsub('[[:punct:]]+','',doc)
  listOfWords <- paste0(doc_no_punctuation, collapse=" ")
  
  wordCount <- reshape2::melt(table(strsplit(tolower(listOfWords), " "))) 
  wordCount <- dplyr::arrange(wordCount, desc(wordCount$value)) 
  wordCount <- dplyr::filter(wordCount, wordCount$value > ...)
  
  wordCount <- dplyr::filter(wordCount, Var1 != "the" & Var1 != "of" & Var1 != "to" 
                             & Var1 != "and" & Var1 != "a" & Var1 != "in" & Var1 != "this"
                             & Var1 != "as" & Var1 != "for" & Var1 != "that" & Var1 != "is"
                             & Var1 != "be") 
  
  return(wordCount)
  
  
}