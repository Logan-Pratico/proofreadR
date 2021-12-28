getSentenceWordCountFrequency <- function(doc, ...){
  sentenceArray <- tokenizers::tokenize_sentences(doc)
  sentenceArray <- unlist(sentenceArray)
  
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  i <- 1
  while(i <= length(sentenceArray)){
    sentenceArrayNoPunct <- gsub('[[:punct:]]+','',sentenceArray[i])
    
    wordCountSentence <- reshape2::melt(table(strsplit(tolower(sentenceArrayNoPunct), " ")))
    wordCountSentence <- dplyr::arrange(wordCountSentence, desc(wordCountSentence$value))
    wordCountSentence <- dplyr::filter(wordCountSentence, wordCountSentence$value > ...) 
    
    wordCountSentence <- dplyr::filter(wordCountSentence, Var1 != "the" & Var1 != "of" & Var1 != "to" 
                                       & Var1 != "and" & Var1 != "a" & Var1 != "in" & Var1 != "this"
                                       & Var1 != "as" & Var1 != "for" & Var1 != "that" & Var1 != "is"
                                       & Var1 != "be")
    
    if(nrow(wordCountSentence) > 0){
      wordCountSentence <- dplyr::mutate(wordCountSentence, sentence_index=i)
    }
    
    df <- rbind(df, wordCountSentence)
    i <- i + 1
  }
  colnames(df) <- c("word","sentence_count", "sentence_index")
  return(df)
}
