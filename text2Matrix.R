text2Matrix <- function(boilerplate, lowfreq = 50, highfreq = Inf, Sparsity = 0.8, dictionary = FALSE, sparse = TRUE){
  
  #Libraries
  require(tm)
  require(SnowballC)
  require(RJSONIO)
  
  #Create Boilerplates
  boilerplate <- lapply(boilerplate, fromJSON, simply = FALSE, USE.NAMES = FALSE, simplifyWithNames = FALSE)
  boilerplate <- lapply(boilerplate, function(x){return(dummy <- c(x$title, x$body, x$url))})
  
  #Use TM Package to create corpus
  corpus <- Corpus(VectorSource(boilerplate))
  corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeWords, union(stopwords("en"), stopwords("SMART")))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stemDocument)
  
  if (dictionary == FALSE & sparse == TRUE){
    
    #Matrix <- removeSparseTerms(TermDocumentMatrix(corpus), sparse = Sparsity)
    #dictionaryExtracted <- findFreqTerms(Matrix, lowfreq = lowfreq, highfreq = highfreq)
    #as.matrix(DocumentTermMatrix(corpus, list(dictionary = dictionaryExtracted))) 
    
    #Matrix <- removeSparseTerms(weightTfIdf(TermDocumentMatrix(corpus)), sparse = Sparsity)
    #dictionaryExtracted <- findFreqTerms(Matrix, lowfreq = lowfreq, highfreq = highfreq)
    #as.matrix(weightTfIdf(DocumentTermMatrix(corpus, list(dictionary = dictionaryExtracted)))) 
          
    #as.matrix(removeSparseTerms(weightTfIdf(DocumentTermMatrix(corpus)), sparse = Sparsity)) 
    
    #as.matrix(weightTfIdf(removeSparseTerms(DocumentTermMatrix(corpus), sparse = Sparsity)))
    
    weightTfIdf(DocumentTermMatrix(corpus))
    
  
    }else if(dictionary == FALSE & sparse == FALSE){
      
    as.matrix(weightTfIdf(removeSparseTerms(DocumentTermMatrix(corpus), sparse = Sparsity)))
      
    }else{
    
    as.matrix(DocumentTermMatrix(corpus, list(dictionary = dictionary)))
    
  }
  
}