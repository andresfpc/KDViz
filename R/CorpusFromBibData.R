#' Corpus from an article database
#'
#' Obtaining of a corpus from an article database
#'
#' @param bibData a vector containing the key terms to search
#' @param bibUnits a vector containing bibliometric units of analysis (e.g., Title, Abstract, Keywords, Journal, Authors, Year)
#' @param controlList a vector of transformations that will be applied to the corpus
#' @param stopwords a vector of stopwords to be removed from the corpus
#' @param wordsToRemove a vector of custom words to be removed
#' @param replaceWords a data frame of custom words and its corresponding replacement word
#'
#' @return a corpus object
#'
#' @examples
#' 
#'
#' @export
#'
#' @import tm
#' @import stringr
#' @importFrom utils flush.console read.delim
CorpusFromBibData <-
function(bibData, bibUnits, controlList, stopwords, wordsToRemove, replaceWords) {
   cat("Corpus processing from bibliometric data in progress...\n\n")
   flush.console()
   
   dataCorp <- data.frame(bibData[, colnames(bibData) %in% bibUnits])
   rownames(dataCorp) <- rownames(bibData)

   if(missing(stopwords)) {
      stopwords = FALSE
   }
   if(missing(wordsToRemove)) {
      wordsToRemove = c("")
   }
   
   corpusTime <- Sys.time()
   
   CORPUS <- tm::Corpus(DataframeSource(dataCorp))
   
   if("stripWhitespace" %in% controlList) {
   	cat("Collapsing multiple whitespace characters to a single one...\n")
   	flush.console()

      CORPUS <- tm::tm_map(CORPUS, stripWhitespace)
   }
   
   CORPUS <- tm::tm_map(CORPUS, content_transformer(
      function(x) stringr::str_replace_all(pattern="[-]", replacement="_", x)
   ))
   
   CORPUS <- tm::tm_map(CORPUS, content_transformer(
      function(x) stringr::str_replace_all(pattern = "[_]+", replacement = "_", x)
   ))
   
   CORPUS <- tm::tm_map(CORPUS, content_transformer(
      function(x) stringr::str_replace_all(pattern = "[^[:alnum:][_]]", replacement = " ", x)
   ))
   
   CORPUS <- tm::tm_map(CORPUS, content_transformer(tolower))
   
   CORPUS <- tm::tm_map(CORPUS, PlainTextDocument)
   
   if(stopwords == TRUE) {
      cat("Removing stopwords...\n")
   	flush.console()

      # Kevin Bougé stop words list (https://sites.google.com/site/kevinbouge/stopwords-lists)
      kbStopwords <- as.character(read.delim("stopwords_en.txt", header = FALSE)[,1])
      
      CORPUS <- tm::tm_map(CORPUS, removeWords, kbStopwords)
   }
   
   if(wordsToRemove != c("")) {
      cat("Removing custom words...\n")
   	flush.console()
      CORPUS <- tm::tm_map(CORPUS, removeWords, wordsToRemove)
   }
   
   if("removeNumbers" %in% controlList) {
      cat("Removing numbers...\n")
   	flush.console()

      CORPUS <- tm::tm_map(CORPUS, removeNumbers)
   }
   
   if(missing(replaceWords)) {
      CORPUS <- CORPUS
   } else {
      CORPUS <- ReplaceByList(CORPUS, wordsFile = replaceWords)
   }
   
   corpusTime <- Sys.time() - corpusTime
   cat("\n\nCorpus process finished\n\n")
   flush.console()
   
   return(CORPUS)
}
