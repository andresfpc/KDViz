#' Replace a list of words by a pair list
#'
#' A process similar to lemmatization with a custom dictionary file in the form of a data frame of custom words and its corresponding replacement word
#'
#' @param corpus a corpus object
#' @param wordsFile a file with custom words to be replaced (first column with the replacement words, second column with the original words; separated by tabulation)
#'
#' @return a corpus with replaced words
#'
#' @examples
#' 
#'
#' @export
#'
#' @import tm
#' @importFrom utils read.delim flush.console
ReplaceByList <-
function(corpus, wordsFile) {

   termFreq <- TermFrequency(corpus)

   wordSet <- read.delim(wordsFile, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
	colnames(wordSet) <- c("Replace", "Word")
	wordSubset <- wordSet[which(wordSet$Word %in% termFreq$Term), ]

	replaceTime <- Sys.time()

   nWords <- dim(wordSubset)[1]
   cat(nWords,"words to replace:\n"); flush.console()
   for(l in 1:nWords) {
      corpus <- tm::tm_map(corpus, content_transformer(
         function(x) gsub(pattern = paste0("\\b", wordSubset[l, 2], "\\b"), replacement = wordSubset[l, 1], x)
      ))
      cat(" ", round((l/nWords)*100, 1), "% of words replaced\r", sep = ""); flush.console()
   }
   
   replaceTime <- Sys.time() - replaceTime

   return(corpus)
}
