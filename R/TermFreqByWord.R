#' Term frequency by a specific words
#'
#' Returns the frequency of the terms containing a specific word
#'
#' @param termFreqTable a TermFrequency table
#' @param word a custom word to be matched
#'
#' @return a list of terms and their respective frecuencies
#'
#' @examples
#' 
#'
#' @export
TermFreqByWord <-
function(termFreqTable, word) {
   TFByWord <- termFreqTable[grep(word, termFreqTable$Term), ]
   
   return(TFByWord)
}
