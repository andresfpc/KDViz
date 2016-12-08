#' Document-term matrix from a corpus
#'
#' Obtaining of a binary document-term matrix from a corpus removing null rows
#'
#' @param corpus a corpus object
#' @param rowNames the row names of the matrix where the corpus comes from
#'
#' @return a document-term matrix
#'
#' @examples
#' 
#'
#' @export
#'
#' @import tm
DTMFromCorpus <- function(corpus, rowNames) {
   dtm <- tm::DocumentTermMatrix(corpus)
   dtm <- tm::weightBin(dtm)
   dtm <- as.matrix(dtm)
   if(missing(rowNames)) {
      row.names(dtm) <- paste0("A", 1:nrow(dtm))
   } else {
      row.names(dtm) <- rowNames
   }
   dtm <- dtm[which(rowSums(dtm) != 0), ]
   
   return(dtm)
}