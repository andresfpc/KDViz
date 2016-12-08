#' Sparse rate to remove a proportion of terms
#'
#' Returns the threshold to decide the sparse rate to use depending on the minimum allowed frequency of the terms in the document term matrix
#'
#' @param termFreq a minimum allowed frecuency to define the sparse of the terms
#' @param dtm document-term matrix
#'
#' @return sparse percentage of non empty elements
#'
#' @examples
#' 
#'
#' @export
#'
#' @import tm
SparseRate <-
function( termFreq, dtm ) {
	if (sum(class(dtm)%in%c("VCorpus", "Corpus")) > 0) {
		dtm <- tm::DocumentTermMatrix(dtm)
		dtm <- as.matrix(dtm)
	} else if (sum(class(dtm) %in% c("DocumentTermMatrix", "simple_triplet_matrix")) > 0) {
		dtm <- as.matrix(dtm)
	}
	numDocs <- dim(dtm)[1]
	sparseRate <- ( 1 - ( termFreq/numDocs ) )

	return(sparseRate)
}
