#' Term frequency
#'
#' A list of terms and their absolute frequencies in a corpus or a document-term matrix
#'
#' @param x a corpus or a document-term matrix object
#'
#' @return a list of terms and their respective frecuencies
#'
#' @examples
#' 
#'
#' @export
#'
#' @import tm
TermFrequency <-
function( x ) {
	if (sum(class(x)%in%c("VCorpus", "Corpus")) > 0) {
		dtm <- tm::DocumentTermMatrix(x)
		dtm <- tm::weightBin(dtm)
	} else if (sum(class(x) %in% c("DocumentTermMatrix", "simple_triplet_matrix")) > 0) {
		dtm <- tm::weightBin( x )
	}
	dtm <- as.matrix(dtm)

	termCount <- sort(colSums(dtm), decreasing = TRUE)
	termFreq <- data.frame(
			"Term" = names( termCount ),
			"Freq" = termCount
		)
	rownames(termFreq) <- 1:dim(termFreq)[1]

	return(termFreq)
}
