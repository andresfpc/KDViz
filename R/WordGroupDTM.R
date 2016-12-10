#' Document-term matrix and SCA by word group
#'
#' A portion of an entire document-term matrix depending on the class found using the kdSummary function
#'
#' @param class the number of the class to be partitioned
#' @param dtm a document-term matrix
#' @param wordClasses the value resulting from performing a KDSummary
#' @param graph a logical value. If TRUE, the knowledge domain map of the corresponding class is plotted
#'
#' @return the doc-term matrix and the SCA object from the document and word group
#'
#' @examples
#' 
#'
#' @export
WordGroupDTM <-
function(class, dtm, wordClasses, graph = FALSE) {
	wordGroup <- wordClasses[which(wordClasses[, 2] == class), 1]
	dtmGroup <- dtm[, colnames(dtm) %in% wordGroup]
	dtmGroup <- dtmGroup[which(rowSums(dtmGroup) != 0), ]

	if(missing(graph)) {
		graph = FALSE
	}

	if(graph) {
		coaGroup <- KDViz(dtmGroup, graph = TRUE)
	} else {
		coaGroup <- KDViz(dtmGroup)
	}

	WordGroupDTM <- list("dtmGroup" = dtmGroup, "coaGroup" = coaGroup)

	return(WordGroupDTM)
}
