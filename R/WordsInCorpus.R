#' See words inside a corpus
#'
#' A function to return all words found in a corpus
#'
#' @param corpus a corpus object
#'
#' @return a vector of words
#'
#' @examples
#' 
#'
#' @export
WordsInCorpus <-
function(corpus) {
	# 
	#
	# Args:
	#	corp: an object of class Corpus
	# Returns:
	#	words: a list of the words per each document in the corpus
	#
	# Every word of each document is pasted in a character string separated by a space
	words <- as.vector( sapply( corpus, function( x ) paste( x$content[x$content != ""], collapse = " " ) ) )
	# Multiple whitespace characters are collapsed into a single one
	words <- as.vector( sapply( words, function( x ) gsub( "\\s+", " ", x ) ) )
	# A vector of words per document is returned
	words <- strsplit( words, split = " " )
	
	return(words)
}
