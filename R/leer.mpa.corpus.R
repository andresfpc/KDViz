#' Reads a corpus and passes it to mpa format
#'
#' Returns the content of a corpus object to use mpa package methods
#'
#' @param corpus a corpus object
#'
#' @return a vector containing the term list per document
#'
#' @examples
#' 
#'
#' @export
#'
#' @import stringr
leer.mpa.corpus <-
function(corpus) {
	corpusMPA <- sapply(corpus, function(x) x$content)
	corpusMPA <- corpusMPA[corpusMPA != ""]

	corpusMPA <- stringr::str_replace_all(pattern="\\s+", replacement=" ", corpusMPA)
	corpusMPA <- stringr::str_replace_all(pattern=" ", replacement="/", corpusMPA)
	corpusMPA <- stringr::str_replace_all(pattern="_", replacement=" ", corpusMPA)
	
	corpusMPA <- paste0(c("/ind0/"), corpusMPA, "/")

	return(corpusMPA)
}
