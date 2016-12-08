#' Get text from a RIS element
#'
#' Get the text line from a RIS element (used internally for the GetRISList function)
#'
#' @param x a vector
#' @param pattern the pattern to match the line contents
#' @param replacement the text for replace the matched pattern (empty by default)
#' @param collapse the symbol for collapse the resulting array of contents in the text line
#'
#' @return the text line of a RIS element
#'
#' @examples
#' 
#'
#' @export
GetRISElement <-
function(x, pattern, replacement = "", collapse = ";") {
	text <- gsub(x[grep(pattern, x)], pattern = pattern, replacement = replacement)
	if(length(text) > 1) {
		text <- paste0(text, collapse = collapse)
	}
	if(length(text) == 0) {
		text <- c("")
	}

	return(text)
}
