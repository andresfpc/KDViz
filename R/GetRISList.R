#' Get text from a RIS bibliometric unit
#'
#' Get the text lines from a RIS bibliometric unit
#'
#' @param data a list containing the RIS info of each article
#' @param pattern the pattern to match the line contents
#' @param replacement the text for replace the matched pattern (empty by default)
#' @param collapse the symbol for collapse the resulting array of contents in the text line
#'
#' @return the text lines of a RIS bibliometric unit
#'
#' @examples
#' 
#'
#' @export
GetRISList <-
function(data, pattern, replacement = "", collapse = ";") {
	risList <- sapply(data, function(x) GetRISElement(x, pattern = pattern, replacement = replacement, collapse = collapse))

	return(risList)
}
