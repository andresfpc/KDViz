#' Last element
#'
#' Returns the last element of an array
#'
#' @param x a vector
#'
#' @return the last element of the incoming object
#'
#' @examples
#' 
#'
#' @export
Last <-
function(x) {
	if (length(x) != 0) {
		last <- x[length(x)]
	} else {
		last <- c("")
	}
	
	return(last)
}
