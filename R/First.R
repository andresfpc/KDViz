#' First element
#'
#' Returns the first element of an array
#'
#' @param x a vector
#'
#' @return the first element of the incoming object
#'
#' @examples
#' 
#'
#' @export
First <-
function(x) {
	if (length(x) != 0) {
		first <- x[1]
	} else {
		first <- c("")
	}

	return(first)
}
