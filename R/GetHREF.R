#' Get HREF attribute
#'
#' Get the href attribute from a html object
#'
#' @param nodeSet a html node or node set
#'
#' @return the href attribute from the node (or the nodes)
#'
#' @examples
#' 
#'
#' @export
#'
#' @import rvest
GetHREF <-
function(nodeSet) {
	attrList <- rvest::html_attrs(nodeSet)
	href <- sapply(attrList, function(x) as.character(x["href"]))
	
	return(href)
}
