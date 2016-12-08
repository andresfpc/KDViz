#' Text from html
#'
#' Extracts the text attribute from an html node depending on the type and the desired quantity of selectors
#'
#' @param html an html node
#' @param selector the type of html selector ("class" or "id")
#' @param names the possible names of the selector you are looking for (the first not null is selected)
#' @param sep a separator, the symbol to replace the html text spaces between words (" " by default)
#'
#' @return the plain text extracted from the html element
#'
#' @examples
#' 
#'
#' @export
#'
#' @import rvest
TextFromHtml <-
function(html, selector, names, sep = " ") {
	if (!selector %in% c("class", "id")) {
		stop("The selector is not valid")
	}
	if (selector == "class") {
		for(element in names) {
			xpath = paste0("//*[contains(concat(' ', @class, ' '), concat(' ', '", element, "', ' '))]") 	
			htmlText <- rvest::html_text(rvest::html_nodes(html, xpath = xpath))
			if (length(htmlText) != 0) {
				text <- First(paste(htmlText, collapse = sep))
				break
			}
			text <- First(htmlText)
		}
	} else if (selector == "id"){
		for(element in names) {
			xpath = paste0("//*[(@id = '", element, "')]") 	
			htmlText <- rvest::html_text(rvest::html_nodes(html, xpath = xpath))
			if (length(htmlText) != 0) {
				text <- First(paste(htmlText, collapse = ""))
				break
			}
			text <- First(htmlText)
		}
	}

	return(text)
}
