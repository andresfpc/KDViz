#' Load the info of an article
#'
#' Load the info of an article and if wanted, shows the webpage of it
#'
#' @param articleData a data frame containing the info (Title, Abstract, Keywords, URL, ...) of an article
#' @param articleName the name of an article (rowname from the articleData)
#' @param browser a logical value. If TRUE, the article URL is opened in a browser window
#'
#' @return the info from the article and the website where it is available
#'
#' @examples
#' 
#'
#' @export
#'
#' @importFrom utils browseURL
LoadArticle <-
function(articleData, articleName, browser = TRUE) {
	cat("Showing article ", articleName, ":\n\n", sep = "")
	index <- which(row.names(articleData) %in% articleName)
	print(as.matrix(articleData[index, ]))

	if (browser) {
		browseURL(url = as.character(articleData$URL[index]))
	}
}
