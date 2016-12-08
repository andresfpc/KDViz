#' Article search
#'
#' Search articles by specifying a list of key terms and a journal database
#'
#' @param keywords a vector containing the key terms to search
#' @param size the number of articles from which the information is extracted
#' @param webSite the journal databases where the information of the articles will be searched
#' @param addInfo a logical value indicating whether the info of abstract, keyword, journalTitle, journalVol and authorName should be retrieved by each article
#' @param infoList a data frame of titles and URLs to skip the first step of getting the main information of each article
#'
#' @return a data frame containing the information requested in the function call
#'
#' @examples
#' 
#'
#' @export
ArticleSearch <-
function(keywords, size, webSite = "ScienceDirect", addInfo = FALSE, infoList) {
	if (missing(keywords)){
		if (missing(infoList)) {
			stop("Neither keywords nor a list of articles names and links is provided, at least one must be specified")
		} else {
			if (size > nrow(infoList) ) {
				stop("The provided 'size' is greater than the number of articles in the 'infoList' parameter")
			} else {
				infoList <- infoList[1:size, ]
			}
			artData <- ScienceDirectArticles(keywords, size, addInfo, infoList)
		}
	} else {
		if (size < 1) {
			stop("The number of articles to retrieve must be greater than 0")
		}
		if (length(keywords) == 0) {
			stop("Keyword vector can not be empty")
		}
		webList <- c("ScienceDirect")
		if (!webSite %in% webList) {
			stop("Only ScienceDirect is available, soon there will be more...")
		}
		artData <- ScienceDirectArticles(keywords, size, addInfo, infoList)
	}

	return(artData)
}
