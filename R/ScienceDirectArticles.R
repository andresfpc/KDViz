#' Article search from ScienceDirect
#'
#' Search articles from the ScienceDirect database by specifying a list of key terms
#'
#' @param keywords a vector containing the key terms to search
#' @param size the number of articles from which the information is extracted
#' @param addInfo a logical value indicating whether the info of abstract, keyword, journalTitle, journalVol and authorName should be retrieved by each article
#' @param infoList a data frame of titles and URLs to skip the first step of getting the main information of each article
#'
#' @return a data frame containing the information requested in the function call
#'
#' @examples
#' 
#'
#' @export
#'
#' @import rvest
#' @import xml2 
ScienceDirectArticles <-
function(keywords, size, addInfo = FALSE, infoList) {
	if (missing(keywords)){
		if (missing(infoList)) {
			stop("Neither keywords nor a list of articles names and links is provided, at least one must be specified")
		} else {
			cat("Getting info from the provided list...\n\n")
			flush.console()
			artTitle = infoList$Title
			artUrl = infoList$URL
		}
	} else {
		if (size > 999) {
			stop("Only a maximum of 999 articles can be obtained from the ScienceDirect webpage")
		}
		keywords <- paste0("{", keywords, "}", collapse = "OR")
		url <- "http://www.sciencedirect.com/"
		originalSession <- rvest::html_session(url)

		cat("Performing keyword search...\n")
		flush.console()
		searchForm <- rvest::html_form(originalSession)[[3]]
		searchValues <- rvest::set_values(searchForm, qs_all = keywords)
		originalQuery <- rvest::submit_form(session = originalSession, form = searchValues, submit = "sdSearch")$url

		currentSession <- rvest::jump_to(originalSession, originalQuery)
		
		resultsFound <- rvest::html_text(rvest::html_node(currentSession, ".queryText"))
		numberOfArt <- as.numeric(gsub(x = resultsFound, pattern = "[A-z]|\\s+|[:punct:]|[.]|[,]", replacement = ""))

		if (numberOfArt < size) {
			size <- numberOfArt
			cat(" Only a total of", numberOfArt, "articles were found\n\n")
			flush.console()
		} else {
			cat(" A total of", numberOfArt, "articles were found\n\n")
			flush.console()
		}

		cat("Looking for the main information of the articles:\n")
		flush.console()

		artNodes <- rvest::html_nodes(currentSession, ".artTitle")

		artTitle <- rvest::html_text(artNodes)
		artUrl <- GetHREF(artNodes)
	
		if (size <= 25) {
			cat(" A total of", size, "articles were retrieved\n\n")
			flush.console()
		} else {
			cat(" ", length(artUrl), "articles retrieved\r")
			flush.console()
			repeat {
				
				artListForm <- rvest::html_form(currentSession)[[4]]
				
				nextList <- rvest::submit_form(currentSession, artListForm, submit = "bottomNext")$url
				currentSession <- rvest::jump_to(currentSession, nextList)
	
				artNodes <- rvest::html_nodes(currentSession, ".artTitle")
		
				artTitle <- c(artTitle, rvest::html_text(artNodes))
				artUrl <- c(artUrl, GetHREF(artNodes))
		
				cat(" ", length(artUrl), "articles retrieved\r")
				flush.console()
				if (length(artUrl) >= size) {
					cat(" A total of", size, "articles were retrieved\n\n")
					flush.console()
					break
				}
			}
		}
	}
	artData <- data.frame("Title" = artTitle, "URL" = artUrl, stringsAsFactors = FALSE)[1:size, ]

	tryCatch({
		if (addInfo) {
			artInfo <- NULL
			repeat{
				if (is.null(nrow(artInfo))) {
					currentSession <- rvest::html_session(artData$URL[1])
					artPage <- xml2::read_html(artData$URL[1])
				} else {
					artPage <- xml2::read_html(artData$URL[nrow(artInfo)+1])
				}
				abstract <- gsub(x = TextFromHtml(artPage, "class", c("abstract")), pattern = "Abstract|Highlights|Graphical abstract", replacement = "")
				keyword <- gsub(x = TextFromHtml(artPage, "class", c("keyword", "svKeywords")), pattern = "Keywords", replacement = "")
				journalTitle <- gsub(x = TextFromHtml(artPage, "class", c("title", "journal-title")), pattern = "[\r\n]|\\s+", replacement = " ")
				journalVol <- TextFromHtml(artPage, "class", c("volIssue", "journal-volume"))
				authorName <- gsub(x = TextFromHtml(artPage, "class", c("author-name-link", "authorName"), sep = ", "), pattern = "[\r\n]|\\s+", replacement = " ")
				#doi <- TextFromHtml(artPage, "id", c("ddDoi"))
				#doiNodes <- html_node(artPage, xpath = '//*[(@id = "S_C_ddDoi")] | //*[(@id = "ddDoi")]')
				#doi <- GetHREF(doiNodes)

				artInfo <- rbind(artInfo, c(First(abstract), First(keyword), First(authorName), First(journalTitle), First(journalVol)))
			
				cat("The entire information of", nrow(artInfo), "articles has been obtained\r")
				flush.console()
				if (nrow(artInfo) == size) {
					cat("The entire information of", size, "articles has been obtained\n")
					flush.console()
					break
				}
			}
		
			artData <- data.frame(cbind(artData$Title, artInfo, artData$URL), stringsAsFactors = FALSE)
			names(artData) <- c("Title", "Abstract", "Keywords", "Authors", "Journal", "Volume", "URL")
		} else {
			cat("The main information of", size, "articles has been obtained\n")
			flush.console()
		}
	}, error = function(e) print("Couldn't finish to get the entire information"))

	rownames(artData) <- paste0("Art", rep(1:nrow(artData)))

	return(artData)
}
