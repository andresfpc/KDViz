#' Read a RIS file
#'
#' Read the entire info from a RIS file
#'
#' @param risFile a file of RIS extension
#' @param fileName a character giving the name of the resulting file to export
#' @param saveRda a logical value that indicates whether the file should be saved or not in a Rda file
#' @param saveCSV a logical value that indicates whether the file should be saved or not in a csv file
#'
#' @return a data frame of bibliometric units by each article
#'
#' @examples
#' 
#'
#' @export
#'
#' @importFrom utils write.table
ReadRIS <-
function(risFile, fileName, saveRda = FALSE, saveCSV = FALSE) {
	if(missing(fileName)) {
		fileName = "bibData"
	}
	dataBib <- readLines(risFile, encoding = "UTF-8")

	artFrom <- grep("TY  - ", dataBib )
	artTo <- grep("ER  - ", dataBib )

	artData <- list()
	repeat {
		if(length(artData) == length(artFrom)) {
			break
		}
		artData[[length(artData)+1]] <- dataBib[artFrom[length(artData)+1]:artTo[length(artData)+1]]
	}

	artData <- lapply(artData, function(x) x[x != c("")])

	keywords <- gsub("; ", ";", GetRISList(artData, pattern = "KW  - "))
	keywords <- gsub(" ;", ";", keywords)
	keywords <- gsub(" ", "-", keywords)

	bibData <- data.frame(
		"Title" = GetRISList(artData, pattern = "T1  - "),
		"Abstract" = GetRISList(artData, pattern="AB  - "),
		"Keywords" = keywords,
		"Authors" = GetRISList(artData, pattern = "AU  - "),
		"Journal" = GetRISList(artData, pattern = "JO  - "),
		"Volume" = GetRISList(artData, pattern = "VL  - "),
		"Issue" = GetRISList(artData, pattern = "IS  - "),
		"StartPage" = GetRISList(artData, pattern = "SP  - "),
		"EndPage" = GetRISList(artData, pattern = "EP  - "),
		"PubYear" = GetRISList(artData, pattern = "PY  - "),
		"ISBN.ISSN" = GetRISList(artData, pattern = "SN  - "),
		"DOI" = GetRISList(artData, pattern="DO  - "),
		"URL" = GetRISList(artData, pattern="UR  - "),
		stringsAsFactors = FALSE
	)

	bibData[bibData$Abstract == "Abstract", ]$Abstract <- c("")

	row.names(bibData) <- paste0("A", 1:nrow(bibData))

	if(saveRda) {
		save(bibData, file = paste0(fileName, ".Rda"))
	}

	if(saveCSV) {
		write.table( bibData, file = paste0(fileName, ".csv"))
	}
	
	return(bibData)
}
