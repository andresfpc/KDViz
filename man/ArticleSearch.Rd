% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ArticleSearch.R
\name{ArticleSearch}
\alias{ArticleSearch}
\title{Article search}
\usage{
ArticleSearch(keywords, size, webSite = "ScienceDirect", addInfo = FALSE,
  infoList)
}
\arguments{
\item{keywords}{a vector containing the key terms to search}

\item{size}{the number of articles from which the information is extracted}

\item{webSite}{the journal databases where the information of the articles will be searched}

\item{addInfo}{a logical value indicating whether the info of abstract, keyword, journalTitle, journalVol and authorName should be retrieved by each article}

\item{infoList}{a data frame of titles and URLs to skip the first step of getting the main information of each article}
}
\value{
a data frame containing the information requested in the function call
}
\description{
Search articles by specifying a list of key terms and a journal database
}
\examples{


}

