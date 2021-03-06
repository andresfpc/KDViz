% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LoadArticle.R
\name{LoadArticle}
\alias{LoadArticle}
\title{Load the info of an article}
\usage{
LoadArticle(articleData, articleName, browser = TRUE)
}
\arguments{
\item{articleData}{a data frame containing the info (Title, Abstract, Keywords, URL, ...) of an article}

\item{articleName}{the name of an article (rowname from the articleData)}

\item{browser}{a logical value. If TRUE, the article URL is opened in a browser window}
}
\value{
the info from the article and the website where it is available
}
\description{
Load the info of an article and if wanted, shows the webpage of it
}
\examples{


}

