% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TextFromHtml.R
\name{TextFromHtml}
\alias{TextFromHtml}
\title{Text from html}
\usage{
TextFromHtml(html, selector, names, sep = " ")
}
\arguments{
\item{html}{an html node}

\item{selector}{the type of html selector ("class" or "id")}

\item{names}{the possible names of the selector you are looking for (the first not null is selected)}

\item{sep}{a separator, the symbol to replace the html text spaces between words (" " by default)}
}
\value{
the plain text extracted from the html element
}
\description{
Extracts the text attribute from an html node depending on the type and the desired quantity of selectors
}
\examples{


}

