#' Calculation of co-occurrences matrix and matrix associations from a corpus
#'
#' Similar to the mpa package, it calculates the co-occurrences matrix and the matrix associations from the resulting object of the leer.mpa.corpus function
#'
#' @param corpus a corpus object
#' @param fmin minimal appearance frequency of key words inside the corpus
#' @param cmin minimal co-occurrence between words
#'
#' @return a list that contains the associations and the co-ocurrence matrices, the vector of words and a lexical table (obtained from the original matriz.mpa function0)
#'
#' @examples
#' 
#'
#' @export
#'
#' @import mpa
matriz.mpa.corpus <-
function(corpus, fmin = 3, cmin = 3) {
   corpusMPA <- leer.mpa.corpus(corpus)
   
   mpaWords <- mpa::matriz.mpa(corpusMPA, fmin = fmin, cmin = cmin)
   
   return(mpaWords)
}
