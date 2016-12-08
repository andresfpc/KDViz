#' Summary of word groups
#'
#' Summary of word groups used to visualize knowledge domains
#'
#' @param matriz.mpa vector from the different words that appears in the corpus (returned value by matriz.mpa function)
#' @param mpa a list of values resulting from the mpa function
#'
#' @return a list of objects to summarize the term clustering mpa method
#'
#' @examples
#' 
#'
#' @export
#'
#' @import stringr
KDSummary <-
function(matriz.mpa, mpa) {
   clustTable <- data.frame("Cluster" = mpa$Nombres, "Size" = mpa$Resumen[, 2], "Density" = mpa$Resumen[, 3], "Centrality" = mpa$Resumen[, 4])
   diagram.mpa(mpa)
   
   wordClasses <- data.frame("Word" = matriz.mpa$Palabras, "Class" = mpa$Clases)
   wordClasses$Word <- stringr::str_replace_all(pattern = " ", replacement = "_", wordClasses$Word)
   
   KDSummary <- list("clustTable" = clustTable, "wordClasses" = wordClasses)
   
   return(KDSummary)
}
