rm(list = ls())
library("KDVizUN")

risFile <- system.file("ScienceDirectRIS.ris", package = "KDVizUN") # Original data

myData <- ReadRIS(risFile, "bibDataRIS", saveRda = TRUE, saveCSV = FALSE) # RIS file to data object

bibData <- system.file("bibData.Rda", package = "KDVizUN")
load(bibData)

#load("bibData.Rda") # Load data file 
#dim(bibData)

corpus <- CorpusFromBibData(bibData = bibData, bibUnits = c("Keywords"), controlList = "", stopwords = "", wordsToRemove = "") # Create a corpus from the bib data

dtm <- DTMFromCorpus(corpus, row.names(bibData)) # Create a doc-term matrix from the corpus
dim(dtm)

bibUnits <- c("Keywords") # Selection of bibliometric units to analyze
controlList <- c("stripWhitespace", "removeNumbers") # List of tm process to perform
stopwords <- FALSE # Decide which stopwords are going to be used (file or FALSE if they are not required)
#stopwords <- system.file("stopwords_en.txt", package = "KDVizUN") # Optional
wordsToRemove <- c("nanotechnology") # List of custom words to remove

replaceWords <- system.file("keywordReplace.txt", package = "KDVizUN") # Custom dictionary to replace some selected words

corpus <- CorpusFromBibData(bibData = bibData, bibUnits = bibUnits, controlList = controlList, stopwords = stopwords, wordsToRemove = wordsToRemove, replaceWords = replaceWords) # Corpus from bibdata with and a control list to perform the entire tm process

termFreqTable <- TermFrequency(corpus) # See the frequency of terms in the corpus
head(termFreqTable, 98)

TermFreqByWord(termFreqTable = termFreqTable, word = "reduction") # Search for words containing the term in 'word' parameter

# corpus <- ReplaceByList(corpus = corpus, wordsFile = replaceWords) # An optional function (contained yet in the previous process) to replace other words after getting a corpus

termFreqTable <- TermFrequency(corpus) # See the frequency of terms in the current corpus
head(termFreqTable, 100)

dtm <- DTMFromCorpus(corpus, row.names(bibData)) # Create a doc-term matrix from the corpus
dim(dtm)
rownames(dtm)

termFreq <- TermFrequency(dtm) # See the frequency of terms in the doc-term matrix
head(termFreq, 100)

mpaWords <- matriz.mpa.corpus(corpus, fmin = 5, cmin = 1) # mpa matrices from a corpus object
mpaWords$Palabras

classes <- mpa::mpa(mpaWords$Matriza, 10, mpaWords$Palabras) # mpa method from the calculated objects in 'mpaWords'
classes

kdSummary <- KDSummary(matriz.mpa = mpaWords, mpa = classes) # a quick summary of the mpa process

mpa::plotmpa(3, mpaWords$Matriza, classes) # Plot the network of selected class
	 	 	
WordGroupDTM(dtm, wordClasses = kdSummary$wordClasses, class = 7, graph = TRUE) # Extract a partition of the original 'dtm' matrix depending on the class that you want

group1 <- WordGroupDTM(dtm, wordClasses = kdSummary$wordClasses, class = 1, graph = TRUE)
group2 <- WordGroupDTM(dtm, wordClasses = kdSummary$wordClasses, class = 2, graph = TRUE)
group3 <- WordGroupDTM(dtm, wordClasses = kdSummary$wordClasses, class = 3, graph = TRUE)
group4 <- WordGroupDTM(dtm, wordClasses = kdSummary$wordClasses, class = 4, graph = TRUE)
group5 <- WordGroupDTM(dtm, wordClasses = kdSummary$wordClasses, class = 5, graph = TRUE)
group6 <- WordGroupDTM(dtm, wordClasses = kdSummary$wordClasses, class = 6, graph = TRUE)
group7 <- WordGroupDTM(dtm, wordClasses = kdSummary$wordClasses, class = 7, graph = TRUE)
group8 <- WordGroupDTM(dtm, wordClasses = kdSummary$wordClasses, class = 8, graph = TRUE)
group9 <- WordGroupDTM(dtm, wordClasses = kdSummary$wordClasses, class = 9, graph = TRUE)
group10 <- WordGroupDTM(dtm, wordClasses = kdSummary$wordClasses, class = 10, graph = TRUE)
plot(group1$coaGroup, ucal = 0, cex.col = 0.8, cex.row = 0.5)

LoadArticle(bibData, "A625") # Load the info of an article (it will open the URL by default)
