# Text Mining with R
# https://www.youtube.com/watch?v=j1V2McKbkLo
# Timothy D'Auria
# How to Build a Text Mining, Machine Learning Document Classification System in R!

# Init
libs <- c("tm", "plyr", "class")
lapply(libs, require, character.only= TRUE)

# Set Options
options(stringsAsFactors = FALSE)

#Set Parameters
candidates <- c("TRUMP","BUSH","CRUZ","SANDERS","CLINTON")
pathname <- "/Users/PHS/Documents/Git_Repos/debates"

# Clean text
cleanCorpus <- function(corpus){
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp,stripWhitespace)
  #corpus.tmp <- tm_map(corpus.tmp,tolower)
  corpus.tmo <- tm_map(corpus.tmp, PlainTextDocument)
  corpus.tmp <- tm_map(corpus.tmp,removeWords, stopwords("english"))
  return(corpus.tmp)
}
# Buildt TDM
gererateTDM <- function(cand,path){
  s.dir <-sprintf("%s/%s",path,cand)
  s.cor <-Corpus(DirSource(directory = s.dir, encoding= "ANSI")) 
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.7)
  result <- list(name = cand, tdm = s.tdm)
}
tdm <- lapply(candidates, generateTDM, path=pathname)
# Attach Name
bindCandidateToTDM <- function(tdm) {
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringAsFactors = FALSE)
  s.df <-cbind (s.df, rep(tdm[["name"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetCandiate"
  return(s.df)
}
candTDM <- lapply(tdm, bindCandidateToTDM)
# Stack
tdm.stack <- do.call(rbind.fill, candTDM)
tdm.stack[is.na(tdm.stack)] <- 0
# Hold-Out
train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * 0.7))
test.idx <- (1:nrow(tdm.stack)) [- train.idx]
# Model - KNN
tdm.cand <- tdm.stack[, "targetCandiate"]
tdm.stack.nl <- tdm.stack [, !colnames(tdm.stack) %in% "targetCandiate"]
knn.pred <-knn(tdm.stack.nl[train.idx, ], tdm.stack.nl[test.idx, ], tdm.cand[train.idx])
# Accuracy
conf.mat <- table("Predictions" =knn.pred, Actual= tdm.cand[test.idx])
(accuracy <- sum(diag(conf.mat)) / length(test.idx)* 100)