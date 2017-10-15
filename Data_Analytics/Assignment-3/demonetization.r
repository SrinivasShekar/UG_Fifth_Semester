library(ggplot2)
library(tm) 
library(SnowballC) 
library(wordcloud) 
library(RColorBrewer)


#Loading data
demo<-read.csv('demonetization.csv',sep=',')

democorpus <- Corpus(VectorSource(demo$text))

#Cleaning the data
democorpus <- tm_map(democorpus, content_transformer(tolower))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

democorpus <- tm_map(democorpus, content_transformer(removeURL))


removeNumPunc <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
democorpus <- tm_map(democorpus, content_transformer(removeNumPunc))

democorpus <- tm_map(democorpus, removePunctuation)

democorpus <- tm_map(democorpus, removeNumbers)

myStopwords <- c(stopwords('english'), "available", "via","rt")

myStopwords <- setdiff(myStopwords, c("r", "big"))

democorpus <- tm_map(democorpus, removeWords, myStopwords)

democorpus <- tm_map(democorpus, stripWhitespace)



democorpus <- tm_map(democorpus, stemDocument)

inspect(democorpus[1:5])

for (i in c(1:2, 300)) 
{
  cat(paste0("[", i, "] "))
  writeLines(strwrap(as.character(democorpus[[i]]), 60))
}




democorpus <- Corpus(VectorSource(democorpus))
tdm <- TermDocumentMatrix(democorpus,
                          control = list(wordLengths = c(1, Inf)))


m <- as.matrix(tdm)

word.freq <- sort(rowSums(m), decreasing = T)

pal2 <- brewer.pal(10,"Dark2")


#Plotting the word cloud
library(wordcloud)
set.seed(1234)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, 
          random.order = FALSE,rot.per=.1, colors = pal2, scale=c(10,0.5))




