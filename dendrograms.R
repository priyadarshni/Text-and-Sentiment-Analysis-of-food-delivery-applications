require(RCurl)
require(twitteR)
require(wordcloud)
require(tm)
require(syuzhet)

require(SnowballC)
require(ggplot2)
require(rtweet)
require(dplyr)
require(stringr)

text<-readLines(file.choose())
text
docs <- VCorpus(text)
writeLines(as.character(text[1]))
docs <- VCorpus(VectorSource(text))
writeLines(as.character(docs[2]))
docs <- tm_map(docs,removePunctuation) 
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])
}
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs
docs <- tm_map(docs, removeWords, stopwords("english"))  
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, removeWords, c("ubereats", "http","can")) 

docs_st <- tm_map(docs, stemDocument)  
docs_st <- tm_map(docs_st, PlainTextDocument)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
dtm <- DocumentTermMatrix(docs)   
tdm <- TermDocumentMatrix(docs) 
freq <- colSums(as.matrix(dtm)) 
length(freq)
ord <- order(freq) 
m <- as.matrix(dtm)  
dim(m)  


write.csv(m, file="DocumentTermMatrixDoor.csv")  
dtms <- removeSparseTerms(dtm, 0.2)
dtms
freq <- colSums(as.matrix(dtm))

head(table(freq), 20)
tail(table(freq), 20)
freq <- colSums(as.matrix(dtms)) 

freq


freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE) 
head(freq, 14) 

findFreqTerms(dtm, lowfreq=10)
wf <- data.frame(word=names(freq), freq=freq)
wf
head(wf)
library(ggplot2)   
p <- ggplot(subset(wf, freq>10), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

p
findAssocs(dtm, c("free" , "deliv"), corlimit=0.25)
findAssocs(dtms, "dasher", corlimit=0.70)
set.seed(142)   
wordcloud(names(freq), freq, min.freq=1500)
set.seed(142)   
wordcloud(names(freq), freq, max.words=100)
set.seed(142)  
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2) 
wordcloud(names(freq), freq, min.freq=500, scale=c(5, .1), colors=brewer.pal(8, "Dark2"))
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)  

dtmss <- removeSparseTerms(dtm, 0.15)
dtmss
library(cluster)
d <- dist(t(dtmss), method="euclidian") 
fit <- hclust(d=d, method="complete")
fit 
plot(fit, hang=-1)   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=4) 
rect.hclust(fit, k=4, border="blue") 
library(fpc)  
install.packages("fpc")
d <- dist(t(dtmss), method="euclidian") 
kfit <- kmeans(d, 2)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  
dtmss
