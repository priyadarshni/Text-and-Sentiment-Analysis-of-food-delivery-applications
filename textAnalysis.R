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
appName<-"TextSentAnalysis"
ckey <-""
csecret <-""
atoken <-""
asecret <-""
twitter_token <-create_token(app= appName,consumer_key = ckey,consumer_secret = csecret,access_token = atoken,access_secret = asecret)
grub.tweets <- search_tweets(q=
                              "Grubhub",n=4000, lang="en", type= "recent", token = twitter_token)
grub.tweets$text<-sapply(grub.tweets$text,function(row)iconv(row, "latin1", "ASCII", sub = ""))
grub.tweets$text

grub.cleantweets<-Corpus(VectorSource(grub.tweets$text))

grub.cleantweets<-tm_map(grub.cleantweets, content_transformer(tolower))
grub.cleantweets<-tm_map(grub.cleantweets, removeWords,stopwords(kind="english"))
grub.cleantweets <- tm_map(grub.cleantweets, removeWords, c("https", "can","grubhub"))

grub.cleantweets<-tm_map(grub.cleantweets, content_transformer(removePunctuation))
grub.cleantweets <- tm_map(grub.cleantweets, stripWhitespace)
grub.cleantweets <- tm_map(grub.cleantweets, removeNumbers)
grub.cleantweets <- tm_map(grub.cleantweets, stemDocument)
RemoveURL <- function(x) gsub('http[[alnum:]]*','',x)
grub.cleantweets <- tm_map(grub.cleantweets, content_transformer(RemoveURL))

grub.tweets <- cbind(grub.tweets, data.frame(corrected.text=sapply(grub.cleantweets, identity), stringsAsFactors=FALSE))

grub.tweetsSent <- get_nrc_sentiment(grub.tweets$corrected.text) 
grub.tweetsSent <- cbind(grub.tweets[,c("created_at", "retweet_count", "favorite_count", "text")],grub.tweetsSent)
grub.tweetsSent
vec<-as.vector(grub.tweetsSent)
vec

vec1<-as.vector(ubereats.tweetsSent)
vec1

vec2<-as.vector(door.tweetsSent)
vec2

################### calculate separate values###########################################
docs_vector1 <- as.character(grub.cleantweets)
emotion_grub_data <- get_nrc_sentiment(docs_vector1)
emotion_grub_data
dataforpiegrub<-emotion_grub_data[1,]
dataforpiegrub

####
docs_vector2 <- as.character(door.cleantweets)
emotion_door_data <- get_nrc_sentiment(docs_vector2)
emotion_door_data
dataforPiedoor<-emotion_door_data[1,]

############
docs_vector3 <- as.character(ubereats.cleantweets)
emotion_uber_data <- get_nrc_sentiment(docs_vector3)
emotion_uber_data
dataforPieubereats<-emotion_uber_data[1,]

############################Pie Chart###################################################

# Get the library.
library(plotrix)

# Create data for the graph.
x <-  c(dataforpiegrub$trust,dataforPiedoor$trust,dataforPieubereats$trust)
lbl <-  c("grubhub trust","doordash trust","ubereats trust")
piepercent<- round(100*x/sum(x), 1)

# Give the chart file a name.
png(file = "3d_pie_chart.jpg")

# Plot the chart.
pie3D(x,labels = lbl,explode = 0.1, main = "Pie Chart of trust factor ")

# Save the file.
dev.off()

###############2nd pie chart for anger ######################################
# Create data for the graph.
x <-  c(dataforpiegrub$anger,dataforPiedoor$anger,dataforPieubereats$anger)
labels <-  c("grubhub_anger","doordash_anger","ubereats_anger")

piepercent<- round(100*x/sum(x), 1)

# Give the chart file a name.
png(file = "city_percentage_legends.jpg")

# Plot the chart.
pie(x, labels = piepercent, main = "Anger for food delivery apps",col = rainbow(length(x)))
legend(x="topright",y=0.92, c("grubhub_anger","doordash_anger","ubereats_anger"), cex = 0.8,
       fill = rainbow(length(x)))

# Save the file.
dev.off()



################not working###############################################

?pie3D





p <- plot_ly() %>%
  add_pie(data = count(ubereats.tweetsSent), labels = ~vec, values = ~n,
          name = "Ubereats", domain = list(x = c("positive", "negative"), y = c(248, 257))) %>%
  add_pie(data = count(door.tweetsSent), labels = ~vec1, values = ~n,
          name = "DoorDash", domain = list(x = c("postive","negative"), y = c(219, 249))) %>%
  add_pie(data = count(grub.tweetsSent), labels = ~vec2, values = ~n,
          name = "Grubhub", domain = list(x = c("positive", "negative"), y = c(242, 246))) %>%
  layout(title = "Pie Charts with Subplots", showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p
#################################################################




################# radar chart of emotions##################
library(plotly)

p <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = c(119, 106, 71, 120, 83, 96,67,148,246,242),
    theta = c("anger","anticipation","disgust", "fear", "joy", "sadness","surprise","trust","negative","positive"),
    name = 'Grubhub'
  ) %>%
  add_trace(
    r = c(102,102,81, 119, 83, 108,73,136,249,219),
    theta = c("anger","anticipation","disgust", "fear", "joy", "sadness","surprise","trust","negative","positive"),
    name = 'DoorDash'
  ) %>%
 add_trace(
    r = c(109, 118, 73, 123, 91, 104,71,139,257,248),
    theta = c("anger","anticipation","disgust", "fear", "joy", "sadness","surprise","trust","negative","positive"),
   name = 'Ubereats'
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,300)
      )
    )
  )




p
#########################################################################





?add_pie
door.tweets <- search_tweets(q=
                               "DoorDash",n=4000, lang="en", type= "recent", token = twitter_token)
door.tweets$text<-sapply(door.tweets$text,function(row)iconv(row, "latin1", "ASCII", sub = ""))
door.tweets$text


door.cleantweets<-Corpus(VectorSource(door.tweets$text))

door.cleantweets<-tm_map(door.cleantweets, content_transformer(tolower))
door.cleantweets<-tm_map(door.cleantweets, removeWords,stopwords(kind="english"))
docs <- tm_map(docs, removeWords, c("https", "can","doordash","door","dash"))

door.cleantweets<-tm_map(door.cleantweets, content_transformer(removePunctuation))
door.cleantweets <- tm_map(door.cleantweets, stripWhitespace)
door.cleantweets <- tm_map(door.cleantweets, removeNumbers)
door.cleantweets <- tm_map(door.cleantweets, stemDocument)
RemoveURL <- function(x) gsub('http[[alnum:]]*','',x)
door.cleantweets <- tm_map(door.cleantweets, content_transformer(RemoveURL))

door.tweets <- cbind(door.tweets, data.frame(corrected.text=sapply(door.cleantweets, identity), stringsAsFactors=FALSE))

door.tweetsSent <- get_nrc_sentiment(door.tweets$corrected.text) 
door.tweetsSent <- cbind(door.tweets[,c("created_at", "retweet_count", "favorite_count", "text")],door.tweetsSent)
door.tweetsSent





ubereats.tweets <- search_tweets(q=
                               "Uber Eats",n=4000, lang="en", type= "recent", token = twitter_token)
ubereats.tweets$text<-sapply(ubereats.tweets$text,function(row)iconv(row, "latin1", "ASCII", sub = ""))
ubereats.tweets$text

ubereats.cleantweets<-Corpus(VectorSource(ubereats.tweets$text))

ubereats.cleantweets<-tm_map(ubereats.cleantweets, content_transformer(tolower))
ubereats.cleantweets<-tm_map(ubereats.cleantweets, removeWords,stopwords(kind="english"))
ubereats.cleantweets <- tm_map(ubereats.cleantweets, removeWords, c("https", "can","ubereats"))

ubereats.cleantweets<-tm_map(ubereats.cleantweets, content_transformer(removePunctuation))
ubereats.cleantweets <- tm_map(ubereats.cleantweets, stripWhitespace)
ubereats.cleantweets <- tm_map(ubereats.cleantweets, removeNumbers)
ubereats.cleantweets <- tm_map(ubereats.cleantweets, stemDocument)
RemoveURL <- function(x) gsub('http[[alnum:]]*','',x)
ubereats.cleantweets <- tm_map(ubereats.cleantweets, content_transformer(RemoveURL))

ubereats.tweets <- cbind(ubereats.tweets, data.frame(corrected.text=sapply(ubereats.cleantweets, identity), stringsAsFactors=FALSE))

ubereats.tweetsSent <- get_nrc_sentiment(ubereats.tweets$corrected.text) 
ubereats.tweetsSent <- cbind(ubereats.tweets[,c("created_at", "retweet_count", "favorite_count", "text")],ubereats.tweetsSent)
ubereats.tweetsSent



s <- reshape2::melt(ubereats.tweetsSent,
                    variable.name = "emotion",
                    value.name = "sentiment",
                    id.vars = c("created_at", "favorite_count", "retweet_count","text"))
u <- reshape2::melt(grub.tweetsSent,
                    variable.name = "emotion",
                    value.name = "sentiment",
                    id.vars = c("created_at", "favorite_count", "retweet_count","text"))
u <- mutate(u, foodDelivery = "Ubereats")
s <- mutate(s, foodDelivery = "Grubhub")
both.foodDelivery <- rbind(u, s)
both.foodDelivery.aggr <- both.foodDelivery %>%
  group_by(foodDelivery, emotion) %>% 
  summarise(sentiment = sum(sentiment))
g <- ggplot(data = both.foodDelivery.aggr, mapping = aes(x = emotion, y = sentiment))
g <- g + geom_bar(aes(fill = foodDelivery),stat = "identity", position = "dodge")
g <- g + theme_minimal() + theme(legend.position = "bottom") + labs(title = "NRC Sentiment Comparison", y = "Total Sentiment", x = "Emotion")
g + scale_fill_discrete(name  = "foodDelivery",
                        breaks=c("Ubereats", "Grubhub"),
                        labels=c("Ubereats", "Grubhub")
                        
############################bar chart final###############################                        
                        
s <- reshape2::melt(ubereats.tweetsSent,
                                            variable.name = "emotion",
                                            value.name = "sentiment",
                                            id.vars = c("created_at", "favorite_count", "retweet_count","text"))
u <- reshape2::melt(grub.tweetsSent,
                                            variable.name = "emotion",
                                            value.name = "sentiment",
                                            id.vars = c("created_at", "favorite_count", "retweet_count","text"))
j<- reshape2::melt(door.tweetsSent,
                                           variable.name = "emotion",
                                           value.name = "sentiment",
                                           id.vars = c("created_at", "favorite_count", "retweet_count","text"))
u <- mutate(u, foodDelivery = "Ubereats")
s <- mutate(s, foodDelivery = "Grubhub")
j <- mutate(j, foodDelivery = "DoorDash")
all.foodDelivery <- rbind(u, s,j)
all.foodDelivery.aggr <- all.foodDelivery %>%
                group_by(foodDelivery, emotion) %>% 
              summarise(sentiment = sum(sentiment))
g <- ggplot(data = all.foodDelivery.aggr, mapping = aes(x = emotion, y = sentiment))
g <- g + geom_bar(aes(fill = foodDelivery),stat = "identity", position = "dodge")
g <- g + theme_minimal() + theme(legend.position = "bottom") + labs(title = "NRC Sentiment Comparison for diiferent food delivery apps", y = "Total Sentiment", x = "Emotion")
        g + scale_fill_discrete(name  = "foodDelivery",
        breaks=c("Ubereats", "Grubhub","DoorDash"),
        labels=c("Ubereats", "Grubhub","DoorDash")
g
        
#############################dendograms try###############################################
df <- read.csv("C:/Users/Priya/Documents/R/grubText1.txt", head=FALSE, sep=",")
d <- dist(as.matrix(df))
hc <- hclust(d)
plot(hc)
q()
