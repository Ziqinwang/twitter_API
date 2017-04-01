library(twitteR)
library(Rfacebook)
library(ROAuth)
#library(base64enc)
library(httr)
library(httpuv)
library(curl)
library(installr)
library(devtools)
#library(get_oauth_sig)

#library(RTextTools)
#library(topicmodels)
#library(RCurl) 


PokermanGO <- searchTwitter("Pokerman Go", lang="en", n=10000);

popeinphilly <- searchTwitter("popeinphilly", lang="en", n=10000);
yolo <- searchTwitter("yolo", lang="en", n=10000);
septa <- searchTwitter("septa", lang="en", n=10000)
recount <-searchTwitter("recount", lang="en", n=10000)


install.packages("tm")
install.packages("RTextTools")
install.packages("SnowballC")

library(tm)
library(RTextTools)
library(SnowballC)

#    Saving twitter feed as a data frame
df <- do.call("rbind", lapply(PokermanGO, as.data.frame));

df <- do.call("rbind", lapply(popeinphilly, as.data.frame));
df <- do.call("rbind", lapply(septa, as.data.frame));
df <- do.call("rbind", lapply(yolo, as.data.frame));

#    Corpora are collections of documents containing (natural language) text. 
#    Used in packages which employ the infrastructure provided by package tm. 
myCorpus <- Corpus(VectorSource(df$text));
myCorpus

#    Defining toSpace function
#    Removing special characters
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
myCorpus <- tm_map(myCorpus, toSpace, "/|@|\\|")
myCorpus <- tm_map(myCorpus, toSpace, "\\[")
myCorpus <- tm_map(myCorpus, toSpace, "]")

#    Removing numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

#    Removing punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

#    Removing English stop words
stopwords("english")
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))

#    Removing our own stop words
myCorpus <- tm_map(myCorpus, removeWords,c("http", "qhb...", "https", "http...", "for"))

#as.data.frame(myCorpus) %>%
#  with(., invisible(sapply(text, function(x) {strWrap(x); cat("\n\n")})))



#Removing whitespace characters
#When rendered, a whitespace character does not correspond to a visible
#mark, but typically does occupy an area on a page
myCorpus <- tm_map(myCorpus, stripWhitespace)

#Stemming (i.e., removing common word endings like "es", "ed", etc.)
myCorpus <- tm_map(myCorpus, stemDocument)


#Creating a Document Term Matrix,#LOOK AT THE FREQUENCY OF THE REMAINING WORDS
dtm <- DocumentTermMatrix(myCorpus)
dtm
dim(dtm)


freq <- sort(colSums(as.matrix(dtm)), decreasing=FALSE)


#install.packages("qdap")
#install.packages("qdapDictionaries")
#install.packages("dplyr")
#install.packages("RColorBrewer")
#install.packages(RColorBrewer)

install.packages("wordcloud")
library(wordcloud)

wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))






myCorpus <- Corpus(VectorSource(df$text));
myCorpus





#    Remove the columns replyToSID, replyToUID and replyToSN from data frame df
df$replyToSID <- NULL
df$replyToUID <- NULL
df$replyToSN <- NULL
head(df)
dim(df)

sum(is.na(df$longitude))

#    Let's look at the number of observations which have missing variables
#    There are 138 variables left which have missing observations for 
#    latitude and longitude.
df.geocoded=na.omit(df)
dim(df.geocoded)


#mean(df.geocoded$latitude)
#mean(df.geocoded$longitude)

install.packages("ggmap")
install.packages("ggplot2")
install.packages("rworldmap")
install.packages("mapproj")
library(ggmap)
library(ggplot2)
library(rworldmap)
library(mapproj)

setwd("D:/study/musa 500/Lecture 23")
write.csv(df.geocoded, file = "df_geocoded.csv")
geocoded.points <- read.csv("df_geocoded.csv")


# Get the map; you might have to try several zoomlevels te get the right one
library(ggmap)
map <- qmap('United States',zoom = 4)
mapImageData <- get_googlemap(center = c(lon = mean(geocoded.points$longitude), 
	lat = mean(geocoded.points$latitude)), zoom=2)

map <- qmap('London',zoom = 8)
# Plot the points on the map
map +
  geom_point(data=geocoded.points, aes(x=longitude, y=latitude), colour="red", size=6, alpha=.6)




#    Corpora are collections of documents containing (natural language) text. 
#    Used in packages which employ the infrastructure provided by package tm. 
myCorpusg <- Corpus(VectorSource(df.geocoded$text));
myCorpusg

#    Defining toSpace function
#    Removing special characters
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
myCorpusg <- tm_map(myCorpusg, toSpace, "/|@|\\|")
myCorpusg <- tm_map(myCorpusg, toSpace, "\\[")
myCorpusg <- tm_map(myCorpusg, toSpace, "]")

#    Removing numbers
myCorpusg <- tm_map(myCorpusg, removeNumbers)

#    Removing punctuation
myCorpusg <- tm_map(myCorpusg, removePunctuation)

#    Removing English stop words
stopwords("english")
myCorpusg <- tm_map(myCorpusg, removeWords, stopwords("english"))

#    Removing our own stop words
myCorpusg <- tm_map(myCorpusg, removeWords,c("http", "qhb...", "https", "http...", "for"))

#as.data.frame(myCorpusg) %>%
#  with(., invisible(sapply(text, function(x) {strWrap(x); cat("\n\n")})))



#Removing whitespace characters
#When rendered, a whitespace character does not correspond to a visible
#mark, but typically does occupy an area on a page
myCorpusg <- tm_map(myCorpusg, stripWhitespace)

#Stemming (i.e., removing common word endings like "es", "ed", etc.)
myCorpusg <- tm_map(myCorpusg, stemDocument)


#Creating a Document Term Matrix
dtm <- DocumentTermMatrix(myCorpusg)
dtm
dim(dtm)


freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)


#install.packages("qdap")
#install.packages("qdapDictionaries")
#install.packages("dplyr")
#install.packages("RColorBrewer")
#install.packages(RColorBrewer)

install.packages("wordcloud")
library(wordcloud)
#YOU CAN CHANGE THE FREQUENCE IF YOU WANT
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))



