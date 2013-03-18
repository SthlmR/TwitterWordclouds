# Presentation - Producing a Wordcloud from Twitter with R

# Read libraries
Sys.setenv(NOAWT=TRUE) 
library(twitteR)
library(tm)
library(ROAuth)
library(RColorBrewer)
library(wordcloud)

# Set wd
setwd("~/Desktop/TwiPres")

# Step: 1 Create an Twitter application (need a Twitter account)
# https://twitter.com/apps/new

# Open Authorization (using ROauth)
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
consumerKey <- "MyKey"
consumerSecret <- "MySecret"

# Create a Twitter OAuth
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake()
registerTwitterOAuth(twitCred)

# Searchterm
HashTag<-c("#svpol")

# Get data from Twitter
# load(file="TwiData.Rdata")
# load(file="TwiExample.Rdata")
TwitterDataRaw<-searchTwitter(searchString=HashTag,n=100)

MyTweets <- userTimeline('MansMeg',n=50)

MySelf<-getUser('MansMeg')
MyDF<-MySelf$toDataFrame()

MyFollowers<-MySelf$getFollowers()
MyFollowers

MyFriends<-MySelf$getFriends()
MyFriends


# Turn the Tweets into a DataFrame
TwitterDF<-twListToDF(TwitterDataRaw)
#TwitterDF<-twListToDF(twitter.data.raw)

if(!exists("TwitterData")){
  TwitterData<-TwitterDF[!duplicated(TwitterDF),]
}else{
  TwitterData<-rbind(TwitterDF,TwitterData)
  TwitterData<-TwitterData[!duplicated(TwitterData),]
}

# save(TwitterData,file="TwiData.Rdata")

# Turn the text into a corpus (using the tm package)
TwiText<-paste(TwitterData$text,collapse=" ")
TwiCorp<-Corpus(VectorSource(TwiText))

inspect(TwiCorp)

# Do some cleaning befor producing a wordcloud
TwiCorp<-tm_map(x=TwiCorp,tolower)
TwiCorp<-tm_map(x=TwiCorp,gsub,pattern="https?://t.co/[0-9A-Öa-ö]+", replacement=" ")
TwiCorp<-tm_map(x=TwiCorp,gsub,pattern="#svpol", replacement=" ")
TwiCorp<-tm_map(x=TwiCorp,gsub,pattern="[^A-ZÅÄÖa-zåäö0-9@#_]", replacement=" ")
TwiCorp<-tm_map(x=TwiCorp,removeWords,stopwords(kind="swedish"))
TwiCorp<-tm_map(x=TwiCorp,stripWhitespace)

inspect(TwiCorp)

TwiCorpStem<-tm_map(x=TwiCorp,stemDocument,language="swedish")

inspect(TwiCorpStem)

# Producing a TermDocument matrix
DtmTwitter <- TermDocumentMatrix(TwiCorp)
#DtmTwitter <- TermDocumentMatrix(TwiCorpStem)

DtmTwitterMatrix<-as.matrix(DtmTwitter)

# Number of words to include in Wordcloud
FreqLimit<-25

DtmTwitterByWho<-as.matrix(table(TwitterData$screenName))
FreqLimitByWho<-order(DtmTwitterByWho,decreasing=TRUE)<=FreqLimit
DtmTwitterByWho<-DtmTwitterByWho[FreqLimitByWho,,drop=FALSE]

DtmTwitter2Who<-DtmTwitterMatrix[substr(rownames(DtmTwitterMatrix),1,1)=="@",,drop=FALSE]
FreqLimit2Who<-order(DtmTwitter2Who,decreasing=TRUE)<=FreqLimit
DtmTwitter2Who<-DtmTwitter2Who[FreqLimit2Who,,drop=FALSE]

FreqLimitWhat<-order(DtmTwitterMatrix,decreasing=TRUE)<=25
DtmTwitterWhat<-DtmTwitterMatrix[FreqLimitWhat,,drop=FALSE]

# Produce a Wordcloud
wordcloud(rownames(DtmTwitterWhat), DtmTwitterWhat[,1], colors=brewer.pal(9,"Blues"),min.freq=1)
wordcloud(rownames(DtmTwitter2Who), DtmTwitter2Who[,1], colors=brewer.pal(9,"Blues"),min.freq=1)
wordcloud(rownames(DtmTwitterByWho), DtmTwitterByWho[,1], colors=brewer.pal(9,"Blues"),min.freq=1)

