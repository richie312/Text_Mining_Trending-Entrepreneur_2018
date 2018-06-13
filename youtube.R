## Import packages

library(tuber)
library(stringr)
library(stringi)
library(sentimentr)
library(wordcloud)
library(tidytext)
library(wordcloud2)
library(tm)
library(plyr)
library(dplyr)


## Set the working directory

setwd("C:/Users/Richie/Desktop/NLP")


apikey<-"AIzaSyABdBR3iJRgPM5x67CZ3yepIg6EaEdaL9Y"
id<-"1009352872731-k76rb1hpv0cm5pgrt0e5c7llrai278oj.apps.googleusercontent.com"
client_secret<-"c6OsQTzyrTbiw43UUHslRCFp"


# = Autentication = #
yt_oauth(app_id = id,
         app_secret = client_secret,token='')

## Get the meta data for Elon Musk

yt_master_data<-yt_search("JackMa",published_after = "2018-01-01T00:00:00Z")

## Get the comments for Elon mUsk from any of his random video

comments<-get_all_comments(video_id = yt_master_data$video_id[2])
nrow(comments)

text<-comments$textDisplay[1:100]%>%data.frame()
colnames(text)<-c("comments")

#  remove html links, which are not required for sentiment analysis
tweet1=stri_enc_toutf8(gsub('http\\S+\\s*',"", text$comments))
tweet2=gsub("[][!#$%()*,.:;<=>@^_`|~.{}?\"&]","",tweet1)
tweet3=gsub("[[:digit:]]","",tweet2)
tweet3=stri_trans_general(tweet3,"latin-ascii")
tweet3<-tweet3%>%data.frame()
colnames(tweet3)<-c("comments")


## Sentiment Analysis

commentdf=tweet3
commentdf=commentdf[!is.na(commentdf$comments),]
commentdf<-commentdf%>%data.frame(stringsAsFactors = FALSE)
colnames(commentdf)<-c("comments")
commentdf$comments<-as.character(commentdf$comments)
colnames(commentdf)<-c("comments")

## Write the local file on hard drive

write.csv(commentdf,"JackMa.csv",row.names = FALSE)






  
  
  