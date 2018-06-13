library(tidytext)
library(dplyr)
library(ggplot2)
library(sentimentr)
library(wordcloud2)



## Read the .csv file for sentiment analysis 

x<-file.choose()

get_file<-function(x){
  read.csv(x,stringsAsFactors = FALSE)
}

## read the file
commentdf<-get_file(x)

## Get the sentiment using tidytext package

text_df<-data_frame(line = 1, text=commentdf$comments)
text_df2 <- text_df %>%
  unnest_tokens(word,text)
Sentiment<-text_df2%>%inner_join(get_sentiments("nrc"),by="word")


sentiment_df<-group_by(Sentiment,sentiment)

sentiment_df<-summarise(sentiment_df,count=n())




## Plot the Emotion pie diagram

library(plotrix)

pie3D(sentiment_df$count, labels = sentiment_df$sentiment, 
      main = "Categorisation of words into different levels of emotion", 
      explode=0.2, radius=.9, labelcex = 1,  start=1)

## Sentiment analysis using sentimentr

sentiment_df2<-sentiment(commentdf$comments)

## Categorise the words to very negative to very positive from comments
Neg<-nrow(as.data.frame(sentiment_df2$element_id[sentiment_df2$sentiment<0]))
Neutral<-nrow((as.data.frame(sentiment_df2$element_id[sentiment_df2$sentiment==0])))
Pos<-nrow(as.data.frame(sentiment_df2$element_id[sentiment_df2$sentiment>0 & sentiment_df2$sentiment<=0.5]))
vPos<-nrow(as.data.frame(sentiment_df2$element_id[sentiment_df2$sentiment>0.5]))
## Build the data frame
Tweets=as.data.frame(c(Neg,Neutral,Pos,vPos))
colnames(Tweets)=c("Number of Comments")
Emotion=as.data.frame(c("Neg","Neutral","Pos","vPos"))
colnames(Emotion)=c("Degree of Emotion")
Plot.Result=as.data.frame(cbind(Emotion,Tweets))


## ggplot for sentimentr plot

ggplot(data=Plot.Result,aes(x=Plot.Result$`Degree of Emotion`,y=Plot.Result$`Number of Comments`))+
  geom_bar(aes(fill=Plot.Result$`Degree of Emotion`),stat="identity",width=0.4)+
  scale_fill_brewer(palette="Dark2")+xlab("Degree of Emotion")+
  ylab("Number of Comments")+
  geom_text(aes(label=Plot.Result$`Number of Comments`),
            vjust=-0.5,colour="brown",stat="identity")+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),legend.position = 'none')

## ggplot for word count

word_count<-text_df2%>%inner_join(get_sentiments("bing"))%>%count(word,sentiment,sort=TRUE)


word_count %>%
  filter(n > 1) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")


## Wordcloud2
library(tm)

## read the stopwords list
stopwords=scan("stopwords.txt",what='character',comment.char = ';')

text_df2<-stri_trans_general(text_df2,"latin-ascii")

Corpus=Corpus(VectorSource(text_df2))
Corpus=tm_map(Corpus,content_transformer(tolower))
Corpus=tm_map(Corpus,removeWords,c(stopwords,"00a0","00bc","00b7","0b95","0bcd"))

myDTM = TermDocumentMatrix(Corpus)
m = as.matrix(myDTM)
v = sort(rowSums(m), decreasing = TRUE)
d<-data.frame(word=names(v),freq=v)

wordcloud2(data=d[1:150,],size=0.5,color="random-light")
