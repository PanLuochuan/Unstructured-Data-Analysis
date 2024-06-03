###PROJECT 2
library(topicmodels)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tm)
library(tidytext)

## Using the data from the sports folder
sports<-DirSource("D:/ukm.ppt/非结构数据/project2/sports")
docs<-VCorpus(sports)

docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removePunctuation) #remove punctuation
docs <- tm_map(docs, removeNumbers) #Stripdigits
docs <- tm_map(docs, removeWords, stopwords("english")) #remove stopwords
docs<-tm_map(docs, removeWords, c("said", "will","just"))
docs <- tm_map(docs, stripWhitespace) #remove whitespace
docs <- tm_map(docs, stemDocument)
dtm <- DocumentTermMatrix(docs)

###1.

sp_lda<-LDA(sport,k=4,control=list(seed=1234))
terms(sp_lda, 8)
sp_topics<-tidy(sp_lda,matrix="beta")
sp_top_terms <- sp_topics %>% group_by(topic) %>% top_n(8,beta) %>% ungroup () %>% arrange (topic, -beta)
sp_top_terms%>% mutate(term=reorder(term,beta))%>%
  ggplot(aes(term,beta,fill=factor(topic)))+geom_col(show.legend=FALSE)+
  facet_wrap(~topic,scales="free")+coord_flip()

beta_spread <- sp_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic1>0.003 | topic2 > 0.003) %>% mutate(log_ratio = log2(topic2/topic1))
beta_spread <- sp_topics %>% mutate (topic=paste0("topic",topic)) %>% spread(topic,beta) %>%
  filter (topic3>0.003 | topic4 > 0.003) %>% mutate(log_ratio = log2(topic4/topic1))
beta_diff <- tidy(sp_lda, matrix = "beta") %>%
  spread(topic, beta) %>%
  mutate(beta_diff = abs(`1` - `2`)) %>%
  arrange(desc(beta_diff))
  arrange(desc(beta_diff))
beta_diff <- tidy(sp_lda, matrix = "beta") %>%
  spread(topic, beta) %>%
  mutate(beta_diff = abs(`3` - `4`)) %>%
  arrange(desc(beta_diff))

max_diff <- beta_diff[1, "beta_diff"]

beta_spread%>% mutate(term=reorder(term,log_ratio))%>%
  ggplot(aes(term,log_ratio))+geom_col(show.legend=FALSE)+coord_flip()

ap_documents<-tidy(sp_lda,matrix="gamma")
ap_documents

doc_topic_matrix <- spread(ap_documents, key = topic, value = gamma)
first_document_name <- "'Emotional' Nadal knocked out of Madrid Open by Lehecka.txt"
tidy(dtm)%>%filter(document==first_document_name)%>%arrange(desc(count)) 

###2.
library(proxy)
tdm.tfidf <- weightTfIdf(dtm)
tdm.tfidf <- removeSparseTerms(tdm.tfidf, 0.999)
tfidf.matrix <- as.matrix(tdm.tfidf)


library(dbscan) 
dist.matrix <- dist(tfidf.matrix, method = "cosine")
truth.K=5
clustering.kmeans <- kmeans(tfidf.matrix, truth.K)
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
clustering.dbscan <- hdbscan(dist.matrix, minPts= 5) 
summary(clustering.kmeans)
summary(clustering.hierarchical)
summary(clustering.dbscan)

library(cluster)
clusplot(as.matrix(dist.matrix),clustering.kmeans$cluster,color=T,shade=T,labels=2,lines=0)
plot(clustering.hierarchical)
rect.hclust(clustering.hierarchical,5)
plot(as.matrix(dist.matrix),col=clustering.dbscan$cluster+1L)

master.cluster <- clustering.kmeans$cluster
slave.hierarchical <- cutree(clustering.hierarchical,k = truth.K)
slave.dbscan <- clustering.dbscan$cluster

table(master.cluster)
table(slave.hierarchical)
table(slave.dbscan)

library(colorspace)
points <- cmdscale(dist.matrix, k = 3)
palette <- diverge_hcl(truth.K) 
layout(matrix(1:3,ncol=1))
plot(points, main = 'K-Means clustering', col = as.factor(master.cluster),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
plot(points, main = 'Hierarchical clustering', col= as.factor(slave.hierarchical),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan),
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

cost_df <- data.frame()

for(i in 1:20){
  kmeans<- kmeans(x=tfidf.matrix, centers=i, iter.max=100)
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")
plot(cost_df$cluster, cost_df$cost)
lines(cost_df$cluster, cost_df$cost)

### 3.
library(tm)
library(SnowballC)
library(RColorBrewer)
library(syuzhet)
library(wordcloud)
text<-read.csv(file.choose(),sep = ",") # Using sentiment-analysis.csv
head(text)
df <- text %>%
  separate(`Text..Sentiment..Source..Date.Time..User.ID..Location..Confidence.Score`, into = c("Text", "Sentiment", "Source", "Date/Time", "User ID", "Location", "Confidence Score"),
           sep = ",", remove = TRUE, convert = TRUE)
head(df)
clean_data<-na.omit(df)
Text <- clean_data$Text

text_words <- clean_data %>%unnest_tokens(word, Text) %>%anti_join(get_stopwords()) 
sentiment_words <- text_words %>%inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%top_n(10) 
wordcloud(words = sentiment_words$word, freq = sentiment_words$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))

syuzhet_vector <- get_sentiment(Text, method="syuzhet")
head(syuzhet_vector)
summary(syuzhet_vector)

bing_vector <- get_sentiment(Text, method="bing")
head(bing_vector)
summary(bing_vector)

afinn_vector <- get_sentiment(Text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

nrc_vector <- get_sentiment(Text, method="nrc")
head(nrc_vector)
summary(nrc_vector)

rbind(sign(head(syuzhet_vector)),sign(head(bing_vector)),sign(head(afinn_vector)))

d<-get_nrc_sentiment(Text) 
head (d,10)
td<-data.frame(t(d)) 
td_new <- data.frame(rowSums(td)) 
names(td_new)[1] <- "count" 
td_new <- cbind("sentiment" = rownames(td_new), td_new) 
rownames(td_new) <- NULL 
td_new2<-td_new[1:8,]

quickplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment,ylab="count")+ggtitle("Survey sentiments")
barplot(sort(colSums(prop.table(d[, 1:8]))),horiz = TRUE,cex.names = 0.7,las = 1,main = "Emotions in Text", xlab="Percentage")






