# Word cloud, Sentiment Analysis and Network graphs for GOT Final episode fan reviews
# Author - Abhirami A

#install.packages("wordcloud2")
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("wordcloud2")
library("RColorBrewer")
library("stringr")


getwd()
setwd("H:/DATA SCIENCE/DS & ML")

got_file_loc <- ("H:/DATA SCIENCE/DS & ML/Word Cloud  & Sentiment analysis Projects/GOT_finale_fan_reactions/got_finale_fan_review.txt")

got_text <- readLines(got_file_loc) 
got_text <- str_replace_all(got_text ,'[^ a-zA-Z]'," ")

class(got_text)

length(got_text)

head(got_text)
head(got_df)


x## Load the data as Corpus

got_docs <- Corpus(VectorSource(got_text))

class(got_docs)


# View the content of the 
inspect(got_docs)


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ",
                                                            x))

got_docs <- tm_map(got_docs, toSpace, "/")
got_docs <- tm_map(got_docs, toSpace, "@")
got_docs <- tm_map(got_docs, toSpace, "\\|")
got_docs <- tm_map(got_docs, toSpace, "\\-")


# Convert the text to lower case
got_docs <- tm_map(got_docs, content_transformer(tolower))
# Remove numbers
got_docs <- tm_map(got_docs, removeNumbers)
# Remove english common stopwords
got_docs <- tm_map(got_docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector

got_docs <- tm_map(got_docs, removeWords, c("game of thrones","one","two","thrones","last","ending","got","snow",
                            "three","four","five","six","season","show",
                                            "seven","eight","fans","game","said","shows","final","series","now","getting","also","didn","though","couln","can","episodes","around","episodes","don","first",
                                        "can","made","saw","shows","finale","episode","episodes")) 

# Remove punctuations
got_docs <- tm_map(got_docs, removePunctuation)
# Eliminate extra white spaces
got_docs <- tm_map(got_docs, stripWhitespace)


# Text stemming

#stemDocument(c("episodes","episode))

got_dtm <- TermDocumentMatrix(got_docs)

m <- as.matrix(got_dtm)

v <- sort(rowSums(m),decreasing=TRUE)


d <- data.frame(word = names(v),freq=v,stringsAsFactors = F)

head(d, 10)

glimpse(d)



set.seed(1234)

windows() ## create window to plot your file
wordcloud2(d , size=.7)
#wordcloud2(d,shape = 'cardioid')

#wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#          max.words=200, random.order=FALSE, rot.per=0.35, 
#          colors=brewer.pal(8, "Dark2"))

# __________ SENTIMENT ANALYSIS________________________

#nrc sentiment lexicon

require(tidytext)
require(dplyr)

get_sentiments("nrc")
get_sentiments("nrc") %>% group_by(sentiment) %>% summarise( n() )


#bing sentiment lexicon
get_sentiments("bing")
get_sentiments("bing") %>% group_by(sentiment) %>% summarise( n() )


#afinn sentiment lexicon
get_sentiments("afinn")
get_sentiments("afinn") %>% mutate(Sentiment = ifelse(score < 0 , 'Negative',
                                                      'Positive')) %>%
  group_by(Sentiment) %>% summarise( n() , sum(score))


#Loughran sentiment lexicon

get_sentiments("loughran")
get_sentiments("loughran") %>% group_by(sentiment) %>% summarise( n() )


#GOT finale fan reviews sentiment analysis 
# nrc lexicon

got_finale_sentiment_nrc <- d %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

Sentiment_Colors <- c("red","green","grey","pink",
                      "blue","violet","yellow","springgreen",
                      "orange","magenta" )

barplot(got_finale_sentiment_nrc$n,
        names.arg=got_finale_sentiment_nrc$sentiment,
        xlab="Sentiment (nrc)",ylab="Count",
        col=Sentiment_Colors,
        main="GOT Finale Fan Reviews Sentiment Analysis",
        las=2)

# bing Lexicon
got_finale_sentiment_bing <- d %>%
  right_join(get_sentiments("bing")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)


barplot(got_finale_sentiment_bing$n,
        names.arg=got_finale_sentiment_bing$sentiment,
        xlab="Sentiment(bing)",ylab="Count",
        col=c("red","green"),
        main="GOT Finale Fan Reviews Sentiment Analysis",
        las=2)

barplot(got_finale_sentiment_bing$n,
        names.arg=got_finale_sentiment_bing$sentiment,
        xlab="Sentiment",ylab="Count",
        col=c("red","green"),
        main="Sentiment chart",
        las=2,
        horiz = T)
#pie chart
slices <-  got_finale_sentiment_bing$n
lbls <- got_finale_sentiment_bing$sentiment

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 

pie(slices,labels = lbls, col = rainbow(length(lbls)),
    main = "GOT Finale Fan Review Sentiment Analysis(bing)")

# 3D Exploded Pie Chart
#install.packages("plotrix", dependencies = T)

library(plotrix)

pie3D(slices,labels=lbls,explode=0.1,
      main="GOT Finale Fan Review Sentiment Analysis(bing)")


## Wordcloud depicting the sentiment 
#----------------------------------
require(reshape2)
windows()

got_finale_sentiment_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"))

# Tokenizing by n-gram
#------------------------
require(tidytext)
bigram <- tibble()

got_df <- as.data.frame(got_text, stringsAsFactors = F)
class(got_df)

glimpse(got_df)
got_finale_Bi_Gram <- got_df %>% unnest_tokens(bigram,got_df,token = "ngrams", n = 2,n_min =2)

  
#Now , We can examine the most common bigrams 
#------------------------------------------------

got_finale_Bi_Gram %>% count(bigram, sort = TRUE)

#Applying Stop Words and removing common un-intersting words 
#----------------------------------------------------------------
require(tidyr)
# C.1) Separate the Bigram into individual words 
bigrams_separated <- got_finale_Bi_Gram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# Unite the separated words of Bigram 
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

bigrams_united %>% count(bigram, sort = TRUE)




#______________________________________________
##  Visualizing a network of bigrams with ggraph

require(igraph)
require(ggraph)
# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 600) %>%
  graph_from_data_frame()

set.seed(2019)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

