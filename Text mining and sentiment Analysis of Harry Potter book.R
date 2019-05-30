
####----------------------------------------------------------------------####
##            Sentiment Analysis of Harry Potter Series (All 7 books)      ##                             ##
####----------------------------------------------------------------------####


# Installing required packages 
library(wordcloud)
library(wordcloud2)
library(tidyverse)      
library(stringr)   
#install.packages("tidytext", dependencies = T)
library(tidytext)
library(dplyr)
#install.packages("reshape2", dependencies = T)
library(reshape2)
library(tidyr)
# Below 2 Packages are used for Network Analysis
#install.packages("igraph",dependencies = T)
library(igraph)
#install.packages("ggraph",dependencies = T)
library(ggraph)

#install.packages("devtools")



# Checking for version of "devtools" and installing the same 
if (packageVersion("devtools") < 1.6) {
    install.packages("devtools")
}

# Install the package "rprojroot" which is needed to install packages from github
install.packages("rprojroot" , dependencies = T)

library(rprojroot)
library(devtools)

# Load the "harrypotter" package from github
#devtools::install_github("bradleyboehmke/harrypotter")

library(harrypotter)


# Create a character vector containing the Titles of each Harry Potter Book 

Book_Titles <- c("Philosopher's Stone", "Chamber of Secrets", 
                 "Prisoner of Azkaban", "Goblet of Fire", 
                 "Order of the Phoenix", "Half-Blood Prince",
                 "Deathly Hallows")

# Check the Data Object pertaining to the first book called philosophers_stone

class(philosophers_stone)
length(philosophers_stone)

for( i in length(philosophers_stone)) {
    
    print(nchar(philosophers_stone))
}


# Create a list containing all the 7 books text 

Books_text <- list(philosophers_stone, 
                    chamber_of_secrets, 
                    prisoner_of_azkaban,
                    goblet_of_fire, 
                    order_of_the_phoenix, 
                    half_blood_prince,
                    deathly_hallows)
require(dplyr)
require(tm)
require(tidytext)

##Each book is an array in which each value in the array is a chapter 
Harry_Potter_Series <- tibble()



for(book in seq_along(Book_Titles)) {
    
    temp_Book <- tibble(Chapter_Nbr = seq_along(Books_text[[book]]),
                        text =  Books_text[[book]]) %>%
                                unnest_tokens(word, text) %>%
                                ##Here we tokenize each chapter into words
                                mutate(Book_Name = Book_Titles[book]) %>%
                                select(Book_Name, everything())
    
    Harry_Potter_Series <- rbind(Harry_Potter_Series, temp_Book)
}

# set factor to keep books in order of publication
Harry_Potter_Series$Book_Name <- factor(Harry_Potter_Series$Book_Name, 
                                        levels = rev(Book_Titles))

glimpse(Harry_Potter_Series)


# We can get simple counts for each word using the count function. 

Harry_Potter_Series %>% count(word, sort = TRUE)


# Let's drop STOPWORDS 

Harry_Potter_Series_WO_Stp_Wrds <- Harry_Potter_Series %>% 
                                            anti_join(stop_words)


Harry_Potter_Series_Count <- Harry_Potter_Series_WO_Stp_Wrds %>% 
                                           count(word, sort = TRUE)

# Build Wordcloud 
require(wordcloud2)
wordcloud2(Harry_Potter_Series_Count , 
           size=1.5,
           shape = "star")
           


# Sentiment Analysis 
#
#  The basic motivation behind sentiment analysis is to assess how positive or 
#  negative text is, based on a dictionary of words that have been previously 
#  classified as positive or negative. This type of dictionary is called a 
#  sentiment lexicon. The tidyverse has several built in lexicons for sentiment
#  analysis, but for this example we will stick with ‘nrc’ and ‘bing’. 
#  The ‘nrc’ is a more advanced lexicon that categorizes words into several 
#  sentiment categories - sadness, anger, positive, negative, trust, etc.
#

get_sentiments("nrc")

get_sentiments("nrc") %>% group_by(sentiment) %>% summarise( n() )



get_sentiments("bing")

get_sentiments("bing") %>% group_by(sentiment) %>% summarise( n() )



get_sentiments("afinn")

get_sentiments("afinn") %>% mutate(Sentiment = ifelse(score < 0 , 'Negative',
                                                      'Positive')) %>%
                            group_by(Sentiment) %>% summarise( n() , sum(score))



get_sentiments("loughran")

get_sentiments("loughran") %>% group_by(sentiment) %>% summarise( n() )






Harry_Potter_Series_sentiment <- Harry_Potter_Series %>%
                                    right_join(get_sentiments("nrc")) %>%
                                    filter(!is.na(sentiment)) %>%
                                    count(sentiment, sort = TRUE)

Sentiment_Colors <- c("red","green","grey","pink",
                 "blue","violet","yellow","springgreen",
                 "orange","magenta" )


barplot(Harry_Potter_Series_sentiment$n,
        names.arg=Harry_Potter_Series_sentiment$sentiment,
        xlab="Sentiment",ylab="Count",
        col=Sentiment_Colors,
        main="Harry Potter Series Sentiment chart",
        las=2)


barplot(Harry_Potter_Series_sentiment$n,
        names.arg=Harry_Potter_Series_sentiment$sentiment,
        xlab="Sentiment",ylab="Count",
        col=Sentiment_Colors,
        main="Sentiment chart",
        las=2,
        horiz = T)


# The ‘bing’ lexicon only classifies words as positive or negative.

Harry_Potter_Series_Sent_Bing <- Harry_Potter_Series %>%
                                    right_join(get_sentiments("bing")) %>%
                                    filter(!is.na(sentiment)) %>%
                                    count(sentiment, sort = TRUE)


barplot(Harry_Potter_Series_Sent_Bing$n,
        names.arg=Harry_Potter_Series_Sent_Bing$sentiment,
        xlab="Sentiment",ylab="Count",
        col=c("red","green"),
        main="Sentiment chart",
        las=2)

barplot(Harry_Potter_Series_Sent_Bing$n,
        names.arg=Harry_Potter_Series_Sent_Bing$sentiment,
        xlab="Sentiment",ylab="Count",
        col=c("red","green"),
        main="Sentiment chart",
        las=2,
        horiz = T)


# Wordcloud depicting the sentiment 
#----------------------------------

windows()

Harry_Potter_Series %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#F8766D", "#00BFC4"))



# Chapter wise Sentiment Analysis 
#--------------------------------
#
#  Each book text is split into groups of 1200 words, 
#  and counts for the number of positive and negative words in each group 
#      (based on the ‘bing’ lexicon) are calculated. 
#  Then we subtract the number of negative words from the number of positive 
#      words in each group. For example, if there were 341 positive words in a 
#      group and 159 negative words in the same group, the sentiment score for 
#      the group would be 182 (a positive sentiment score). 
#  We calculate this sentiment score for each 1200 word group within each book 
#     in the series. 
#  Using ggplot, we create bar charts for each book in the series that 
#     demonstrate how the sentiment score for the text groups changes as time 
#     passes in the series. 
#
#  CONCLUSION : Overall, the sentiment of the Harry Potter series appears to be 
#               negative.
#
#


Hry_Ptr_Srs_Chap_Sntmnts <-  Harry_Potter_Series %>%
                                 group_by(Book_Name) %>% 
                                 mutate(word_count = 1:n(),
                                 index = word_count %/% 1200 + 1) %>% 
                                 inner_join(get_sentiments("bing")) %>%
                                 count(Book_Name, 
                                       index , 
                                        sentiment) %>%
                                 ungroup() %>%
                                 spread(sentiment, n, fill = 0) %>%
                                 mutate(sentiment = positive - negative,
                                        Book_Name = factor(Book_Name, 
                                                       levels = Book_Titles)) 
    

Hry_Ptr_Srs_Chap_Sntmnts %>%
          ggplot(aes(index, sentiment, fill = Book_Name)) +
          geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
          facet_wrap(~ Book_Name, ncol = 2, scales = "free_x")




####
##
##  N - Grams 
##
####
#
# So far we’ve considered words as individual units, and considered their 
# relationships to sentiments or to documents. However, many interesting text 
# analyses are based on the relationships between words, whether examining 
# which words tend to follow others immediately, or that tend to co-occur within
# the same documents.
#
# Let's explore some of the methods tidytext offers for calculating and 
# visualizing relationships between words in your text dataset.
#
# We’ll also use 1 new package: ggraph, which extends ggplot2 to construct 
# network plots,
#

# A) Tokenizing by n-gram
#------------------------

# We’ve used the unnest_tokens function to tokenize by word above, 
# which is useful for the kinds of sentiment and frequency analyses we did. 
# But we can also use the function to tokenize consecutive sequences of words, 
# called n-grams. 
# By seeing how often word X is followed by word Y, we can then build a model 
# of the relationships between them.
# We can achieve this by adding the token = "ngrams" option to unnest_tokens(), 
# and setting n to the number of words we wish to capture in each n-gram. 
# When we set n to 2, we are examining pairs of two consecutive words,
# often called “bigrams”.
#

## Each book is an array in which each value in the array is a chapter 
Harry_Potter_Series_Bi_Gram <- tibble()



for(book in seq_along(Book_Titles)) {
    
    temp_Book <- tibble(Chapter_Nbr = seq_along(Books_text[[book]]),
                        text =  Books_text[[book]]) %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        ##Here we tokenize each chapter into words
        mutate(Book_Name = Book_Titles[book]) %>%
        select(Book_Name, everything())
    
    Harry_Potter_Series_Bi_Gram <- rbind(Harry_Potter_Series_Bi_Gram, 
                                         temp_Book)
}

# set factor to keep books in order of publication
Harry_Potter_Series_Bi_Gram$Book_Name <- factor(Harry_Potter_Series_Bi_Gram$Book_Name, 
                                                levels = rev(Book_Titles))

Harry_Potter_Series_Bi_Gram


# B) Now , We can examine the most common bigrams 
#------------------------------------------------

Harry_Potter_Series_Bi_Gram %>%
    count(bigram, sort = TRUE)


# C) Applying Stop Words and removing common un-intersting words 
#----------------------------------------------------------------

# C.1) Separate the Bigram into individual words 
bigrams_separated <- Harry_Potter_Series_Bi_Gram %>%
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




####
##
##  Visualizing a network of bigrams with ggraph
##
####

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
                filter(n > 60) %>%
                graph_from_data_frame()

set.seed(2019)

ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)

                     
