#********************************************************************************#
#******                           WORD Cloud in R                         *******#
#******              Steve Job's 2005 Speech at Stanford                  *******#
#********************************************************************************#

## User Defined Function to find the Packages not already installed 
##      and then install those packages only 
##-------------------------------------------------------------------------------
UDF_Install_Multiple_Packages <- function(Pckgs) {
  
  # Check for Installed packages 
  Installed_Pckgs <- installed.packages()[,1]
  
  Not_Installed_Pckgs <- which(! (Pckgs %in% Installed_Pckgs))
  
  if (length(Not_Installed_Pckgs) == 0) {
    print( " All Packages already Installed !!")
  }else {
    print(paste(length(Not_Installed_Pckgs) , "Package not already installed " ,
                ", being installed now ..........."))
  }
  
  for ( i in Not_Installed_Pckgs ){
    
    install.packages( Pckgs[i] , dependencies = T)
    
  }
  
}

##       End of User Defined Fucntion Definition
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


##  List all Packages being used in this R Script 
##       And have those Packages Installed & Loaded 
##-------------------------------------------------------------------------------


# Here list all the Packages to be used in this R Script
Packages_To_Install <- c( "tm" ,               # Package for Text Mining
                          "SnowballC",         # Package for text Stemming
                          "wordcloud",         # Package for Wordcloud generator
                          "RColorBrewer",      # Package for Color Palletes
                          "stringr",
                          "wordcloud2")
                               
                          

# Call the User defined function to Install all the required packages
UDF_Install_Multiple_Packages(Packages_To_Install)

# Load all the required packages 
sapply(Packages_To_Install , require , character.only = T)

## Step-1 : Import the Speech file into R 
##----------------------------------------

Speech_Text <- readLines(file.choose())

class(Speech_Text)

length(Speech_Text)

## Step-2 : Convert the Character Vector to Corpus 
##------------------------------------------------
# Create corpus
Corpus_Doc <- VCorpus(VectorSource(Speech_Text))

# Check the Class type of Corpus data structure
class(Corpus_Doc)

# View the content of the corpus
inspect(Corpus_Doc)

summary(Corpus_Doc)

Corpus_Doc

# Find the number of documents in the corpus
length(Corpus_Doc)

inspect(Corpus_Doc[[1]])

writeLines(as.character(Corpus_Doc[[1]]))

## Step-3 : Cleaning the text
##------------------------------------------------

# All the transformations available in R 
getTransformations()

Replace_With_Space <- content_transformer(function(x , pttrn) {
                      return(str_replace_all(x , pttrn , " ")) }
                      )


# Replace_With_Space <- content_transformer(function(x , pttrn){(return(gsub(pttrn," ", x)))})

inspect(Corpus_Doc[[4]])

Corpus_Doc <- tm_map(Corpus_Doc , Replace_With_Space , "/")
Corpus_Doc <- tm_map(Corpus_Doc , Replace_With_Space , "@")
Corpus_Doc <- tm_map(Corpus_Doc , Replace_With_Space , "\\|")
Corpus_Doc <- tm_map(Corpus_Doc , Replace_With_Space , "-")
Corpus_Doc <- tm_map(Corpus_Doc , Replace_With_Space , ":")
Corpus_Doc <- tm_map(Corpus_Doc , Replace_With_Space , "'")

inspect(Corpus_Doc[[4]])

Corpus_Doc <- tm_map(Corpus_Doc , content_transformer(tolower))

# Remove punctuations 
Corpus_Doc <- tm_map(Corpus_Doc , removePunctuation)
Corpus_Doc <- tm_map(Corpus_Doc , removeNumbers)

inspect(Corpus_Doc[[4]])

# Remove Stop Words 
#-------------------

# List stopwords 
stopwords("english")

class(stopwords("english"))


Corpus_Doc <- tm_map(Corpus_Doc , removeWords ,stopwords("english"))

# Updating the Stopword list with your own additional words 

MY_Stop_Word <- c(stopwords("english") , c("Satya","Vamsi"))

Corpus_Doc <- tm_map(Corpus_Doc , removeWords ,MY_Stop_Word)

inspect(Corpus_Doc[[1]])

# Remove unnecessary whitespaces 
Corpus_Doc <- tm_map(Corpus_Doc , stripWhitespace)

inspect(Corpus_Doc[[1]])

# Stemming
#----------
# Typically a large corpus will contain  many words that have a common root - 
#    for example: offer, offered and offering.  Stemming is the process of 
#    reducing such related words to their common root, which in this case would 
#    be the word offer.

Corpus_Doc_Stmmd <- tm_map(Corpus_Doc , stemDocument)

inspect(Corpus_Doc_Stmmd[[1]])

# There is a more sophisticated procedure called lemmatization that takes 
# grammatical context into account. 


# Creating Document term matrix or Term Document matrix from the Corpus
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#  document term matrix  (DTM)- a matrix that lists all occurrences of words 
# in the corpus, by document. In the DTM, the documents are represented by rows 
# and the terms (or words) by columns.  If a word occurs in a particular 
# document, then the matrix entry for corresponding to that row and column is 1,
# else it is 0 (multiple occurrences within a document are recorded - that is, 
# if a word occurs twice in a document, it is recorded as "2" in the relevant 
# matrix entry).

Doc_DTM <- DocumentTermMatrix(Corpus_Doc)
inspect(Doc_DTM)


Doc_TDM <- TermDocumentMatrix(Corpus_Doc)
inspect(Doc_TDM)

## Computing Word Frequency
##---------------------------

Word_Freq <- colSums(as.matrix(Doc_DTM))

length(Word_Freq)
class(Word_Freq)

# Sort the word Frequency vector in descending order of frequency
Word_Freq_sorted_Desc <- sort(Word_Freq , decreasing = T)

head(Word_Freq_sorted_Desc , 10)
tail(Word_Freq_sorted_Desc , 10)

# FInd frequent terms
findFreqTerms(Doc_DTM , lowfreq = 10)


Word_freq_df <- data.frame(Name = names(Word_Freq_sorted_Desc),
                            freq = Word_Freq_sorted_Desc)
 

head(Word_freq_df)

## Draw the Wordcloud 
##--------------------

set.seed(100)

windows()

wordcloud(names(Word_Freq_sorted_Desc), Word_Freq_sorted_Desc , 
          min.freq = 1 ,
          random.order = F , rot.per = 0.35,
          colors = brewer.pal(6,"Dark2"))


wordcloud2(Word_freq_df , size = 0.7)

wordcloud2(Word_freq_df , size = 0.12 , shape = 'star')

wordcloud2(Word_freq_df , size = 0.7 , shape = 'diamond')

wordcloud2(Word_freq_df , size = 0.7 , shape = 'triangle')

wordcloud2(Word_freq_df , size = 0.7 , shape = 'pentagon')

wordcloud2(Word_freq_df , size = 0.7 , shape = 'star' ,
           color = 'random-light')

wordcloud2(Word_freq_df , size = 0.7 , shape = 'star' ,
           color = 'random-dark',
           backgroundColor = 'yellow')
