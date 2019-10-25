#--------------------------------------------------------------
# Instacart Data for MBA - Abhirami A
#---------------------------------------------------------------
# Data info link "https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2"
# data dictionary "https://www.kaggle.com/c/instacart-market-basket-analysis/data"
# data download " https://www.instacart.com/datasets/grocery-shopping-2017 "

# use this data to test models for predicting products that a user will 
# buy again, try for the first time or add to cart next during a session.

# Assumptions - For the sake of simplicity for MBA Analysis, let us consider the
# following files and columns -
# 1. order_products_prior (csv file with ~3.2m orders)
# prior - orders prior to that users most recent order.

# Let us consider the following columns initially
# a. orderid 
# b. productid 
# c. product_name from a diff csv file
#----------------------------------------------------------------------------

# Load packages

require(tidyverse)
require(arules)
require(arulesViz)

path <- "H:/DATA SCIENCE/DS & ML/Abhirami's Projects/MBA/Instacart data/instacart_online_grocery_shopping_2017_05_01/instacart_2017_05_01/order_products_prior.csv"
product_name <- "H:/DATA SCIENCE/DS & ML/Abhirami's Projects/MBA/Instacart data/instacart_online_grocery_shopping_2017_05_01/instacart_2017_05_01/products.csv"
#data1_raw <- read.csv(path,header = TRUE)

# load only first 50,000 rows to overcome memory issues
data1_raw <- read.csv(path,header = TRUE,nrows = 50000)
products_name <- read.csv(product_name,header = TRUE,stringsAsFactors = FALSE)
head(data1_raw)
head(products_name)

# We need only the first 2 cols, order_id, product_name
# max orderid = 4603
data1 %>% head()
data1 <- data1_raw[,c(1,2)]
products_name <- products_name[,c(1,2)]
sample1 %>% head()

sample1 <- merge(x= data1,y = products_name,by= "product_id") %>% head()
str(sample1)
sample1 <- sample1[,c(2,3)]

sample1$order_id %>% max()
#data1$product_id %>% max()
#------------------------------------------------
#  Data Transformation - Convert data to a transctions format
# Steps -

# 1. Generate 2x2 table matrix on Valid invoice and StockCode (to freq counts)
# 2. Convert table matrix to a dataframe
# 3. dataframe to a matrix, with 0s and 1s as values for items
# 4. matrix to transactions format 
#---------------------------------------------

#sample1 <- data1 %>% head(20)
#table(sample1) OR
#sample1 %>% spread(key = product_id,value = product_id,fill = 0) %>% head()

# Step 1 - generate table matrix 
data_table <-  table(sample1)
data_table %>% str()

max(unique(data1$order_id))
#unique(data1$product_id)

# Step 2 - Convert table matrix to dataframe

data_df <- as.data.frame.matrix(data_table)
data_df[1:5,1:5]
str(data_df)

#step 3 - dataframe to matrix

df_matrix <- as.matrix(data_df)
index_var <- which(df_matrix[,] > 1)
df_matrix[index_var] <- 1

#step 4 - convert to transactions

df_trans <- as(df_matrix,"transactions")
df_trans %>% head(5) %>% inspect()


#----------------------------------------------

# Number of transactions - 4978
# number of products - 11616

# max transaction size
size(df_trans) %>% max() 

# freq bought items
freq_items <- eclat(df_trans, parameter = list(support = 0.01,
                                               minlen = 1,
                                               maxlen = 10))
freq_items_srt <- freq_items %>% sort(by='count',decreasing = TRUE)
freq_items_srt %>% head(10) %>% inspect()



# Association

rules1 <- apriori(df_trans,parameter = list(support = 0.001,
                                            confidence = 0.15,
                                            target = 'rules'))

rules1 %>% inspect()

#graph
plot(rules1, method = 'graph')
