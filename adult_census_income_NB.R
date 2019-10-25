#### ------ Classification using Naive Bayes ------- ####
# AUthor - Abhirami A

#--------------------------------------------------------------------------

# Dataset --> Census Income Data Set 
# URL --> https://archive.ics.uci.edu/ml/datasets/Census+Income

# Prediction task is to determine whether a person makes over 50K a year.
# -----------------------------------------------------------------------------
# Listing of attributes: 
 
#   >50K, <=50K. 

# age: continuous. 
# workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked. 
# fnlwgt: continuous.
# education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool. 
# education-num: continuous. 
# marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse. 
# occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces. 
# relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried. 
# race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black. 
# sex: Female, Male. 
# capital-gain: continuous. 
# capital-loss: continuous. 
# hours-per-week: continuous. 
# native-country: country names.

# Note on fnlwgt - People with similar demographic characteristics should have similar weights. 

#-------------------------------------------------------------------------------

# load required packages

require(tidyverse)
require(mice)
require(caret)
require(e1071)

file_location <- 'C:/Users/abu/Desktop/Machine Learning/Abhirami\'s Projects/5. Naive Bayes/adult.data'
raw_data <- read.delim2(file_location,header = F, sep = ',')
glimpse(raw_data)

col_names <- c('age','workclass','fnlwgt','education','edu_num','marital_st','occu',
               'relation','race','sex','cap_gain','cap_loss','hrs_per_week','native_country','income')
colnames(raw_data) <- col_names

data1 <- raw_data

#EDA

colSums(is.na(data1))

# see unique values for each column
apply(data1,2,unique)

# we see a few ? in the workclass col, lets deal with them

data1$workclass %>% head()
data1$workclass <- data1$workclass %>% as.character()
table(data1$workclass)
data1$workclass<- ifelse(data1$workclass == " ?",NA,data1$workclass)
data1$workclass %>% unique()

data1$workclass <- as.factor(data1$workclass)

glimpse(data1)

# deal with' ?' in occu col
  
data1$occu <- as.character(data1$occu)
data1$occu <- ifelse(data1$occu == " ?",NA,data1$occu)
data1$occu %>% unique()
data1$occu <- as.factor(data1$occu)

# deal with ' ?' in $native_country

data1$native_country <- as.character(data1$native_country)
data1$native_country <- ifelse(data1$native_country == ' ?', NA, data1$native_country)
data1$native_country %>% unique()
data1$native_country <- as.factor(data1$native_country)

glimpse(data1)

#check for NAs and remove NAs

md.pattern(data1)

data2 <- data1[complete.cases(data1),]
data2 %>% nrow()

# create train and test data

set.seed(3733)

train_index <- caret::createDataPartition(data2$income,p=0.8,list = F)
train_data <- data2[train_index,]
train_data %>% nrow()

test_data <-data2[-train_index,]
test_data %>% nrow()

# NB Model

NB_model <- e1071::naiveBayes(income ~.,data = train_data )

pred_val <- predict(NB_model, newdata = test_data[,-15], type = "class")

contingency_tbl <- table(test_data$income,pred_val)

conf_matrix <- caret::confusionMatrix(contingency_tbl)

conf_matrix$table
conf_matrix$overall[2] #kappa
conf_matrix$overall[1] #accuracy
