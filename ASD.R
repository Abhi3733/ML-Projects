### ---- Autistic Spectrum Disorder Screening Data for Adult ---- ###

### Decision Tree classification

# URL -> https://archive.ics.uci.edu/ml/datasets/Autism+Screening+Adult

# Autistic Spectrum Disorder (ASD) is a neurodevelopment  condition associated 
# with significant healthcare costs, and early diagnosis can significantly reduce
# these. 

# In this dataset, we record ten behavioural features (AQ-10-Adult) plus 
# ten individuals characteristics that have proved to be effective in detecting the
# ASD cases from controls in behaviour science. 

file_path <- 'C:/Users/abu/Desktop/Machine Learning/Abhirami\'s Projects/Decision Trees/Autism-Adult-Data Plus Description File/Autism-Adult-Data.arff'
#arff file - Weka Attribute-Relation File Format (ARFF) files

install.packages('sos') # to get help about a particular function
require(sos)

findFn("arff")

require(farff)
raw_data <- readARFF(file_path) #Read ARFF file into data.frame

glimpse(raw_data)

packages_needed <- c("tidyverse","rpart","rpart.plot","party","mice","caret")
sapply(packages_needed, require, character.only = T)

data1 <- raw_data
data1 <- data1 %>% rename(ASD_Class = `Class/ASD`)

glimpse(data1)

data1 %>% head()
data1 %>% tail()

# shuffle data
data2 <- data1[sample(nrow(data1)),]
data2 %>% head()

#distribution of data
table(data2$ASD_Class)
barplot(table(data2$ASD_Class))

#chk missing values
md.pattern(data2)
complete_data_idx <- complete.cases(data2)

data3 <- data2[complete_data_idx,]
md.pattern(data3)

# create train and test data

train_idx <- caret::createDataPartition(data3$ASD_Class, p =0.8, list = F)

ASD_train <- data3[train_idx,]
nrow(ASD_train)

ASD_test <- data3[-train_idx,]
nrow(ASD_test)

### ---- Fit the Decision Tree --- ###

D_Tree_model <- rpart::rpart(ASD_Class ~ ., data = ASD_train, method = 'class')
#summary(D_Tree_model)

rpart.plot(D_Tree_model)

#predict
index1 <- which(colnames(ASD_test) == 'ASD_Class')
ASD_predicted <- predict(D_Tree_model,ASD_test[,-index1], type  = 'class')

glimpse(ASD_predicted)
table(ASD_predicted)
table(ASD_test$ASD_Class)


glimpse(ASD_test)

Conf_mtx_Dtree <- caret::confusionMatrix(ASD_predicted,ASD_test$ASD_Class)
Conf_mtx_Dtree                                         
        
# It seems a good model as there is 100% accuracy and Kappa is one.                               
# Data is very straightforward as we get result at only one branching.


# Visualization of Confusion Matrix
graphics::fourfoldplot(Conf_mtx_Dtree$table, 
                       color = c("#CC6666", "#99CC99"),
                       conf.level = 0, margin = 1, 
                       main = "Confusion Matrix")
