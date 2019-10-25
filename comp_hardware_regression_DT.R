#### ------------------ Regression using Decision Tree ----------------- ####
# Author - Abhirami A
#URL <- https://archive.ics.uci.edu/ml/datasets/Computer+Hardware
# Dataset - COmputer H/W dataset
# Abstract: Relative CPU Performance Data, described in terms of its cycle time, memory size, etc.

# Data Set Information:
#   
# The estimated relative performance values were estimated by the authors using a linear regression method. 
# Attribute Information:
#   
# 1. vendor name: 30 
# 2. Model Name: many unique symbols 
# 3. MYCT: machine cycle time in nanoseconds (integer) 
# 4. MMIN: minimum main memory in kilobytes (integer) 
# 5. MMAX: maximum main memory in kilobytes (integer) 
# 6. CACH: cache memory in kilobytes (integer) 
# 7. CHMIN: minimum channels in units (integer) 
# 8. CHMAX: maximum channels in units (integer) 
# 9. PRP: published relative performance (integer) 
# 10. ERP: estimated relative performance from the original article (integer)



#--------------------------------------------------------------------------------


# load required packages

packages_req <- c("dplyr","rpart","rpart.plot","mice","caret","skimr")
sapply(packages_req, require, character.only = T)

# load data
data_file <- "C:/Users/abu/Desktop/Machine Learning/Abhirami's Projects/3. Decision Trees/Regression using DT/machine.data"
raw_data_comp <- read.delim(data_file, header = F, sep = ',')

col_names <- c("vendor_name","model_name","MYCT","MMIN","MMAX","CACH","CHMIN","CHMAX",
               "PRP","ERP")
colnames(raw_data_comp) <- col_names

glimpse(raw_data_comp)

# EDA
# remove unwanted cols

data1 <- raw_data_comp[,-c(1,2)]
glimpse(data1)


# check for NAs
colSums(is.na(data1))

#check for missing data
md.pattern(data1)

# see unique data
apply(data1, 2, unique)

# see the dristribution of data

skim(data1)
#shuffle data
shuffled_data <- data1[sample(nrow(data1)),]
shuffled_data %>% glimpse()

# split into train and test data

train_idx <- caret::createDataPartition(shuffled_data$ERP, p=0.8,list = F)
train <- shuffled_data[train_idx,]
train %>% nrow()

test <- shuffled_data[-train_idx,]
test %>% nrow()

# fit the model

DT_regression <- rpart::rpart(ERP ~.,data = train,method = "anova")
DT_regression

# visualize the model

rpart.plot(DT_regression)

#predict the model

pred_val <- predict(DT_regression, newdata = test[,-8],method = "anova")

# evaluation metrics

rmse_reg_DT <- RMSE(pred_val,test$ERP)
rmse_reg_DT

# calculate r2

rss <- sum((pred_val - test$ERP)^2)
tss <- sum((test$ERP - mean(test$ERP))^2)
r2 <- 1-(rss/tss)
r2
