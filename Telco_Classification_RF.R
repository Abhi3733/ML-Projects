#### ---- Telco Customer Churn using Random Forest ----- ####
# Author- Abhirami A
###------------------------------------------------------------


packages_req <- c('tidyverse','randomForest','mice')
sapply(packages_req,require, character.only = T)

file_path <- "C:/Users/abu/Desktop/Machine Learning/Abhirami's Projects/4. Random Forests/Telco-Customer-Churn.csv"
raw_data <- read.csv(file_path, header = T)

glimpse(raw_data)

# remove unwanted cols

data1 <- raw_data[,-1]
glimpse(data1)

# check for NAs

md.pattern(data1)

data1 <- na.omit(data1)
glimpse(data1)

# create test and train data

train_idx <- caret::createDataPartition(data1$Churn, p =0.8, list = F)
train <- data1[train_idx,]
train %>% nrow()

test <- data1[-train_idx,]
test %>% nrow()

# fit random forest model

RF_model <- randomForest::randomForest(Churn ~., data = train,
                                       ntree = 1000,
                                       mtry = 4, #sqrt(19) = 4.35
                                       nodesize = 30, #nodesize te
                                       sampsize = 75)
plot(RF_model)
RF_model

#predict values

pred_val <- predict(RF_model,newdata = test[,-20],type = 'response')

#confusion matrix

conf_matrix <- caret::confusionMatrix(test$Churn,pred_val)
conf_matrix # kappa is only 37%


# important variables

imp_var <- caret::varImp(RF_model, scale = F)
imp_var

imp_var_sorted <- imp_var %>% add_rownames() %>% arrange(-Overall)
imp_var_sorted

imp_var_sorted$rowname <- ordered(imp_var_sorted$rowname,
                           levels = imp_var_sorted$rowname)
dev.off()

plot(imp_var_sorted$rowname,imp_var_sorted$Overall)
