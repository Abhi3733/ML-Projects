## ---- CLassification of Telco Customer Churn using KNN ---- ##
# AUthor - Abhirami A
##----------------------------------------------------------------

packgs_req <- c("class","mice","dplyr","dummies")
sapply(packgs_req, require, character.only = T)

data_location <- "C:/Users/abu/Desktop/Machine Learning/Abhirami's Projects/6. KNN/Telco-Customer-Churn.csv"
raw_data_telco <- read.csv(data_location,header = T)

glimpse(raw_data_telco)


# remove unwated cols
data1 <- raw_data_telco[,-1]
glimpse(data1)


# check for NAs
md.pattern(data1)
data1 <- na.omit(data1)
str(data1)

# encode target var as numeric
# code No as 0 and yes as 1

data1$Churn <- if_else(data1$Churn == "No",0,1)
data1$Churn <- as.factor(data1$Churn)
data1%>% glimpse()

# select only numeric data cols

numeric_cols <- dplyr::select_if(data1,is.numeric) %>% colnames()
numeric_cols_names <- numeric_cols %>% head()
numeric_df <- data1[numeric_cols_names]
glimpse(numeric_df)

#boxplot
boxplot(numeric_df)

#select categorical data cols
categ_data <- sapply(data1,is.factor)
categ_data_cols <- colnames(data1[categ_data])
categ_df <- data1[categ_data_cols] #  create df
categ_df %>% glimpse()

# -----------------------------
unique(categ_df$gender)
unique(categ_df$Partner)
unique(categ_df$Dependents)
unique(categ_df$PhoneService)
unique(categ_df$MultipleLines)
table(categ_df$MultipleLines)

# No      No phone service          Yes 
# 3385              680             2967

unique(categ_df$InternetService)
table(categ_df$InternetService)

# DSL    Fiber optic       No 
# 2416        3096        1520 


unique(categ_df$OnlineSecurity)
table(categ_df$OnlineSecurity)
# No    No internet service  Yes
# 3497                1520   2015
 
unique(categ_df$OnlineBackup)
table(categ_df$OnlineBackup)

# No     No internet service   Yes
# 3087                1520     2425

unique(categ_df$DeviceProtection)
table(categ_df$DeviceProtection)

#No    No internet service     Yes
#3094                1520    2418
 

unique(categ_df$TechSupport)
table(categ_df$TechSupport)
# No No internet service      Yes
# 3472                1520    2040

unique(categ_df$StreamingTV)
table(categ_df$StreamingTV)
# No No internet service     Yes
# 2809                1520   2703

unique(categ_df$StreamingMovies)
table(categ_df$StreamingMovies)

# No    No internet service                 Yes 
# 2781                1520                2731 

unique(categ_df$Contract)
table(categ_df$Contract)

# Month-to-month       One year       Two year 
# 3875                 1472           1685 

unique(categ_df$PaperlessBilling)
table(categ_df$PaperlessBilling)
# No  Yes 
# 2864 4168 

unique(categ_df$PaymentMethod)
table(categ_df$PaymentMethod)


# Bank transfer (automatic)   Credit card (automatic) 
# 1542                      1521 
# Electronic check              Mailed check 
# 2365                      1604 

unique(categ_df$Churn) 
table(categ_df$Churn)
# 0 - No, 1- Yes
# 0    1 
# 5163 1869

# all the above can be replaced with 
sapply(categ_df,table)

#-----------------------------------------------------------

# center and scale data numeric data frame
data1_std <- base::scale(numeric_df, center = T, scale = T)
data1_std <- as.data.frame(data1_std)
glimpse(data1_std)

# OHE of categorical data
categ_df_ohe <- dummy.data.frame(categ_df[,-16]) # without target var
categ_df_ohe %>% head()
glimpse(categ_df_ohe)
# add back target var to catge data
categ_df_ohe <- cbind(categ_df[,16],categ_df_ohe)
glimpse(categ_df_ohe)

categ_df_ohe <- categ_df_ohe %>% rename("Churn"="categ_df[, 16]")
categ_df_ohe %>% glimpse()

# combine scaled and standardized cols
total_df <- cbind(data1_std,categ_df_ohe)
glimpse(total_df)

sapply(data1_std, mean)
sapply(data1_std, sd)
# split into train and test data

train_idx <- caret::createDataPartition(total_df$Churn, p =0.8, list = F)
train <- total_df[train_idx,]
train %>% nrow()
table(train$Churn) %>% prop.table()

test <- total_df[-train_idx,]
test %>% nrow()
table(test$Churn) %>% prop.table()

# calculate k value
k_val <- train %>% nrow() %>% sqrt() %>% as.integer

# Run the knn model
knn_model <- class::knn(train,test,total_df[train_idx,5], k = k_val)
summary(knn_model) %>% prop.table()

# No predict step in knn, directly run knn model

# negative = 0, churn yes =1 is postive class
table(test$Churn,knn_model)

confusion_matrix <- caret::confusionMatrix(knn_model,test$Churn,positive = '1')
confusion_matrix

# accuracy - 89%
# kappa - 73 %

# sensitivity/TPR/recall - 75% --> TP/actual Yes
# Specificity - 95% --> TP/

# ROC curve to see the performance of the classifier

require(pROC)
knn_roc <- roc(test$Churn,knn_model_prob)
# error -  Predictor must be numeric or ordered.

# one work around is model knn as numeric or ordered(not sure if correct)

test_roc <- as.numeric(test$Churn)
knn_model <- as.numeric(knn_model)

knn_roc <- roc(test_roc,knn_model)
plot(knn_roc)

# ARea under curve - 85.46
knn_roc$auc 

# other roc metrics
knn_roc$sensitivities
knn_roc$specificities
knn_roc$thresholds
