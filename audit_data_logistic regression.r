###### ---- AUDIT DATA - Logistic Regression ---- ######
# url - https://archive.ics.uci.edu/ml/datasets/Audit+Data

# Abstract: Exhaustive one year non-confidential data in the year 2015 to 2016
# of firms is collected from the Auditor Office of India to build a predictor for 
# classifying suspicious firms.

require(tidyverse)
require(caret)
require(skimr)
#install.packages('mice',dependencies = T)
require(mice)
#install.packages('dummies',dependencies = T)
require(dummies)

file_path <- 'C:/Users/abu/Desktop/Machine Learning/Abhirami\'s Projects/Logistic Regression/audit_data/audit_risk.csv'
raw_data <- read.csv(file_path,header = T, sep = ',')
glimpse(raw_data)
data1 <- raw_data
glimpse(data1)

######### -- DATA Pre - processing ----- ######

#convert response var into factor
data1$Risk <- as.factor(data1$Risk)
glimpse(data1)

#check for NAs
dev.off()
skim(data1)

#remove rows with NAs
#na_index <- raw_data[is.na(raw_data$Money_Value),]
#data1 <- raw_data[-na_index,]

md.pattern(data1)
data1 <- na.omit(data1)

colSums(is.na(data1))
complete.cases(data1) %>% table()

glimpse(data1)


# -- OHE of CATEGORICAL VARIABLES -- #
# Location_ID is nominal factor hence do OHE
# ordinal factors can be recoded with numbers

#create df for location_id using OHE
location_id_ohe <- dummy(data1$LOCATION_ID) #for ohe of single var 
#ignore warning of non-list contrasts
# for full df --> Data_DF_OHE <- dummy.data.frame(Data_DF)

location_id_ohe <- as.data.frame(location_id_ohe) # convert to df 

#remove location_id and add location_id_ohe to data1 using cbind

glimpse(data1)
data1 <- data1[,-2]
data1 <- cbind(data1,location_id_ohe)

md.pattern(data1)

# look if its balanced data

table(data1$Risk)
unique(data1$Detection_Risk)

# lets chk cols where sd is zero before standardizing

apply(data1,2,sd)

# we see that Detection_Risk has SD zero, so lets remove it

index1 <- which(colnames(data1) == 'Detection_Risk')
data1 <- data1[,-index1]
# center and scale the data -- good practise so as to understand and explain beta coeeff better

index <- which(colnames(data1) == 'Risk')
data1_std <- scale(data1[,-index], center = T, scale = T) %>% as.data.frame()

#add back the response var which is not standardized
data1_std <- cbind(data1$Risk,data1_std)
colnames(data1_std)[colnames(data1_std) == "data1$Risk"] <- "Risk" #rename colname
glimpse(data1_std)


# After running model, no variables were significant, hence lets try PCA
# to reduce num of features

pca_model <- stats::prcomp(data1_std[,-1])

summary(pca_model)

screeplot(pca_model)
screeplot(pca_model,type='l',npcs = 69)

pca_model_df <- data.frame(pca_model$x[,1:39],Risk = as.character(data1$Risk))
glimpse(pca_model_df)

data2 <- pca_model_df




### ------ APPLY the Logistic Regression ALgorithm MODEL ----- ####
# split data into train and test

set.seed(3733)

train_idx <- createDataPartition(data2$Risk,p = 0.8, list = FALSE)
train_idx 

train_data <- data2[train_idx,]
train_data %>% nrow()


test_data <- data2[-train_idx,]
test_data %>% nrow()



# LOGISTIC REGRESSION

# glm() is used to compute logistic regression and family = binomial is used 
# to fit logistic regression.

logit_model <- glm(Risk ~ .,data = train_data,family = 'binomial')

summary(logit_model)


#AIC was 50, no var significant
# AFter PCA, AIC is 945, all var significant

## NONE of the variables are significant. This raises some RED FLAGS.
## AFter PCA all var are significant

coef(logit_model)

summary(logit_model)$coef

logit_model$fitted.values


# Make Predictions
index2 <- which(colnames(test_data) == 'Risk')
pred_values <- predict(logit_model,test_data[,-index2], type = 'response')
pred_values

# which class is which ?
contrasts(test_data$Risk)


# define threshold

y_pred_num <- ifelse(pred_values > 0.5 , 1, 0)
y_pred <- factor(y_pred_num,levels = c(0,1))


y_act <- test_data$Risk

# accuracy 

mean(y_pred == y_act) #94% accuracy
# after PCA, accuracy is 92%



# CONFUSION MATRIX
require(e1071)

table(test_data$Risk)
table(y_pred)

# num of cases
table(test_data$Risk,y_pred)

#---
# all metrics
caret::confusionMatrix(data = y_pred,reference = test_data$Risk,positive = '1')

# before PCA accuracy is 94,  kappa is 87.87%, AIC < 100
# after PCA, accuracy is 93, kappa is 85 %, AIC = 945


# so model after pca is better than model before pca as variables significant,
# has a good accuracy and kappa score.

# AIC of 2 model cant compared as underlying data is different.

# NEXT STEPS --> See if the model perform well using Decision Trees and Random Forest.
#--------------
# AUC and ROC
# ROC curves are insensitive to class imbalance

require(pROC)

log_reg_roc <- roc(test_data$Risk,pred_values)
dev.off()
plot.roc(log_reg_roc, print.auc = TRUE)


# Extract some interesting results

log_reg_roc_data <- data.frame(
  thresholds = log_reg_roc$thresholds,
  sensitivity = log_reg_roc$sensitivities,
  specificity = log_reg_roc$specificities)

# Get the probability of threshold for specificity = 0.8
log_reg_roc_data %>% filter(specificity >= 0.8)



# The best threshold with the highest sum sensitivity + specificity can be 
# printed as follow. 
# There might be more than one threshold.
dev.off()

plot.roc(log_reg_roc, print.auc = TRUE, print.thres = "best")



