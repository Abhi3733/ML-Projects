# 
# Dresses_Attribute_Sales Data Set 

# Abstract: This dataset contain Attributes of dresses and their 
# recommendations according to their sales.Sales are monitor on the
# basis of alternate days.


#load required packages

require(tidyverse)
require(readxl)
require(mice)
#install.packages('randomForest')
require(randomForest)
require(dummies)

require(cluster)
require(factoextra)
require(dendextend)
require(fpc)
table(data_raw$Recommendation)

#url <- "https://archive.ics.uci.edu/ml/datasets/Dresses_Attribute_Sales"

file_of_path <- "H:/DATA SCIENCE/DS & ML/Abhirami's Projects/Clustering/Hierarchial clustering/Attribute DataSet.xlsx"
data_raw <- read_excel(file_of_path,col_names  = TRUE)
data_raw %>% glimpse()

data1 <- data1 %>% rename("pattern_type" = `Pattern Type`)
# remove response var
#remove dress_id as its unique and doesnt contribute to clustering algorithm
data1 <- data_raw[,-c(1,14)]
data1 %>% glimpse()


# Data Pre-processing

# See data distribution and chk for null values
table(data1$Style)
table(data1$Price)
table(data1$Rating)
table(data1$Size)
table(data1$Season)
table(data1$NeckLine)
data1$NeckLine <- ifelse(data1$NeckLine == "NULL",NA,data1$NeckLine)

table(data1$SleeveLength)
data1$SleeveLength <- ifelse(data1$SleeveLength == "NULL",NA,data1$SleeveLength)
data1$SleeveLength <- ifelse(data1$SleeveLength %in% c("sleeevless","sleeveless","sleveless"),"sleevless",data1$SleeveLength)

table(data1$waiseline) 
data1$waiseline <- ifelse(data1$waiseline == "null",NA,data1$waiseline)

table(data1$Material) 
data1$Material <- ifelse(data1$Material == "null",NA,data1$Material)

table(data1$FabricType) 
data1$FabricType <- ifelse(data1$FabricType == "null",NA,data1$FabricType)

table(data1$Decoration) 
data1$Decoration <- ifelse(data1$Decoration == "null",NA,data1$Decoration)

table(data1$pattern_type) 
data1$pattern_type <- ifelse(data1$pattern_type == "null",NA,data1$pattern_type)


# 1. Check for NA Values 

colSums(is.na(data1))
sum(is.na(data1)) 

# Missing values need to be recoded to NA
# FInd number of instances the values in  a column is 0 

sum(data1$Rating == 0 , na.rm = T)
data1$Rating <- ifelse(data1$Rating==0,NA,data1$Rating)


# Analysis of the NA or missing values at row level 
sum(complete.cases(data1))

md.pattern(data1)


## Find the Data type of columns having NA Values

cols_with_na <- which(colSums(is.na(data1)) > 0)
apply(data1[,cols_with_na], 2, class)
data1 %>% glimpse()

## As the columns are of character data type , we will use 'Mode Imputation'
# we need to create a function to return mode of categorical values

#mode1 <- function(x){
#  ux <- unique(x)
#  tab <- tabulate(match(x,ux))
#  ux[tab==max(tab)]
#}

#mode_vals <- apply(data1[,cols_with_na], 2 , mode1)
#a <- c("a","b","c","c","b")
#ux <- unique(a)
#tab <- tabulate(match(a,ux))
#ux[tab==max(tab)]

# mode vals has NA


#########################################

# for imputation to work, the character should be of type factor i.e nominal data

data1$NeckLine <- as.factor(data1$NeckLine)
data1$Style <- as.factor(data1$Style)
data1$Price <- as.factor(data1$Price)
data1$Size <- as.factor(data1$Size)
data1$Season <- as.factor(data1$Season)
data1$SleeveLength <- as.factor(data1$SleeveLength)
data1$waiseline <- as.factor(data1$waiseline)
data1$Material <- as.factor(data1$Material)
data1$FabricType <- as.factor(data1$FabricType)
data1$Decoration <- as.factor(data1$Decoration)
data1$pattern_type <- as.factor(data1$pattern_type)


#decide which cols to drop
colSums(is.na(data1))
data1 %>% glimpse()
# drop FabricType and Decoration as more than 236 NAs are present i.e almost half
data_v2 <- data1[,-c(10,11)]
data_v2 %>% glimpse()

colSums(is.na(data_v2))

# impute data for all cols
data_v2_imptd <- data_v2

data_v2_imputed <- mice(data_v2_imptd,method ='pmm',seed=500)
summary(data_v2_imputed)


data_v2_full <- complete(data_v2_imputed,1)
glimpse(data_v2_full)

md.pattern(data_v2_full)
data_v2_full %>% str()

#scale and center the Rating column which ranges from 0 to 5
data_v2_full$Rating <- scale(data_v2_full$Rating,center = TRUE,scale = TRUE)


# OHE for categorical variables

data_v2_full_ohe <- dummy.data.frame(data_v2_full)

#center and scale the whole dataset ater OHE

data_v2_ready <- scale(data_v2_full_ohe,center = TRUE, scale =TRUE)
data_v2_ready %>% summary()

#--------END OF DATA PREPROCESSING -------------------------#

## hclust() from stats package for Agglomerative Clustering
## Agnes good for small clusters

#---------------------------------------------------

# Create distance matrix
data_v2_ready_matrix <- stats::dist(data_v2_ready)

# create bottom up clusters
hclust_agnes <- stats::hclust(data_v2_ready_matrix,method = 'complete')

# plot dendogram
dev.off()
plot(hclust_agnes)
plot(hclust_agnes, hang = -1)


# Plot coloured Dendogram using dendextend 
dendo_clust <- as.dendrogram(hclust_agnes)

# using Height criteria for splitting 
dendo_clust_1 <- color_branches(dendo_clust, h = 3)
plot(dendo_clust_1)

#using K criteria for splitting
dendo_clust_2 <- color_branches(dendo_clust, k = 2)
plot(dendo_clust_2)


# The DEndogram can be cut at the desired level by using cutree()

HClust_Cut <- stats::cutree(hclust_agnes , 2)
stats::cutree(Hrchl_Clstr , h=4)
table(HClust_Cut) # clustering into groups not good
# CONFUSION MATRIX / CONTINGENCY TABLE

table(data_raw$Recommendation,HClust_Cut)

#----------------------KMeans-------------

kmean_clust_2 <- kmeans(data_v2_ready, centers = 2, nstart = 25)
kmean_clust_2

kmean_clust_2$size # 145 and 355

# algorithmic & mathematical approach to find 'Optimum K'
#-----------------------------------------------------------------------

# 1. Elbow method/Scree plot to find Optimum K

set.seed(10)

fviz_nbclust(data_v2_ready,kmeans,method = 'wss')
# no elbow point found

# 2. Silhouette analysis

K_Value_Sil <- vector(mode='numeric')

Sil_Width <- vector(mode='numeric')

for ( i in (1:9)){
  
  K_Value_Sil[i] <- i+1
  
  Sil_Width_Model <- cluster::pam(data_v2_ready, k=i+1)
  
  Sil_Width[i] <- Sil_Width_Model$silinfo$avg.width
  
}

K_Value_Sil
Sil_Width

Sil_width_DF <- data.frame(K_Value_Sil , Sil_Width)

Sil_width_DF


ggplot(Sil_width_DF,aes(x=K_Value_Sil, y=Sil_Width)) + 
  geom_line() +
  scale_x_continuous(breaks = 2:10)


table(data_raw$Recommendation,kmean_clust_2$cluster)


# CONCLUSION --> Kmeans seems to have better results than agnes.



