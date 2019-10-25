###--------------------------------------------------------------###
### seeds Datasets - Clustering Analysis using K-means Algorithm
### Author - Abhirami A
### Data link - " https://archive.ics.uci.edu/ml/datasets/seeds "

# Info - The examined group comprised kernels belonging to three different
# varieties of wheat: Kama, Rosa and Canadian, 70 elements each, randomly 
# selected for the experiment.
###--------------------------------------------------------------###

# load required packages

require(cluster)
require(mice)
require(tidyverse)
require(gridExtra)
require(factoextra)

# txt data has some tab related issues and gives 221 rows and 8 cols
# hence use csv file to overcome those issues and get 210 rows and 7 cols

data_path <- "H:/DATA SCIENCE/DS & ML/Abhirami's Projects/k-mean Clustering/seeds_dataset_UCI.csv"
seeds_data <- read.csv(data_path,header = FALSE)
seeds_data %>% glimpse()
seeds_data$V8 %>% unique()

#assign header names
header_names <- c('Area','Perimeter','Compactness','length_kernel',
                  'width_kernel','asymmtery_coeff','length_groove','Type')

colnames(seeds_data) <- header_names

#remove the response variable
seeds_df <- seeds_data[,-8]
seeds_df %>% glimpse()

# Data Pre-processing steps -

# 1. Check for missing values and NAs.

md.pattern(seeds_df,plot = TRUE)

colSums(is.na(seeds_df))

# 2. Impute values if necessary.
# 3. Convert categorical into numeric data using OHE
# 3. Data Standardization.

summary(seeds_df_std)
seeds_df_std <- scale(seeds_df, center = T, scale = T)

#---------------------------------------------------------
# calculate distance matrix

seeds_df_std_dis <- factoextra::get_dist(seeds_df_std, method = 
                                           'euclidean')
class(seeds_df_std_dis)

# to view distance matrix, let convert it into a matrix
seeds_df_std_dis_mtx <- as.matrix(seeds_df_std_dis)
seeds_df_std_dis_mtx %>% head()
seeds_df_std_dis_mtx[1:20,1:3]

#visualize distance matrix
dev.off()
fviz_dist(seeds_df_std_dis)

# We can set the specific colour code for representing Distance
fviz_dist(seeds_df_std_dis ,
          gradient = list( low = 'red' ,
                           mid ='white',
                           high = 'green'))
#pairwise scatterplots
plot(seeds_df,main="Pairwise scatterplots of the data")

#-------------------------------------------------------
# Clustering the observations using 'K-Means' algorithm
#-------------------------------------------------------

kmeans2 <- kmeans(seeds_df, centers = 2, nstart = 25)
kmeans2

# View the Clusters identified by the K-Means algorithm
kmeans2$cluster

# Get the 2 cluster centers 
kmeans2$centers

#size of 2 clusters
kmeans2$size

kmeans2$withinss #smaller the better
kmeans2$betweenss #larger the better
kmeans2$tot.withinss #smaller the better

# 'totss' : Sum of 'tot.withinss'  & 'betweenss'
kmeans2$totss

kmeans2$betweenss/kmeans2$totss #ideal value is 1

# Ideally we want a clustering that has the properties of
# internal cohesion and external separation,
# i.e. the BSS/TSS ratio should approach 1.

#-------------------------------------------------------------
#  Lets Visualize the Clustering arrived at by kmeans()
#---------------------------------------------------------

dev.off()

#graph 1
factoextra::fviz_cluster(kmeans2,seeds_df_std)

#graph2
dev.off()
cluster::clusplot(seeds_df_std,kmeans2$cluster,color = T, shade = T)

#----------------------------------------------------------
# Lets find the optimum value of 'k'
#--------------------------------------------------------

# Manual approach - brute force method

kmeans3 <- kmeans(seeds_df_std, centers = 3, nstart = 25 )
kmeans4 <- kmeans(seeds_df_std, centers = 4, nstart = 25 )
kmeans5 <- kmeans(seeds_df_std, centers = 5, nstart = 25 )
kmeans6 <- kmeans(seeds_df_std, centers = 6, nstart = 25 )

plot2 <- fviz_cluster(kmeans2,seeds_df_std,geom = 'point') + ggtitle('K=2')
plot3 <- fviz_cluster(kmeans3,seeds_df_std,geom = 'point') + ggtitle('K=3')
plot4 <- fviz_cluster(kmeans4,seeds_df_std,geom = 'point') + ggtitle('K=4')
plot5 <- fviz_cluster(kmeans5,seeds_df_std,geom = 'point') + ggtitle('K=5')
plot6 <- fviz_cluster(kmeans6,seeds_df_std,geom = 'point') + ggtitle('K=6')

grid.arrange(plot2, plot3,plot4,plot5,plot6,nrow = 2)


# loop method for the above - 


K_Mean_List_Clstr_Out = list()
Plot_K_list = list()

for ( n in 1 : 4) {
  
  K_Mean_List_Clstr_Out[[n]] <- kmeans(seeds_df_std ,
                                       centers = n+1,
                                       nstart = 25 )
  
  Plot_K_list[[n]] <- fviz_cluster(K_Mean_List_Clstr_Out[[n]] , 
                                   data = seeds_df_std,
                                   geom = 'point') + 
    ggtitle(paste("K :- ", n+1))
  
}


grid.arrange(Plot_K_list[[1]] ,
             Plot_K_list[[2]] ,
             Plot_K_list[[3]] ,
             Plot_K_list[[4]] ,
             nrow = 2)
#--------------------------------------------------------------------------
# algorithmic & mathematical approach to find 'Optimum K'
#-----------------------------------------------------------------------

# 1. Elbow method/Scree plot to find Optimum K

set.seed(10)

fviz_nbclust(seeds_df_std,kmeans,method = 'wss')

# from the scree plot, 2 seems ideal k

# 2. Silhouette Analysis

#for k=2
sil_seeds_2 <- cluster::silhouette(kmeans2$cluster,stats::dist(seeds_df_std))

dev.off()
plot(sil_seeds_2,main='SIlhouette analysis, k = 2')

# for 2, there r negative values, so k=2 is not optimal

#for k=3
sil_seeds_3 <- silhouette(kmeans3$cluster,dist(seeds_df_std))
dev.off()

plot(sil_seeds_3,main = 'Silhouette analysis, k = 3')

# Therefore, 3 is ideal K Value

#for k=4
sil_seeds_4 <- silhouette(kmeans4$cluster,dist(seeds_df_std))
dev.off()

plot(sil_seeds_4,main = 'Silhouette analysis, k = 4')




#------------------------
# ## Plotting the silhouette width
# SIl width, nearer to 1, the better

K_Value_Sil <- vector(mode='numeric')

Sil_Width <- vector(mode='numeric')

for ( i in (1:9)){
  
  K_Value_Sil[i] <- i+1
  
  Sil_Width_Model <- cluster::pam(seeds_df_std, k=i+1)
  
  Sil_Width[i] <- Sil_Width_Model$silinfo$avg.width
  
}

K_Value_Sil
Sil_Width

Sil_width_DF <- data.frame(K_Value_Sil , Sil_Width)

Sil_width_DF


ggplot(Sil_width_DF,aes(x=K_Value_Sil, y=Sil_Width)) + 
  geom_line() +
  scale_x_continuous(breaks = 2:10)

# CONCLUSION - Though optimal K value is 2 according to algorithm,
# only for this case we will for k=3, as we know the num
# of groups in the data is 3.

# Compare cluster with original data - CONFUSION MATRIX/
#CONTINGENCY TABLE

table(seeds_data$Type)
kmeans3$size
kmeans3$cluster

table(seeds_data$Type,kmeans3$cluster)

# From confusion matrix, we oberve that -
# 66 were correctly gropued in type1
# 62 were correctly grouped in type2
# 65 were correctly grouped in type3
