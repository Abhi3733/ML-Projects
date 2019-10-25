##  Airfoil Self-Noise Data Set -- Multivariate Regression ##

## url --> https://archive.ics.uci.edu/ml/datasets/Airfoil+Self-Noise#

## Dataset Info - 1503 rows, 6 cols
#   
# The NASA data set comprises different size NACA 0012 airfoils at
# various wind tunnel speeds and angles of attack. The span of the 
# airfoil and the observer position were the same in all of the 
# experiments.
# 
# 
# Attribute Information:
#   
#   This problem has the following inputs: 
#   1. Frequency, in Hertzs. 
# 2. Angle of attack, in degrees. 
# 3. Chord length, in meters. 
# 4. Free-stream velocity, in meters per second. 
# 5. Suction side displacement thickness, in meters. 
# 
# The only output is: 
#   6. Scaled sound pressure level, in decibels. 
#-----------------------------------------------------------------
require(dplyr)
require(ggplot2)
require(ggcorrplot)
require(caret)
require(mice)

file_path <- "H:/DATA SCIENCE/DS & ML/Abhirami's Projects/Regression/Airfoil self_noise dataset/airfoil_self_noise.dat"
raw_data <- read.delim(file_path,header = F,sep = '\t')

raw_data %>% glimpse()

col_labels <- c("freq","angle_of_attack","chord_length",
                "free_stream_velocity","suction_thickness",
                "sound_level")

colnames(raw_data) <- col_labels



## data preprocessing

colSums(is.na(raw_data))
complete.cases(raw_data) %>% length()

table(raw_data$freq)
table(raw_data$angle_of_attack)
table(raw_data$chord_length)
table(raw_data$free_stream_velocity)
table(raw_data$suction_thickness)
table(raw_data$sound_level)

md.pattern(raw_data)

raw_data %>% summary()

# boxplot of variables
ggplot(data = raw_data, mapping = aes()) + geom_boxplot()
boxplot(raw_data)

# standardize data set

data1 <- raw_data[,-6]
data1 %>% glimpse()

raw_data_std <- scale(raw_data, scale = TRUE, center = T) %>%
            as.data.frame()

## multivariate linear reg

# split into train and test set

airfoil_train_idx <- createDataPartition(y = raw_data_std$sound_level,
                                         p = 0.8, 
                                         list = FALSE)
airfoil_train <- raw_data_std[airfoil_train_idx,]
airfoil_train %>% glimpse()

airfoil_test <- raw_data[-airfoil_train_idx,]
airfoil_test %>% glimpse()

# train the model

model1 <- lm(sound_level ~ ., data = airfoil_train)
summary(model1)

# test the model

airfoil_test$sound_level <- predict(model1,airfoil_test[,-6])
airfoil_test %>% head()

# Model's goodness
sd(raw_data$)


# ----- improve model by removing outliers ------ #

# calculate IQR using fivenum() function
# Returns Tukey's five number summary
#(minimum, lower-hinge, median, upper-hinge, maximum) for the input data.

freq_5num <- fivenum(raw_data$freq)
outlier_freq  <- 1.5*(freq_5num[4] - freq_5num[2])

which(raw_data$freq >= outlier_freq) %>% length()
# 322 outliers


angle_5num <- fivenum(raw_data$angle_of_attack)
outlier_angle  <- 1.5*(angle_5num[4] - angle_5num[2])

which(raw_data$angle_of_attack >= outlier_angle) %>% length()
# 346 outliers


chord_5num <- fivenum(raw_data$chord_length)
outlier_chord  <- 1.5*(chord_5num[4] - chord_5num[2])

which(raw_data$chord_length >= outlier_chord) %>% length()
# 188


stream_5num <- fivenum(raw_data$free_stream_velocity)
outlier_stream  <- 1.5*(stream_5num[4] - stream_5num[2])

which(raw_data$free_stream_velocity >= outlier_stream) %>% length()
# 742


suction_5num <- fivenum(raw_data$suction_thickness)
outlier_suction  <- 1.5*(suction_5num[4] - suction_5num[2])

which(raw_data$suction_thickness >= outlier_suction) %>% length()
#297

sound_5num <- fivenum(raw_data$sound_level)
outlier_sound  <- 1.5*(sound_5num[4] - sound_5num[2])

which(raw_data$sound_level >= outlier_sound) %>% length()
# 1503


## ---- extreme outliers - 3(IQR) -------- #


freq_5num <- fivenum(raw_data$freq)
outlier_freq  <- 3*(freq_5num[4] - freq_5num[2])

which(raw_data$freq >= outlier_freq) %>% length()
# 86 outliers


angle_5num <- fivenum(raw_data$angle_of_attack)
outlier_angle  <- 3*(angle_5num[4] - angle_5num[2])

which(raw_data$angle_of_attack >= outlier_angle) %>% length()
# 0 outliers


chord_5num <- fivenum(raw_data$chord_length)
outlier_chord  <- 3*(chord_5num[4] - chord_5num[2])

which(raw_data$chord_length >= outlier_chord) %>% length()
# 0


stream_5num <- fivenum(raw_data$free_stream_velocity)
outlier_stream  <- 3*(stream_5num[4] - stream_5num[2])

which(raw_data$free_stream_velocity >= outlier_stream) %>% length()
# 0


suction_5num <- fivenum(raw_data$suction_thickness)
outlier_suction  <- 3*(suction_5num[4] - suction_5num[2])

which(raw_data$suction_thickness >= outlier_suction) %>% length()
#94

#sound_5num <- fivenum(raw_data$sound_level)
#outlier_sound  <- 3*(sound_5num[4] - sound_5num[2])

#which(raw_data$sound_level >= outlier_sound) %>% length()
# 1503

