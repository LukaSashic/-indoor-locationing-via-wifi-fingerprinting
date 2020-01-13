#Load libraries
source("00_libraries.R")

#Load Data
train <- read.csv("trainingData.csv")
valid <- read.csv("validationData.csv")

# DATA CLEANING

# Separate training data into waps and info data frames


waps <- train %>% select(starts_with("WAP"))
info <- train %>% select(-starts_with("WAP"))

info_val <- valid  %>% select(-starts_with("WAP"))

#INITIAL DATA EXPLORATION

# Data dimension, summary and structure 

dim(train)  # 19937 529
dim(valid)  # 1111  529    we have same number of variables in training and validation data set

summary(info)
str(info)

summary(info_val)  # SPACEID, RELATIVEPOSITION and USERID have all zero values, we should remove them before training the models
str(info_val)

names(train)

head(train, 5)[1:8]

str(train, list.len=3)

glimpse(train)

# Check for missing values


sum(is.na(train)) # no missing values
sum(is.na(valid)) # no missing values


# Check for duplicates

count(distinct(train))   # from 19937 rows 19300 are distict, we have 637 duplicated rows 
count(distinct(valid))   # no duplicated rows in validation data set

# use only distinct rows, remove duplicated rows

train <- distinct(train)

dim(train)  # 19300 529 duplicated rows removed

# Transform some variables to factor/numeric/datetime


factors <- c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID")
train[,factors] <-lapply(train[,factors], as.factor)
valid[,factors] <-lapply(valid[,factors], as.factor)

rm(factors)

numeric <- c("LONGITUDE", "LATITUDE")
train[,numeric]<-lapply(train[,numeric], as.numeric)
valid[,numeric]<-lapply(valid[,numeric], as.numeric)

rm(numeric)

train$TIMESTAMP <- NULL
valid$TIMESTAMP <- NULL

# Change value of WAPS= 100 to WAPS= -105

no_signal <- grep("WAP", names(train), value=T)
train[,no_signal] <- sapply(train[,no_signal],function(x) ifelse(x==100,-105,x))
valid[,no_signal] <- sapply(valid[,no_signal],function(x) ifelse(x==100,-105,x))



# Check for Zero Variance columns


which(apply(train, 2, var) == 0)

train <- train[ - as.numeric(which(apply(train, 2, var) == 0))]


dim(train)               # 19300 473

valid <- valid %>% intersect()


#Move the info to the front and gather the data

train_gathered <- train[ , c((ncol(train)-7):(ncol(train)), 1:(ncol(train)-8))]
train_gathered <- gather(train_gathered, WAP, RSSI, 9:ncol(train_gathered))

head(train_gathered)

# Merge training and validation data sets

common_cols <- intersect(colnames(train), colnames(valid))

combinedSets <- rbind(train[common_cols], valid[common_cols])


dim(combinedSets)



#CHECKING FOR PROBLEMS OF DATA VALUES > -30

# Creates a vector to store indexes of rows that contain values > -30


index_above_30 <-  as.data.frame(which(combinedSets[, 1:(ncol(combinedSets) - 8)] > -30, arr.ind = TRUE))
index_above_30 <- unique(index_above_30$row)
length(index_above_30)



#Creates a dataframe with the rows containing values > -30
data_above_30  <- combinedSets[index_above_30, ]



badwaps <- colnames(data_above_30[1:(ncol(combinedSets) - 8)])[apply(data_above_30[1:(ncol(combinedSets) - 8)], 1, which.max)]


#Count the the number of times WAPs have values > -30
length(unique(badwaps)) # # there are 55 bad WAPS
sort(table(badwaps), decreasing = TRUE)


#Count how many values > -30 each user has
count(data_above_30, USERID, BUILDINGID, FLOOR)  # User #6 has 430 and user #14 has 51 occasions where the value is above -30

# count how many observations user 6 has
user6 <- combinedSets %>% 
  filter(USERID == 6)

nrow(user6) # 977 observations from user 6

# count how many observations for  building 2 and  floor 4

filteredB2F3 <- filter(train, BUILDINGID==2 & FLOOR==4) 

nrow(filteredB2F3) # 727 observations for 4th floor of the building 2

count(filteredB2F3, USERID) #but most of them are from user 6, if we remove User 6, we will lose almost all information about floor 4



