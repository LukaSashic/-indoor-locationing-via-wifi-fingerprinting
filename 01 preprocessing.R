# Load libraries

library(readr)
library(dplyr)
library(tidyr)
library(h2o)
library(ggplot2)
library(reshape2)
library(plotly)
library(RColorBrewer)

# Load data

train <- read.csv("trainingData.csv")
valid <- read.csv("validationData.csv")
wifi <- rbind(train, valid)



# Initial exploration

dim(train)  # 19937 529
dim(valid)  # 1111  529    we have same number of variables in training and validation data set

summary(train[,520:529])
str(train[,520:529])


summary(valid[,520:529])
str(valid[,520:529])

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

dim(train)

# change structure according to attribute information in the resources in training dataset:

train$LONGITUDE <- as.numeric(train$LONGITUDE)
train$LATITUDE <- as.numeric(train$LATITUDE)
train$FLOOR <- as.integer(train$FLOOR)
train$BUILDINGID <- as.integer(train$BUILDINGID)
train$SPACEID <- as.integer(train$SPACEID)
train$RELATIVEPOSITION <- as.integer(train$RELATIVEPOSITION)
train$USERID <- as.integer(train$USERID)
train$PHONEID <- as.integer(train$PHONEID)
train$TIMESTAMP <- as.POSIXct(train$TIMESTAMP, '1970-01-01', tz = 'GMT')

# do the same for the validation data set
valid$LONGITUDE <- as.numeric(valid$LONGITUDE)
valid$LATITUDE <- as.numeric(valid$LATITUDE)
valid$FLOOR <- as.integer(valid$FLOOR)
valid$BUILDINGID <- as.integer(valid$BUILDINGID)
valid$SPACEID <- as.integer(valid$SPACEID)
valid$RELATIVEPOSITION <- as.integer(valid$RELATIVEPOSITION)
valid$USERID <- as.integer(valid$USERID)
valid$PHONEID <- as.integer(valid$PHONEID)
valid$TIMESTAMP <- as.POSIXct(valid$TIMESTAMP, origin = '1970-01-01', tz = 'GMT')


# check for zero variance and remove columns with zero variance from train


rmZVtrain <- train[ -which(apply(train, 2, var) == 0 )] 

which(apply(rmZVtrain, 2, var) == 0)


dim(rmZVtrain) # after removing zero variance rows and columns we have 19227 rows and 472 columns

train <- rmZVtrain

# Convert all 100 (i.e. not detected) to -105

train[train == 100] <- -105
valid[valid == 100] <- -105


#Move the info to the front and gather the data

trainingData_Gathered <- train[ , c((ncol(train)-8):(ncol(train)), 1:(ncol(train)-9))]
trainingData_Gathered <- gather(trainingData_Gathered, WAP, RSSI, 10:ncol(trainingData_Gathered))

trainingData_Gathered %>% filter(RSSI>=-30)

# Filter good signal streght

trainingData_Gathered <- filter(trainingData_Gathered, RSSI <= -30 & RSSI >= -70)

trainingData_Gathered %>% 
  group_by(BUILDINGID, FLOOR) %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE, color = BUILDINGID)) +
  geom_point() +
  labs(title = "Training Data Longitude vs Latitude")



# Basic visualisations

# Longitude vs Latitude
train %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE, color = BUILDINGID)) +
  geom_point() +
  labs(title = "Training Data Longitude vs Latitude")


# Building 0 Preview


train %>% 
  filter(BUILDINGID == 0) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(FLOOR), 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building 0 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))


# Building 1 Preview
train %>% 
  filter(BUILDINGID == 1) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(FLOOR), 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building 1 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))


# Building 2 Preview
train %>% 
  filter(BUILDINGID == 2) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(FLOOR), 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building 1 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

# Plot relative position


train$RELATIVEPOSITION <- as.factor(train$RELATIVEPOSITION)
ggplot(train, aes(x=LONGITUDE, y=LATITUDE, color = RELATIVEPOSITION))+
  geom_point() + 
  theme_light() +
  labs(title="Measurements INSIDE/OUTSIDE",
       subtitle = "Divided by RELATIVE POSITION")

# 3D plot
plot_ly(train, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR) %>%
  add_markers(color = ~RELATIVEPOSITION) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))

# visualizing the data FLOOR vs PHONEID
train$PHONEID <- as.factor(train$PHONEID)
ggplot(train, aes(x=LONGITUDE, y = LATITUDE, color = PHONEID)) +
  geom_jitter() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="Measurements made by PhoneID",
       subtitle = "seperated by floor")

## -30 to 0 dBm Analysis
# Outliers on Train
WifiOutData <- train
WifiOutData[WifiOutData == 100] <- -105
WAPSout <- apply(WifiOutData %>% select(starts_with("WAP")),1, max) > -30
sum(WAPSout)
WAPSout30 <- train[WAPSout,] # 492 outliers

WAPSout30 %>% 
  group_by(BUILDINGID, FLOOR, USERID, PHONEID) %>% 
  count(USERID) # UserID 6


train %>% 
  filter(USERID == 6) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(USERID)) %>%
  add_markers() %>%
  layout(title = "User 6 Behavior",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))


# gather all WAPS in one column
GatherWAPS <- train %>% gather(key = 'ALLWAPS', value = 'VALUE',  1:465)


ggplot(GatherWAPS, aes(x=ALLWAPS, y=VALUE, color = BUILDINGID))+
  geom_point() + 
  theme_light() +
  labs(title="Measurements INSIDE/OUTSIDE",
       subtitle = "Divided by RELATIVE POSITION")
