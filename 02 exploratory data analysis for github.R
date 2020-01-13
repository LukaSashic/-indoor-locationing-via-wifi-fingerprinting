# Load libraries

library(readr)
library(dplyr)
library(tidyr)
library(h2o)
library(ggplot2)
library(reshape2)
library(plotly)
library(RColorBrewer)
library(tidyverse)
library(caret)

# Load data

train <- read.csv("preprocessedTraining.csv")
valid <- read.csv("preprocessedValidation.csv")

wifi <- rbind(train, valid)


dim(train)

dim(valid)

dim(wifi)



# Signal Anomaly Analysis on Training Dataset -30 to 0 dBm 


WifiOutData <- train
WAPSout <- apply(WifiOutData %>% select(starts_with("WAP")),1, max) > -30
sum(WAPSout)  # 492 outliers

# Exploring which USERID is receiving bad signals

WAPSout30 <- train[WAPSout,] 

WAPSout30 %>% 
  group_by(BUILDINGID, FLOOR, USERID, PHONEID) %>% 
  count(USERID) # UserID 6


# Visualize USER 6

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



# Remove rows with signal anomalies from the training data

train <- train[!WAPSout,]

dim(train) # 18808 475 Rows with signal anomalies excluded (492)

# Save to csv 

write.csv(train, file = "preprocessedTraining_anomaliesRemoved.csv", row.names=FALSE, col.names = FALSE)




