#Load Libraries: p_load can install, load,  and update packages

if(require("pacman")=="FALSE"){
  install.packages("pacman")
} 

pacman::p_load(readr, tidyverse, caret, doParallel,reshape2, ggplot2, tidyr, dplyr, lubridate)


# Load data

train <- read.csv("preprocessedTrainingAnomaliesRemoved.csv")
valid <- read.csv("preprocessedValidation.csv")


# Data set dimensions

dim(train)  

dim(valid) 



# Combine datasets for speeding-up the transformations and add row ID


df <- gdata::combine(train, valid)



# New WAPS

WAPS <- grep("WAP", names(df), value=T)


# Create new variables HighWAP and HighRSSI


df <- df %>% mutate(HighWAP=NA, HighRSSI=NA)

df <- df %>%  mutate(HighWAP=colnames(df[WAPS])[apply(df[WAPS],1,which.max)])

df <- df %>% mutate(HighRSSI=apply(df[WAPS], 1, max))

df$HighWAP <- as.factor(df$HighWAP)


# Checking for same HighWAP in different Buildings


WAPS_R <- df %>%
  select(HighWAP, BUILDINGID, source) %>%
  distinct(HighWAP, BUILDINGID, source)


RepeatedWAPS <- WAPS_R %>% distinct(HighWAP, BUILDINGID)

RepeatedWAPS <- sort(RepeatedWAPS$HighWAP[duplicated(RepeatedWAPS$HighWAP)]) 

RepeatedWAPS 


# Examine and remove WAP 248

WAP248 <- df[df$HighWAP=="WAP248",465:474]

plot(LATITUDE ~ LONGITUDE, data = df, pch = 20, col = "grey")
points(LATITUDE ~ LONGITUDE, data=WAP248[WAP248$source=="train",], pch=20, col="blue")
points(LATITUDE ~ LONGITUDE, data=WAP248[WAP248$source=="valid",], pch=20, col="red")

df$WAP248 <- NULL

# Examine and remove WAP 001

WAP001 <- df[df$HighWAP=="WAP001",465:474]

plot(LATITUDE ~ LONGITUDE, data = df, pch = 20, col = "grey")
points(LATITUDE ~ LONGITUDE, data=WAP001[WAP001$source=="train",], pch=20, col="blue")
points(LATITUDE ~ LONGITUDE, data=WAP001[WAP001$source=="valid",], pch=20, col="red")

df$WAP001 <- NULL


# Split and save Data to csv

dfSplit <- split(df, df$source)

list2env(dfSplit, envir=.GlobalEnv)

rm(dfSplit)
