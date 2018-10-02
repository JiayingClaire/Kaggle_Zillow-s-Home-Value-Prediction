# Zillow’s Home Value Prediction (Zestimate) project is one of the Featured Prediction Competition in Kaggle.
# Project description: https://www.kaggle.com/c/zillow-prize-1
# Data details: https://www.kaggle.com/c/zillow-prize-1/data

# Step 1: Reading and Reviewing the Data
# set work directory
setwd("C:\\csv_data")
list.files()

# read data
train_raw <- read.csv("train_2016_v2.csv", stringsAsFactors = FALSE)
property <- read.csv("properties_2016.csv", stringsAsFactors = FALSE)

# Understand the data: 1, read data descriptions, 2, look at data
summary(train_raw)
str(train_raw)
dim(train_raw)
head(train_raw)

str(property)
summary(property)
head(property)

length(unique(train_raw$parcelid))
length(unique(property$parcelid))

# Another way to do this (with a function):
count <- function(x){length(unique(x))}
count(train_raw$parcelid)
count(property$parcelid)

# Join two data sets by parcelid
train <- merge(train_raw, property, by = "parcelid")
dim(train)
summary(train)

write.csv(train, 'train_property_20180804.csv')
train <- read.csv("train_property_20180804.csv", stringsAsFactors = FALSE)

# Step 2: Dealing with Missing Data

