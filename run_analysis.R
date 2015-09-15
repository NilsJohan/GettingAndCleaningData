##########################################
######### Course project #################
##########################################

# This script follows a step by step procedure in order to transform the dataset from
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# into a tidy subset dataset as part of the course project in the Coursera:
# Data Science Specialization: Getting and Cleaning Data
# The script is divided in blocks that explain each one of the steps. 
# During this process variables were sensible names to easily identify the parts.
# ACKNOWLEDGEMENT: In all essential parts, Courtesy to Gonzalo PENA C. (https://github.com/goanpeca/coursera-data-science-getting-and-cleaning-data/blob/master/run_analysis.R)

# @ Script preprocessing
# Set the current work directory ####
setwd("C:/Users/Johan/Documents/R-test/3 Coursera_Getting_and_Cleaning_Data/CourseProject")

# Download data set and unzip it ####
dataUrl          <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
tempFile         <- tempfile()
download.file(dataUrl, destfile=tempFile)
unzip(tempFile)

# Define base folders and subfolder names ####
baseFolder       <- 'UCI HAR Dataset'
trainFolder      <- 'train'
testFolder       <- 'test'

# Create path to files ####
activity_labels  <- file.path(baseFolder, file="activity_labels.txt")
features         <- file.path(baseFolder, file="features.txt")
subject_train    <- file.path(baseFolder, trainFolder, file="subject_train.txt")
y_train          <- file.path(baseFolder, trainFolder, file="y_train.txt")
X_train          <- file.path(baseFolder,  trainFolder, file="X_train.txt")
subject_test     <- file.path(baseFolder, testFolder, file="subject_test.txt")
y_test           <- file.path(baseFolder, testFolder, file="y_test.txt")
X_test           <- file.path(baseFolder, testFolder, file="X_test.txt")

# Read files into R data frames ####
activity         <- read.table(activity_labels, col.names=c('Number', 'Activity'))
features         <- read.table(features, col.names=c('Number', 'Feature'))
subject_train    <- read.table(subject_train, col.names=c('Subject'))
y_train          <- read.table(y_train, col.names=c('Number'))
X_train          <- read.table(X_train)
subject_test     <- read.table(subject_test, col.names=c('Subject'))
y_test           <- read.table(y_test, col.names=c('Number'))
X_test           <- read.table(X_test)

# Fix features names to be used as column names ####
features$Feature <- gsub('\\(|\\)', '', features$Feature)      ## skip blank and parentheses
features$Feature <- gsub('-|,', '.', features$Feature)         ## replace "-" with "."
features$Feature <- gsub('BodyBody', 'Body', features$Feature) ## replace "BodyBody" with "Body"
features$Feature <- gsub('^f', 'Frequency.', features$Feature) ## replace "f" at start of line with "Frequency"
features$Feature <- gsub('^t', 'Time.', features$Feature)      ## replace "t" at start of line with "Time."
features$Feature <- gsub('^angle', 'Angle.', features$Feature) ## replace "angle" at start of line with "Angle"
features$Feature <- gsub('mean', 'Mean', features$Feature)     ## replace "mean" with "Mean"
features$Feature <- gsub('tBody', 'TimeBody', features$Feature)## replace "tBody" with "TimeBody"

# Change the name of the data sets using the features data ####
colnames(X_test) <- features$Feature                           ## replace column names in testData with edited edited names in features (matched number and order of columns)
colnames(X_train)<- features$Feature                           ## replace column names in trainData with edited edited names in features (matched number and order of columns)

# Replace train and test labels by the names in the activity file ####
labels           <- factor(activity$Activity)                  ## factor activity, e.g. number=5 <=> standing
testFactors      <- factor(y_test$Number)                      ## test factor labels
trainFactors     <- factor(y_train$Number)                     ## train factor labels
testActivity     <- data.frame(Activity=as.character(factor(testFactors, labels=labels))) ##variable "number" replaced and matched with variable Activity in "labels", e.g. number=5 <=> standing
trainActivity    <- data.frame(Activity=as.character(factor(trainFactors, labels=labels))) ##variable "number" replaced and matched with variable Activity in "labels", e.g. number=5 <=> standing

# Merge data using column binds ####
testMergedData   <- cbind(subject_test, testActivity, X_test)
trainMergedData  <- cbind(subject_train, trainActivity, X_train)

# @ Merges the training and the test sets to create one data set ####
mergedData       <- rbind(testMergedData, trainMergedData)

# Select columns that do not contain Angle or MeanFreq ####
# This columns are not considered as mean or std meassurements
cols             <- c()
colNames         <- colnames(mergedData)
for (i in seq_along(colNames)){
        name <- colNames[i]
        check1 <- grep('Angle', x=name)
        check2 <- grep('MeanFreq', x=name)
        if (!(any(check1) | any(check2))){
                cols <- c(cols, i)
        }
} 

# Extract only the measurements on the mean and standard deviation #### 
# from the selected columns in the previous step
mergedData       <- mergedData[,cols]
mergedDataSubset <- mergedData[,grep('Subject|Activity|Mean|std',x=colnames(mergedData))]

# @ Creates a second, independent tidy data ####
# set with the average of each variable for each activity and each subject ####
library(data.table)
tidyData         <- data.table(mergedDataSubset)
tidyData         <- tidyData[,lapply(.SD, mean), by=c('Subject', 'Activity')]
tidyData         <- tidyData[order(tidyData$Subject, tidyData$Activity),]

# @ Write the output to a file ####
tidyFileName     <- 'tidy.txt'
write.table(tidyData, file=tidyFileName, row.names=FALSE)
DATA             <- read.table(file="tidy.txt", header=T)

# @ File postprocessing
# @ For test purposes read the created file ####
tidyDataRead     <- read.csv(tidyFileName, sep=' ')

# @ For CodeBook.md creation write column names of data set ####
write.table(colnames(mergedDataSubset), 'dataset-column-names.txt', row.names=FALSE)
