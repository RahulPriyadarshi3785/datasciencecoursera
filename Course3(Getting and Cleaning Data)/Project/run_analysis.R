#Project Script
#Set directory path

#Requirement 1:  Merges the training and the test sets to create one data set.
#Read the activity files

library(data.table)
setwd('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project')
list.files(recursive = TRUE)

#Read the test set data files.
test_data <- data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/test/X_test.txt'))
test_label <- data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/test/y_test.txt'))

#load 'dplyr package'
library(dplyr)

test_data <- tbl_df(test_data)
test_label <- tbl_df(test_label)

# checking dimensions
dim(test_data)  ## 2947 * 561
dim(test_label) ## 2947 * 1

# Read the training set data files.
train_data <- tbl_df(data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/train/X_train.txt')))
train_label <- tbl_df(data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/train/y_train.txt')))
dim(train_label) ## 7352 * 1
dim(train_data)  ## 7352 * 561

# Reading subject files
subject_test <- data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/test/subject_test.txt'))
subject_train <- data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/train/subject_train.txt'))

#Merging Datasets
total_data <- rbind(train_data, test_data)
total_label <- rbind(train_label, test_label)
subject <- rbind(subject_train, subject_test)

# checking dimensions
dim(total_data) # 10299     66
dim(total_label) # 10299     1
dim(subject) # 10299     1

# Loading information about features
features1 <- read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/features.txt')

# selecting features related to mean and standard deviations only
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features1[, 2])
length(meanStdIndices) # 66
head(features1)

# selecting feature sets related to mean and standard deviation
total_data <- total_data[,meanStdIndices]

#checking dimensions
dim(total_data) # 10299    66

# Renaming with proper names
names(total_data) <- gsub('\\(\\)','',features1[meanStdIndices,2])
names(total_data) <- gsub('mean','Mean',names(total_data))
names(total_data) <- gsub('std','Std',names(total_data))
names(total_data) <- gsub('-','',names(total_data))

# Loading activity datasets
activity <- read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/activity_labels.txt',row.names = 1)
activity

# tidying dataset activity 
activity[,'V2'] <- tolower(gsub('_','',activity[,'V2']))
activity

# checking dimensions and relation between total_label and activity
dim(total_label) # 10299     1
dim(activity) # 6     1
unique(total_label)

# Mapping total_label as per activity done by subjects
total_label$V1 <- activity[total_label$V1,'V2']

# Checking dataset
total_label

# Renaming with proper name for features
names(total_label) <- 'activity'
names(subject) <- 'subject'

# Final merging of tidy data sets
cleanData <- cbind(subject, total_label, total_data)

# checking dimensions
dim(cleanData) # 10299    68

# Saving result to text file('mergeCleanDataAssignmentPeerGraded.txt')
write.table(cleanData,"mergeCleanDataAssignmentPeerGraded.txt",sep="\t",row.names=FALSE)

#Requirement 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subjectLength <- length(table(subject)) # 30
activityLength <- dim(activity)[1] # 6
varLength <- dim(cleanData)[2] # 68
rslt <- as.data.frame(matrix(NA, nrow = activityLength * subjectLength, ncol = varLength))
names(rslt) <- names(cleanData)
row <- 1
for(i in 1:subjectLength){
  for(j in 1:activityLength){
    rslt[row, 1] <- i # setting subject
    rslt[row, 2] <- activity[j, 1] # setting activity done by subject
    rslt[row, 3:varLength] <- colMeans(cleanData[i == cleanData$subject & activity[j, 1] == cleanData$activity, 3:varLength])
    row <- row + 1 # incrementing row i.e moving to next row
  }
}

# checking output of mean variables
head(rslt)

# Saving final dataFrame as text file('FinalResultMeanMatrix.txt')
write.table(rslt, 'FinalResultMeanMatrix.txt', sep = '\t', row.names = FALSE)

# Fetch Result Matrix back
data <- read.table("C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/FinalResultMeanMatrix.txt")
