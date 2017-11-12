library(data.table)
setwd('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project')
list.files(recursive = TRUE)
test_data <- data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/test/X_test.txt'))
test_label <- data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/test/y_test.txt'))
library(dplyr)
test_data <- tbl_df(test_data)
test_label <- tbl_df(test_label)
dim(test_data)
dim(test_label)
train_data <- tbl_df(data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/train/X_train.txt')))
train_label <- tbl_df(data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/train/y_train.txt')))
dim(train_label)
dim(train_data)
subject_test <- data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/test/subject_test.txt'))
subject_train <- data.table(read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/train/subject_train.txt'))
total_data <- rbind(train_data, test_data)
total_label <- rbind(train_label, test_label)
subject <- rbind(subject_train, subject_test)
dim(total_data)
dim(total_label)
dim(subject)
features1 <- read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/features.txt')
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features1[, 2])
length(meanStdIndices)
head(features1)
total_data <- total_data[,meanStdIndices]
dim(total_data)
names(total_data) <- gsub('\\(\\)','',features1[meanStdIndices,2])
names(total_data) <- gsub('mean','Mean',names(total_data))
names(total_data) <- gsub('std','Std',names(total_data))
names(total_data) <- gsub('-','',names(total_data))
activity <- read.table('C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/UCI HAR Dataset/activity_labels.txt',row.names = 1)
activity
activity[,'V2'] <- tolower(gsub('_','',activity[,'V2']))
activity
dim(total_label)
dim(activity)
unique(total_label)
total_label$V1 <- activity[total_label$V1,'V2']
total_label
names(total_label) <- 'activity'
names(subject) <- 'subject'
cleanData <- cbind(subject, total_label, total_data)
dim(cleanData)
write.table(cleanData,"mergeCleanDataAssignmentPeerGraded.txt",sep="\t",row.names=FALSE)

subjectLength <- length(table(subject))
activityLength <- dim(activity)[1]
varLength <- dim(cleanData)[2]
rslt <- as.data.frame(matrix(NA, nrow = activityLength * subjectLength, ncol = varLength))
names(rslt) <- names(cleanData)
row <- 1
for(i in 1:subjectLength){
  for(j in 1:activityLength){
    rslt[row, 1] <- i
    rslt[row, 2] <- activity[j, 1]
    rslt[row, 3:varLength] <- colMeans(cleanData[i == cleanData$subject & activity[j, 1] == cleanData$activity, 3:varLength])
    row <- row + 1
  }
}

head(rslt)
write.table(rslt, 'FinalResultMeanMatrix.txt', sep = '\t', row.names = FALSE)

data <- read.table("C:/Users/hp/datasciencecoursera/Course3(Getting and Cleaning Data)/Project/FinalResultMeanMatrix.txt")
