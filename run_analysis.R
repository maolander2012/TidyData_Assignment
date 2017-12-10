## load libraries from this course that could possibly help
library(dplyr)
library(data.table)
library(reshape2)

## 1. Merges the training and the test sets to create one data set.

## 1.1) load activity table:

table_activity <- read.table("UCI HAR Dataset/activity_labels.txt", 
                             col.names=c("activity_id", "activity_name"))

## 1.2)  load features table:

table_features <- read.table("UCI HAR Dataset/features.txt", 
                             col.names=c("feature_id", "feature_name"))


## 1.3) Load 3 "train" tables and set column names:

table_x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
colnames(table_x_train) <- table_features$feature_name

table_y_train <- read.table("UCI HAR Dataset/train/y_train.txt",
                            col.names=c("activity_id"))

table_subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",
                                  col.names=c("subject_id"))


## 1.4) Load 3 "test" tables and set column names:

table_x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
colnames(table_x_test) <- table_features$feature_name 


table_y_test <- read.table("UCI HAR Dataset/test/y_test.txt",
                           col.names=c("activity_id"))


table_subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",
                                 col.names=c("subject_id"))


## 1.5) Merge the training and the test sets to create 1 data set:

table_all_train <- cbind(table_subject_train, table_y_train, table_x_train)
table_all_test <- cbind(table_subject_test, table_y_test, table_x_test)
table_both <- rbind(table_all_train, table_all_test)

## reorder by first two ID columns
table_both <- table_both[order(table_both$subject_id, table_both$activity_id),]


## 2. Extracts only the measurements on the mean and standard 
##    deviation for each measurement. 

## get all column names
table_both_colnames <- colnames(table_both)


## set the columns needed to TRUE
mean_std_extracted <- (grepl("activity_id" , table_both_colnames) | 
                         grepl("subject_id" , table_both_colnames) | 
                         grepl("mean.." , table_both_colnames) | 
                         grepl("std.." , table_both_colnames))


## new Subset 
table_both2 <- table_both[,mean_std_extracted == TRUE]


## 3. Uses descriptive activity names to name the activities in the data set.
##    And also turn the first two columns into factors

table_both2$activity_id <- factor(table_both2$activity_id, 
        levels = table_activity[,1], labels = table_activity[,2])

table_both2$subject_id <- as.factor(table_both2$subject_id)


## 4. see below, within #5

## 5. From the data set in step 4, creates a second, 
## independent tidy data set with the average of each variable 
## for each activity and each subject.

## 5.1 Making second tidy data set
TidyDataSet2 <- aggregate(. ~subject_id + activity_id, table_both2, mean)

TidyDataSet2 <- TidyDataSet2[order(TidyDataSet2$subject_id, TidyDataSet2$activity_id),]

## 4. Appropriately label the data set with descriptive variable names
##    Doing it here.
TidyDataSet2 <- rename(TidyDataSet2, Subject=subject_id, Activity=activity_id)


## 5.2 Write 2nd tidy data set to a txt file to turn in
write.table(TidyDataSet2, "TidyDataSet2.txt", row.names = FALSE, col.names = TRUE)

