#!/usr/bin/Rscript

# Getting and Cleaning Data Course Project
# AUTHOR : Luis Garreta
# DATE   : 23-Feb-2021
# INPUTS : 8 files: activity_labels.txt features.txt subject_test.txt subject_train.txt X_test.txt X_train.txt y_test.txt y_train.txt
# OUTPUTS: 1 file : table-grouped-mean-Activiy-Subject.txt

library (dplyr)

#---- 0. Read input data ----
ytrain        = read.table ("inputs/y_train.txt", col.names=c("ActivityId"));
xtrain        = read.table ("inputs/X_train.txt");
xtrainSubjects = read.table ("inputs/subject_train.txt", col.names=c("Subject"));
ytest         = read.table ("inputs/y_test.txt", col.names=c("ActivityId"));
xtest         = read.table ("inputs/X_test.txt");
xtestSubjects  = read.table ("inputs/subject_test.txt", col.names=c("Subject"));

#---- 1. Merges the training and the test sets to create one data set ----
dataSubjTrainTest = rbind (cbind (xtrainSubjects, ytrain, xtrain, stringsAsFactors=F), 
						   cbind (xtestSubjects, ytest, xtest, stringsAsFactors=F), stringsAsFactors=F); 


#---- 3. Uses descriptive activity names to name the activities in the data set ----
activities = read.table ("inputs/activity_labels.txt", col.names=c("ActivityId", "Activity"));
dataWithActivities = merge (activities, dataSubjTrainTest, by= c("ActivityId"));
dataWithActivities ["ActivityId"] = NULL; 

#---- 4. Appropriately labels the data set with descriptive variable names ----
features      = read.table ("inputs/features.txt");
featuresNames = sub ("\\(\\)[-|\\s]*","", features[,2]);
#featuresNames = as.character (features [,2]);
names (dataWithActivities) = c("Activity", "Subject", featuresNames);

#---- 2. Extracts only the measurements on the mean and standard deviation for each measurement ----
indexes  = features %>% 
           filter (grepl ("mean", V2) | grepl ("std", V2)) %>% 
           select (V1) %>% unlist; 

dataMeanStd = dataWithActivities [, c(1,2, indexes+2)];

#---- 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.----
dataGroupedMean = dataMeanStd %>% group_by (Activity, Subject) %>% summarize_all (mean)
write.table (dataGroupedMean, "table-grouped-mean-Activiy-Subject.txt", row.names=F)
write.csv (dataGroupedMean, "table-grouped-mean-Activiy-Subject.csv", row.names=F)

