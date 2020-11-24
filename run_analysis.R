###Step 0. Downloading and unzipping he dataset
#loading required package
library(data.table)
library(dplyr)

#downloading data set
filename <- "Cleaning_data_final.zip"
If(!file.exists(filename)) {
        file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(file_url, filename, method = "curl") 
}

#unzipping dataset
if(!file.exists("UCI HAR Dataset")) {
        unzip(filename)
}


###Step 1. Merging the training and the test sets to create one data set.
##Reading test tables
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

##Reading train tables
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

##Reading activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

##Reading features
features <- read.table("./UCI HAR Dataset/features.txt")



##Assigning column names
colnames(X_train) <- features[, 2]
colnames(y_train) <- "activityId"
colnames(subject_train) <- "subjectId"

colnames(X_test) <- features[, 2]
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activity_labels) <- c("activityId", "activityType")

##Merging data
test <- cbind(X_test, y_test, subject_test)
train <- cbind(X_train, y_train, subject_train)
test_and_train <- rbind(test, train)
dim(test_and_train)


###Step 2. Extracting only the measurements on the mean and standard deviation for each measurement.
##Reading column names
column_names <- colnames(test_and_train)

##Creating the vector for defining ID, mean and standard deviation
mean_and_std <- (grepl("activityId", column_names) |
                 grepl("subjectId", column_names) |
                 grepl("mean...", column_names) |
                 grepl("std...", column_names)
                 )

##Subsetting the data set with mean and standard deviation
test_train_mean_std <- test_and_train[, mean_and_std == TRUE]

###Step 3. Using descriptive activity names to name the activities in the data set
test_train_mean_std_names <- merge(test_train_mean_std, activity_labels,
                                   by='activityId',
                                   all = TRUE)


###Step 4. Appropriately labeling the data set with descriptive variable names done in the previous steps
col_names <- colnames(test_train_mean_std_names)
col_names_1 <- gsub("-mean.+-", "Mean", col_names)
col_names_2 <- gsub("-mean.+", "Mean", col_names_1)
col_names_3 <- gsub("-std.+-", "Std", col_names_2)
col_names_4 <- gsub("-std.+", "Std", col_names_3)
col_names_5 <- gsub("f", "Frequency", col_names_4)
col_names_6 <- gsub("^t", "Time", col_names_5)
col_names_7 <- gsub("Mag", "Magnitude", col_names_6)
col_names_8 <- gsub("Acc", "Accelerator", col_names_7)
col_names_9 <- gsub("Gyro", "Gyroscope", col_names_8)
coll_names_full <- col_names_9
colnames(test_train_mean_std_names) <- coll_names_full



###Step 5. Creating a second, independent tidy data set with the average of each variable 
###for each activity and each subject.

##Creating the second tidy data set
test_train_aggr_mean <- aggregate(.~subjectId + activityId + activityType, 
                                  test_train_mean_std_names, mean)
test_train_aggr_mean <- test_train_aggr_mean[order(test_train_aggr_mean$subjectId, 
                                                   test_train_aggr_mean$activityId,
                                                   test_train_aggr_mean$activityType), ]

##Writing the created data set to the .txt file
write.table(test_train_aggr_mean, "test_train_aggr_mean.txt", row.name = FALSE)



