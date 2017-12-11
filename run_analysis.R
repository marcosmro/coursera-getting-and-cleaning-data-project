# Peer-graded Assignment: Getting and Cleaning Data Course Project

workspace = "<path_to_your_worspace>"
setwd(workspace)
zip_filename <- "uci_har_dataset.zip"

### DOWNLOAD THE DATASET

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# Read features
features <- read.table("UCI HAR Dataset/features.txt")

# Read activity labels
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

### READ DATA

# Read test data
test_subject <-  read.table("UCI HAR Dataset/test/subject_test.txt")
test_activity_codes <- read.table("UCI HAR Dataset/test/y_test.txt")
test_data <- read.table("UCI HAR Dataset/test/X_test.txt")

# Read training data
train_subject <-  read.table("UCI HAR Dataset/train/subject_train.txt")
train_activity_codes <- read.table("UCI HAR Dataset/train/y_train.txt")
train_data <- read.table("UCI HAR Dataset/train/X_train.txt")

### DATA PREPARATION

# Function that prepares the dataset
prepare_dataset <- function(dataset_type_name, subjects, data, activity_codes) {
  colnames(subjects) <- "subject"
  colnames(data) <- tolower(features$V2)
  activities <- merge(activity_codes, activity_labels, by="V1")
  activities <- subset(activities, select = -c(V1)) # remove the activity code. It is redundant
  colnames(activities) <- c("activity_label")
  dataset_type <- cbind(sourcedataset = rep(dataset_type_name,nrow(subjects)))
  result <- cbind(dataset_type, subjects, activities, data)
}

test_full <- prepare_dataset("TEST", test_subject, test_data, test_activity_codes)
train_full <- prepare_dataset("TRAIN", train_subject, train_data, train_activity_codes)

### ASSIGNMENT RESULTS

# TASK 1: Merge the training and the test sets to create one data set.

all_data <- rbind(test_full, train_full)
all_data <- all_data[order(all_data$subject),] # sort by subject identifier

# TASK 2: Extract only the measurements on the mean and standard deviation for each measurement

indexes <- grep("mean\\(\\)|std\\(\\)", colnames(all_data))
mean_std_data <- all_data[,indexes]

# TASK 3: Use descriptive activity names to name the activities in the data set
# Already done as part of the "prepare_dataset" function.

# TASK 4: Appropriately label the data set with descriptive variable names.
# Most part of the labeling is done already in the "prepare_dataset" function. Here we just remove the
# parentheses from the feature names and replace hyphens by underscores
colnames(mean_std_data) <- gsub("\\(\\)", "", colnames(mean_std_data))
colnames(mean_std_data) <- gsub("-", "_", colnames(mean_std_data))

# TASK 5:
# From the data set in step 4, create a second, independent tidy data set with the average of each variable 
# for each activity and each subject.
new_data <- cbind(subject = all_data$subject, activity_label = all_data$activity_label, mean_std_data)

average_groups_data <- aggregate(.~activity_label+subject, new_data, mean)
average_groups_data_sorted <- average_groups_data[order(average_groups_data$activity_label),]

write.table(average_groups_data_sorted, "tidy_data_set.txt", row.name=FALSE)
