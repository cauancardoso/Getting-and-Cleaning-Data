library(plyr)
setwd("C:/Users/Cauan/Documents/Data Science/Coursera/UCI HAR Dataset")

# 1. Merge the training and the test sets to create one data set.

## 1.1. Reading the activity_labels and features set.

features <- read.table("./features.txt",header=FALSE)

activity_labels <- read.table("./activity_labels.txt",header=FALSE)

### 1.1.1. Labeling the activity_labels columns

colnames(activity_labels) <- c("activity_id","activity_type")

## 1.2. Reading the train set.

subject_train <-read.table("./train/subject_train.txt", header=FALSE)

X_train <- read.table("./train/X_train.txt", header=FALSE)

Y_train <- read.table("./train/y_train.txt", header=FALSE)

### 1.2.1. Labeling the columns.

colnames(subject_train) <- "subject_id"

colnames(X_train) <- features[,2]

colnames(Y_train) <- "activity_id"

### 1.2.2. Creating the train_data set

train_data <- cbind(Y_train, subject_train, X_train)

## 1.3. Reading the test set.

subject_test <-read.table("./test/subject_test.txt", header=FALSE)

X_test <- read.table("./test/X_test.txt", header=FALSE)

Y_test <- read.table("./test/Y_test.txt", header=FALSE)

### 1.3.1. Labeling the columns

colnames(subject_test) <- "subject_id"

colnames(X_test) <- features[,2]

colnames(Y_test) <- "activity_id"

### 1.3.2. Creating the test_data set

test_data <- cbind(Y_test, subject_test, X_test)

## 1.4. Creating one dataset.

tt_data <- rbind(train_data, test_data)

# 2. Extract only the measurements on the mean and standard deviation for each measurement

tt_data_mean_std <- tt_data[,grepl("mean|std|subject_id|activity_id",colnames(tt_data))]

# 3. Uses descriptive activity names to name the activities in the data set

tt_data_mean_std <- join(tt_data_mean_std, activity_labels, by = "activity_id", match = "first")

tt_data_mean_std <- tt_data_mean_std[,-1]

# 4. Appropriately label the data set with descriptive variable names.

## 4.1. Remove parentheses

names(tt_data_mean_std) <- gsub("\\(|\\)", "", names(tt_data_mean_std), perl  = TRUE)

## 4.2. Correct syntax in names

names(tt_data_mean_std) <- make.names(names(tt_data_mean_std))

## 4.3. Add descriptive names

names(tt_data_mean_std) <- gsub("Acc", "Acceleration", names(tt_data_mean_std))
names(tt_data_mean_std) <- gsub("^t", "Time", names(tt_data_mean_std))
names(tt_data_mean_std) <- gsub("^f", "Frequency", names(tt_data_mean_std))
names(tt_data_mean_std) <- gsub("BodyBody", "Body", names(tt_data_mean_std))
names(tt_data_mean_std) <- gsub("mean", "Mean", names(tt_data_mean_std))
names(tt_data_mean_std) <- gsub("std", "Std", names(tt_data_mean_std))
names(tt_data_mean_std) <- gsub("Freq", "Frequency", names(tt_data_mean_std))
names(tt_data_mean_std) <- gsub("Mag", "Magnitude", names(tt_data_mean_std))

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_data <- ddply(tt_data_mean_std, c("subject_id","activity_type"), numcolwise(mean))

write.table(tidy_data, row.name = FALSE, file="tidy_data.txt")