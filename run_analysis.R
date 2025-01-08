# Load R packages
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Data load
## UCI HAR Dataset download
if (!file.exists("UCI HAR Dataset")) {
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl, destfile = "UCI HAR Dataset.zip")
  unzip("UCI HAR Dataset.zip")
}

## Define text_load function for .txt file opening
text_load <- function(location, header = FALSE, sep = ""){
  path_wd <- getwd()
  file_path <- paste(path_wd, location, sep = "")
  data <- read.table(file_path, header = header, sep = sep)
  return(data)
}

## Load .txt files
feature_list <- text_load("/UCI HAR Dataset/features.txt")
activity_label <- text_load("/UCI HAR Dataset/activity_labels.txt")
train.set <- text_load("/UCI HAR Dataset/train/X_train.txt")
train.label <- text_load("/UCI HAR Dataset/train/y_train.txt")
train.subject <- text_load("/UCI HAR Dataset/train/subject_train.txt")
test.set <- text_load("/UCI HAR Dataset/test/X_test.txt")
test.label <- text_load("/UCI HAR Dataset/test/y_test.txt")
test.subject <- text_load("/UCI HAR Dataset/test/subject_test.txt")


# 1. Merges the training and the test sets to create one data set.
## Combine subject, label, set according to train or test 
data.train <- cbind(train.subject, train.label, train.set)
data.test <- cbind(test.subject, test.label, test.set)

## Add Source column to mark train or test
data.train$Source <- "train"
data.test$Source <- "test"

## Combine data
data <- rbind(data.train, data.test)

## Set colnames
col_names <- c("ID", "Activity", feature_list$V2, "Source")
colnames(data) <- col_names

## Adjust order of column names
data <- data[, c("Source", "ID", "Activity", feature_list$V2)]


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## Extract column containing mean() or std()
col_select <- grepl("mean\\(\\)|std\\(\\)", names(data))
col_select[1:3] <- TRUE
data.extract <- data[, col_select]


# 3. Uses descriptive activity names to name the activities in the data set
## Use factor function 
data.extract$Activity <- factor(data.extract$Activity,
                                levels = c("1", "2", "3", "4", "5", "6"),
                                labels = activity_label$V2)


# 4. Appropriately labels the data set with descriptive variable names. 
## Replace "-" with "."  and "()" with "" in column names
colnames(data.extract) <- gsub("-", ".", colnames(data.extract))
colnames(data.extract) <- gsub("\\(\\)", "", colnames(data.extract))


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## By using dplyr packages, make tidy data according to ID & Activity
data.tidy <- data.extract %>% 
  group_by(ID, Activity) %>% 
  summarize(across(where(is.numeric), mean, .names = "mean_{.col}"), .groups = "drop")

## Saving as .txt file
write.table(data.tidy, file = "data.tidy.txt", row.names = FALSE)
