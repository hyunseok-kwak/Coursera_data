Human Activity Recognition Data Analysis
================

This repository contains the analysis of the Human Activity Recognition
Using Smartphones Dataset.

## This repository includes the following files:

- `README.md`
- `UCI HAR Dataset.zip` - Human Activity Recognition Using Smartphones
  Dataset
- `run_analysis.R` - R script to clean and process the data
- `data.tidy.txt` - Final processed dataset

## How to work the script

The `run_analysis.R` script performs the following operations:

0.  Download `UCI HAR Dataset.zip` and load `.txt` files from UCI HAR
    Dataset.
1.  Merges the training and the test sets to create one data set.
2.  Extracts only the measurements on the mean and standard deviation
    for each measurement.
3.  Uses descriptive activity names to name the activities in the data
    set.
4.  Appropriately labels the data set with descriptive variable names.
5.  From the data set in step 4, creates a second, independent tidy data
    set.

## How to run the analysis

1.  Clone this repository.
2.  Downloads and unzips the `UCI HAR Dataset.zip` if not present
3.  Open RStudio.
4.  Set working directory to the repository location.
5.  Run `source("run_analysis.R")`. You need `dplyr` packages to run
    script.

## Code books

The data represents experiments carried out with a group of 30
volunteers performing six activities (WALKING, WALKING_UPSTAIRS,
WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone.

### Variables in data_tidy.txt

- `ID`: ID of the subjectS (1-30)
- `Activity`: Type of activity performed (WALKING, WALKING_UPSTAIRS,
  WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)

#### Measurement variables

All measurements are floating-point values, normalized and bounded
within \[-1,1\]. ‘.mean’ is used to denote mean value. ‘.std’ is used to
denote standard variation. ‘.XYZ’ is used to denote 3-axial signals in
the X, Y and Z directions. ‘mean\_’ is used to denote the average of
each variable for each activity and each subject.

The measurements are classified in two domains:

Time-domain signals (variables prefixed by ‘t’), captured at a constant
rate of 50 Hz:

- tBodyAcc
- tGravityAcc
- tBodyAccJerk
- tBodyGyro
- tBodyGyroJerk
- tBodyAccMag
- tGravityAccMag
- tBodyAccJerkMag
- tBodyGyroMag
- tBodyGyroJerkMag

Frequency-domain signals (variables prefixed by ‘Frequency’):

- fBodyAcc
- fBodyAccJerk
- fBodyGyro
- fBodyAccMag
- fBodyAccJerkMag
- fBodyGyroMag
- fBodyGyroJerkMag
