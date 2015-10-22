A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The data is available at:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The aim of the project is to clean and extract usable data from the above zip file. R script called run_analysis.R that does the following: - Merges the training and the test sets to create one data set. - Extracts only the measurements on the mean and standard deviation for each measurement. - Uses descriptive activity names to name the activities in the data set - Appropriately labels the data set with descriptive variable names. - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

File descriptions:

run_analysis.R : the R-code run on the data set

Tidy.txt : the clean data extracted from the original data using run_analysis.R

CodeBook.md : the CodeBook reference to the variables in Tidy.txt

README.md : the analysis of the code in run_analysis.R
