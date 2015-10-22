
#Read training data
SubTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
ActTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
FeaTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

#Read test data
SubTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
ActTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
FeaTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

#Read Metadata feature+activity
FeaNames <- read.table("UCI HAR Dataset/features.txt")
ActLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

#Part 1 - Merge the training and the test sets to create one data set
#combine the respective data in training and test data sets corresponding to subject, activity and features. 
#The results are stored in subject, activity and features.

subject <- rbind(SubTrain, SubTest)
activity <- rbind(ActTrain, ActTest)
features <- rbind(FeaTrain, FeaTest)

#Naming the columns
#The columns in the features data set can be named from the metadata in FeaNames

colnames(features) <- t(FeaNames[2])

#Merge the data
#The data in features,activity and subject are merged and the complete data is now stored in completeData.

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#Part 2 - Extracts only the measurements on the mean and standard deviation 
#for each measurement Extract the column indices that have either mean or std in them.

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
#Add activity and subject columns to the list and look at the dimension of completeData

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredColumns]
dim(extractedData)

#Part 3 - Uses descriptive activity names to name the activities in the data set
#Change activity field in extractedData from numeric type to character

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(ActLabels[i,2])
}

#We need to factor the activity variable, once the activity names are updated.

extractedData$Activity <- as.factor(extractedData$Activity)

#Part 4 - Appropriately labels the data set with descriptive variable names

names(extractedData)
#By examining extractedData, some acronyms can be replaced:
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

names(extractedData)

#Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average 
#of each variable for each activity and each subject
#Firstly, let us set Subject as a factor variable.

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

#We create tidyData as a data set with average for each activity and subject.
#Then, we order the enties in tidyData and write it into data file Tidy.txt 
#that contains the processed data.

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
