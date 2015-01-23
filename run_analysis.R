# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# Read Activity Map & features data
activityMap<-read.table("./UCI HAR Dataset/activity_labels.txt")
features<-read.table("./UCI HAR Dataset/features.txt")

# Initialize column names & data types
names(activityMap)<-c("V1","Activity")
names(features)<-c("id","measurement")
features$measurement<-as.character(features$measurement)

# Get only measurements which are either Mean or standard deviations
keepCols<-sort(c(grep("[M|m][E|e][A|a][N|n]",features$measurement),grep("[S|s][T|t][D|d]",features$measurement)))
keepCols<-sort(c(grep("mean\\(\\)",features$measurement),grep("std\\(\\)",features$measurement)))

# Change the columns to readable names
features$measurement<-gsub("-|\\(|\\)",".",features$measurement)
features$measurement<-gsub("^t","Time.",features$measurement)
features$measurement<-gsub("^f","Frequency.",features$measurement)
features$measurement<-gsub("Acc",".Acceleration.",features$measurement)
features$measurement<-gsub("Gyro",".Angular.Velocity.",features$measurement)
features$measurement<-gsub("(Body)+",".Body.",features$measurement)
features$measurement<-gsub("Gravity",".Gravity.",features$measurement)
features$measurement<-gsub("Jerk",".Jerk.",features$measurement)
features$measurement<-gsub("Mag",".Magnitude.",features$measurement)
features$measurement<-gsub("mean",".Mean.",features$measurement)
features$measurement<-gsub("std",".StandardDeviation.",features$measurement)
features$measurement<-gsub("(\\.)+",".",features$measurement)
features$measurement<-gsub("\\.$","",features$measurement)
features$measurement<-gsub("X$","X-Axis",features$measurement)
features$measurement<-gsub("Y$","Y-Axis",features$measurement)
features$measurement<-gsub("Z$","Z-Axis",features$measurement)

# Read train & merge with measurements & Activity
trainData<-read.table("./UCI HAR Dataset/train/X_train.txt")
names(trainData)<-features$measurement
trainDataLabel<-read.table("./UCI HAR Dataset/train/y_train.txt")
trainDataLabel<-merge(trainDataLabel,activityMap,by="V1")
trainDataSubject<-read.table("./UCI HAR Dataset/train/subject_train.txt")
trainData$Activity<-trainDataLabel$Activity
trainData$Subject<-trainDataSubject$V1
trainData$Source<-"train"

# Repeat the above step for test data
testData<-read.table("./UCI HAR Dataset/test/X_test.txt")
names(testData)<-features$measurement
testDataLabel<-read.table("./UCI HAR Dataset/test/y_test.txt")
testDataLabel<-merge(testDataLabel,activityMap,by="V1")
testDataSubject<-read.table("./UCI HAR Dataset/test/subject_test.txt")
testData$Activity<-testDataLabel$Activity
testData$Subject<-testDataSubject$V1
testData$Source<-"test"

# Concatenate test & train datasets
smartPhoneActRecogData<-rbind.data.frame(trainData,testData)
rm(trainDataLabel,trainDataSubject,testDataLabel,testDataSubject,trainData,testData,activityMap)

# Get selected measurements to desired output
smartPhoneActRecogData<-smartPhoneActRecogData[,c(keepCols,562,563)]
smartPhoneActRecogData$Subject<-as.factor(smartPhoneActRecogData$Subject)
smartPhoneActRecogAvgStats<-aggregate(smartPhoneActRecogData[,1:64], by = list(smartPhoneActRecogData$Activity, smartPhoneActRecogData$Subject), FUN = "mean")
names(smartPhoneActRecogAvgStats)[1:2]<-c("Activity","Subject")

# Write output into text file
write.table(smartPhoneActRecogAvgStats,file = "smartPhoneActRecogAvgStats.txt",row.names = FALSE)
