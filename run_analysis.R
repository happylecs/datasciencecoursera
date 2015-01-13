# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
activityMap<-read.table("./UCI HAR Dataset/activity_labels.txt")
names(activityMap)<-c("V1","Activity")
  
features<-read.table("./UCI HAR Dataset/features.txt")
names(features)<-c("id","measurement")
features$measurement<-as.character(features$measurement)
keepCols<-sort(c(grep("[M|m][E|e][A|a][N|n]",features$measurement),grep("[S|s][T|t][D|d]",features$measurement)))
keepCols<-sort(c(grep("mean\\(\\)",features$measurement),grep("std\\(\\)",features$measurement)))
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

trainData<-read.table("./UCI HAR Dataset/train/X_train.txt")
names(trainData)<-features$measurement
trainDataLabel<-read.table("./UCI HAR Dataset/train/y_train.txt")
trainDataLabel<-merge(trainDataLabel,activityMap,by="V1")
trainDataSubject<-read.table("./UCI HAR Dataset/train/subject_train.txt")
trainData$Activity<-trainDataLabel$Activity
trainData$Subject<-trainDataSubject$V1
trainData$Source<-"train"

testData<-read.table("./UCI HAR Dataset/test/X_test.txt")
names(testData)<-features$measurement
testDataLabel<-read.table("./UCI HAR Dataset/test/y_test.txt")
testDataLabel<-merge(testDataLabel,activityMap,by="V1")
testDataSubject<-read.table("./UCI HAR Dataset/test/subject_test.txt")
testData$Activity<-testDataLabel$Activity
testData$Subject<-testDataSubject$V1
testData$Source<-"test"

smartPhoneActRecogData<-rbind.data.frame(trainData,testData)
rm(trainDataLabel,trainDataSubject,testDataLabel,testDataSubject,trainData,testData,activityMap)

smartPhoneActRecogData<-smartPhoneActRecogData[,c(keepCols,562,563)]
smartPhoneActRecogData$Subject<-as.factor(smartPhoneActRecogData$Subject)

smartPhoneActRecogAvgStats<-aggregate(smartPhoneActRecogData[,1:64], by = list(smartPhoneActRecogData$Activity, smartPhoneActRecogData$Subject), FUN = "mean")
names(smartPhoneActRecogAvgStats)[1:2]<-c("Activity","Subject")

# smartPhoneActRecogAvgStats[smartPhoneActRecogAvgStats$Activity=="WALKING" & smartPhoneActRecogAvgStats$Subject==1
#                          ,1:3]
# mean(
#   smartPhoneActRecogData[smartPhoneActRecogData$Activity=="WALKING" & smartPhoneActRecogData$Subject==1,
#                          1])

write.table(smartPhoneActRecogAvgStats,file = "smartPhoneActRecogAvgStats.txt",row.names = FALSE)
