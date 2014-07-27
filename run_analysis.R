#Step 1
#Combine training and test sets to create data set.
#read subject training data
subject_train = read.table("UCI HAR Dataset/train/subject_train.txt", col.names=c("Subject"))
#Assign values of column and get data
subject_train$ID <- as.numeric(rownames(subject_train))
X_train = read.table("UCI HAR Dataset/train/X_train.txt")
#Assign row ID columns and read training data
X_train$ID <- as.numeric(rownames(X_train))
y_train = read.table("UCI HAR Dataset/train/y_train.txt", col.names=c("activity_id"))  # max = 6
#Assing value of ID COlumn Row Numbers
y_train$ID <- as.numeric(rownames(y_train))
#Combine subject_train and y_train to train
train <- merge(subject_train, y_train, all=TRUE)
#Combine train and X_train
train <- merge(train, X_train, all=TRUE)

#Read Subject Training Data
subject_test = read.table("UCI HAR Dataset/test/subject_test.txt", col.names=c("subject_id"))
#Assign row number as the values of ID column
subject_test$ID <- as.numeric(rownames(subject_test))
#Read testing data
X_test = read.table("UCI HAR Dataset/test/X_test.txt")
#Assign row number as the values of ID column
X_test$ID <- as.numeric(rownames(X_test))
#Read activity testing data
y_test = read.table("UCI HAR Dataset/test/y_test.txt", col.names=c("activity_id"))  # max = 6
#Assign row number as the values of ID column
y_test$ID <- as.numeric(rownames(y_test))
#Merge subject_test and y_test to train
test <- merge(subject_test, y_test, all=TRUE) 
#Merge test and X_test
test <- merge(test, X_test, all=TRUE) 

#Combine train and test
data1 <- rbind(train, test)

#Step 2
#Extract measurements on the mean and standard deviation for each measurement
features = read.table("UCI HAR Dataset/features.txt", col.names=c("feature_id", "feature_label"),)  #561
#Extract measurements on the mean and standard deviation for each measurement
selected_features <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ]
data2 <- data1[, c(c(1, 2, 3), selected_features$feature_id + 3) ]

#Step 3
activity_labels = read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activity_id", "activity_label"),) #
data3 = merge(data2, activity_labels)

#Step 4 
selected_features$feature_label = gsub("\\(\\)", "", selected_features$feature_label)
selected_features$feature_label = gsub("-", ".", selected_features$feature_label)
for (i in 1:length(selected_features$feature_label)) {
    colnames(data3)[i + 3] <- selected_features$feature_label[i]
}
data4 = data3

#Step 5
drops <- c("ID","activity_label")
data5 <- data4[,!(names(data4) %in% drops)]
aggdata <-aggregate(data5, by=list(subject = data5$subject_id, activity = data5$activity_id), FUN=mean, na.rm=TRUE)
drops <- c("subject","activity")
aggdata <- aggdata[,!(names(aggdata) %in% drops)]
aggdata = merge(aggdata, activity_labels)
write.csv(file="DataSet.csv", x=aggdata)