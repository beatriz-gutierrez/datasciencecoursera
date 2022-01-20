# STEP 0 - Initial set-up
# download and unzip data from the web
file_url = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
zip_file <- ".\\getdata_projectfiles_UCI HAR Dataset.zip"
if (!file.exists(zip_file)) { 
        download.file(file_url, destfile = zip_file, mode= 'wb')
        unzip(zip_file)
        date_download <- date()
}
data_path <- ".\\UCI HAR Dataset"

# STEP 1 - Merges the training and the test sets to create one data set
# read the training data
features <- read.table(paste(data_path, "\\features.txt", sep = ""))
colnames(features) <- c("id", "feature")
features_names <- as.character(features$feature) # features names

train_data <- read.table(paste(data_path, "\\train\\X_train.txt", sep = ""))
train_labels <- read.table(paste(data_path, "\\train\\y_train.txt", sep = ""))
train_subject <- read.table(paste(data_path, "\\train\\subject_train.txt", sep = ""))

# merge all train data into a data frame
train_data_frame <- data.frame(train_subject, train_labels, train_data)
colnames(train_data_frame) <- c("Subject", "Activity", features_names)

# read the test data
test_data <- read.table(paste(data_path, "\\test\\X_test.txt", sep = ""))
test_labels <- read.table(paste(data_path, "\\test\\y_test.txt", sep = ""))
test_subject <- read.table(paste(data_path, "\\test\\subject_test.txt", sep = ""))

# merge all train data into a data frame
test_data_frame <- data.frame(test_subject, test_labels, test_data)
colnames(test_data_frame) <- c("Subject", "Activity", features_names)

# merge the training and test datasets
data <- rbind(train_data_frame, test_data_frame)


# STEP 2 - Extracts the measurements on the mean and standard deviation 
#         for each measurement
mean_std_columns <- grep("-mean()|-std()", features_names)
# the mean and std index start in the 3th position in the complete data set
mean_std_indexes <- mean_std_columns + 2 
mean_std_data <- data[, c(1,2, mean_std_indexes)]

# STEP 3 - Uses descriptive activity names to name the activities in the data set
activity_data <- read.table(paste(data_path, "\\activity_labels.txt", sep = ""))
colnames(activity_data) <- c("Id", "Activity")
mean_std_data$Activity <- activity_data$Activity[mean_std_data$Activity]

# STEP 4 - Appropriately labels the data set with descriptive variable names
# based on the features_info.txt file
label_names <- names(mean_std_data)
label_names <- gsub("[(][)]", "", label_names)
label_names <- gsub("-mean", ".Mean",label_names)
label_names <- gsub("-std", ".StandardDeviation", label_names)
label_names<- gsub("-X", ".CoordX", label_names)
label_names<- gsub("-Y", ".CoordY", label_names)
label_names<- gsub("-Z", ".CoordZ", label_names)
label_names<- gsub("^t", "TimeDomainSignal.", label_names)
label_names<- gsub("^f", "FrequencyDomainSignal.", label_names)
label_names <- gsub("Acc", "Accelerometer", label_names)
label_names <- gsub("Gyro", "Gyroscope", label_names)
label_names <- gsub("Mag", "Magnitude", label_names)
label_names <- gsub("Freq", "Frequency", label_names)
names(mean_std_data) <- label_names

# STEP 5 - From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject
variables <- mean_std_data[ ,3:81] # remove activity and subject
# group by activity and subject
by_activity_subject <- aggregate(variables, 
                       by = list(Activity = mean_std_data$Activity, 
                                 Subject = mean_std_data$Subject), 
                                 FUN = mean)
# store the new dataset into a new file and its header into a separate file
write.table(names(by_activity_subject), file = "by_activity_subject_header.txt") 
write.table(by_activity_subject, file = "by_activity_subject_data.txt", row.names = FALSE, col.names = FALSE) 