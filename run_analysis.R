#Reading data

features <- read.csv("features.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
features <- features[,2]

train_x <- read.table("train/X_train.txt")
train_act <- read.csv("train/y_train.txt", header = FALSE, sep = " ")
train_sub <- read.csv("train/subject_train.txt", header = FALSE, sep = " ")

test_x <- read.table("test/X_test.txt")
test_act <- read.csv("test/y_test.txt", header = FALSE, sep = " ")
test_sub <- read.csv("test/subject_test.txt", header = FALSE, sep = " ")

training_set <- data.frame(train_sub, train_act, train_x)
colnames(training_set) <- c(c('Subject', 'Activity'), features)

testing_set <- data.frame(test_sub, test_act, test_x)
colnames(testing_set) <- c(c('Subject', 'Activity'), features)

# 1. Merges the training and the test sets to create one data set.
dataset <- rbind(training_set, testing_set)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_std <- grep('mean|std', features)
data <- dataset[,c(1,2,mean_std+2)]

# 3. Uses descriptive activity names to name the activities in the data set
labels <- read.table('activity_labels.txt', header = FALSE, stringsAsFactors = FALSE)
labels <- labels[,2]
data$Activity <- labels[data$Activity]

# 4. Appropriately labels the data set with descriptive variable names.
new_labels <- colnames(data)

gsub("[(][)]", "", new_labels) -> new_labels
gsub("-", "_", new_labels) -> new_labels
gsub("std", "Standard Deviation", new_labels) -> new_labels
gsub("mean", "Mean", new_labels) -> new_labels
gsub("Mag", "Magnitude", new_labels) -> new_labels
gsub("Acc", "Accelerometer", new_labels) -> new_labels
gsub("^f", "Frequency_", new_labels) -> new_labels
gsub("^t", "Time_", new_labels) -> new_labels
gsub("Gyro", "Gyroscope", new_labels) -> new_labels

colnames(data) <- new_labels

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata <- data %>% group_by(Subject, Activity) %>% summarise_all(.funs = mean)

write.table(tidydata, "Tidy Data.txt", row.name=FALSE)