## Load dplyr package.

library(dplyr)


## Read in the relevant text files as data files, attach appropriate column 
## names and merge dataframes (df) to create one tidy df


### Read in initial data
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
labels_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")


 ## Apply column names to initial df from txt files provided by the data 
## source

colnames(X_train) <- features$V2

##  Manipulate the labels data provied to obtain a vector that can be bound to 
##  df created above.

activity <- labels_train$V1

## Create UDF to assign activity labels to the raw data provided.
act_label <- function(x){
  if (x == 1){
    x <- "walking"
  } else if (x == 2){
    x <- "walking_upstairs"
  } else if (x == 3) {
    x <-"walking_downstairs"
  } else if (x == 4) {
    x <- "sitting"
  } else if (x == 5) {
    x <- "standing"
  } else if (x == 6)
    x <- "laying"
}

### use lapply to apply the UDF "act_label" to the labels provided in the raw 
##  data. Unlist is then applied to the outputs of lapply to create a vector
##  that can be bound to the main data frame. 

y <- lapply( activity, act_label)
mod_act<- unlist(y)

##  Manipulate the subject data provied to obtain a vector that can be bound to 
##  df created above.

colnames(subject_train) <-c("subject")
subject <- subject_train$subject

##   columnbind new vectors to the original data set

X_train <- cbind(subject = subject, activity = mod_act, X_train)

############################################

###   Repeat the above steps for the testing data


### Read in initial data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
labels_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")


## Apply column names to initial df from txt files provided by the data 
## source

colnames(X_test) <- features$V2

##  Manipulate the labels data provied to obtain a vector that can be bound to 
##  df created above.

activity <- labels_test$V1


### use lapply to apply the UDF act_label to the labels provided in the raw 
##  data. Unlist is then applied to the outputs of lapply to create a vector
##  that can be bound to the main data frame. 

y <- lapply( activity, act_label)
mod_act<- unlist(y)

##  Manipulate the subject data provied to obtain a vector that can be bound to 
##  df created above.

colnames(subject_test) <-c("subject")
subject <- subject_test$subject


X_test <- cbind(subject = subject, activity = mod_act, X_test)


###Part 1 - Merge the two data sets

Merged_data <- rbind(X_test, X_train)

### Part 2 Extract mean and standatd deviation measurements

## Preserve subjecy and activity columns from "Merged_data"

subject_new <- Merged_data$subject
activity_new <- Merged_data$activity

## Extract columns that refer to mean and std dev measurements.

Mean <- Merged_data[,grepl("mean", colnames(Merged_data))]
Stddev <- Merged_data[,grepl("std", colnames(Merged_data))]

## Column Bind datasets together to create tidy data set.

Tidydata<-cbind(subject = subject_new, activity = activity_new, Mean, Stddev)

## Part3: Use descriptive activity names to name the activities 
## in the data set

## This step was previously completed as part of the initial 
## data tidy up above using a UDF.

### Part 4 Appropriately labels the data set with descriptive 
## variable names.

names(Tidydata)<-gsub("Acc", "Accelerometer", names(Tidydata))
names(Tidydata)<-gsub("Gyro", "Gyroscope", names(Tidydata))
names(Tidydata)<-gsub("BodyBody", "Body", names(Tidydata))
names(Tidydata)<-gsub("Mag", "Magnitude", names(Tidydata))
names(Tidydata)<-gsub("^t", "Time", names(Tidydata))
names(Tidydata)<-gsub("^f", "Frequency", names(Tidydata))
names(Tidydata)<-gsub("tBody", "TimeBody", names(Tidydata))
names(Tidydata)<-gsub("-mean()", "Mean", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("-std()", "STD", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("-freq()", "Frequency", names(Tidydata), ignore.case = TRUE)
names(Tidydata)<-gsub("angle", "Angle", names(Tidydata))
names(Tidydata)<-gsub("gravity", "Gravity", names(Tidydata))


###Step 5


## Apply groups to "Tidy_data and then calculate the mean of each group and store
## in new dataframe "Final_data
   

Final_data <- Tidydata %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean = mean))                    

### Export the final data frame to a txt file.

write.table(Final_data, file = "Final_data.txt", row.names = F)


