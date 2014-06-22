###### This function generates the file name of the resulting tidy data
###### table - and returns the analysis to the given location.

run_analysis <- function(outputfile = "tidy_data.txt") {
        
        ### include the necessary packages

        #install.packages("plyr")
        #install.packages("utils")
        require(plyr)
        require(utils)
        
        ### Load the data after unzipping
        ### Modify the below line according to your environment
        
        setwd("D:\\T400\\Documents and Settings\\user\\Dropbox\\Coursera\\3 Getting and Cleaning Data\\Course Project")
        my_dir <- "D:\\T400\\Documents and Settings\\user\\Dropbox\\Coursera\\3 Getting and Cleaning Data\\Course Project\\UCI HAR Dataset\\"
                
        features <- read.table(paste0(my_dir, "features.txt"), as.is = TRUE)
        
        ### Get the features desired
        featuresindex <- sort(c(grep("mean\\(", features[,2]),  grep("std", features[,2])))
        fnames <- features[,2][featuresindex]
        
        ### strip-off and tidy unwanted characters
        data_1 <- tolower(fnames)
        data_2 <- sub("\\(\\)", "", data_1,)
        data_3 <- sub("-std", "stddev", data_2,)
        data_4 <- gsub("-", "", data_3,)
        data_5 <- sub("^t", "time", data_4,)
        data_6 <- sub("^f", "freq", data_5,)
        data_final <- gsub("bodybody", "body", data_6,)
        
        ### Add "subject" and "activity" to fnames for the final table
        tidycolnames <- c("subjectid", "activity", data_final)
        
        ### Read the table 
        labels_data <- read.table(paste0(my_dir, "activity_labels.txt"), as.is = TRUE)
		
        ### Make the activity labels lower case, without underscores
        alables <- tolower(labels_data[,2])
        labels_underscore <- gsub("_", "", alables,)
        activitylabels_fin <- data.frame(labels_data[,1], labels_underscore)
        colnames(activitylabels_fin) <- c("V1", "V2")
        
        ### Create data frame
        testframefull <- read.table(paste0(my_dir, "test/X_test.txt"), as.is = TRUE)
        ### Select only the columns related to mean and standard deviation
        testframesub <- testframefull[,featuresindex]
        
        ### Read in the data that has the test subject codes
        testsubjects <- read.table(paste0(my_dir, "test/subject_test.txt"), as.is = TRUE)
        
        ### Read in the data that has the activity codes. Convert them to
        ### descriptive names.
        testactivitycode <- read.table(paste0(my_dir, "test/y_test.txt"), as.is = TRUE)
        testactivity <- join(activitylabels_fin, testactivitycode, by = "V1", type = "right", match = "all")
        
        ### Combine the tables into one data frame and assign the column names.
        testframe <- data.frame(testsubjects, testactivity[,2], testframesub)
        colnames(testframe) <- tidycolnames
        
        ### Create a data.frame from the training file.
        
        ### Read in the data on the training group.
        trainframefull <- read.table(paste0(my_dir, "train/X_train.txt"), as.is = TRUE)
        ### Select only the columns related to mean and standard deviation.
        trainframesub <- trainframefull[,featuresindex]
        
        ### Read in the data that has the training subject codes
        trainsubjects <- read.table(paste0(my_dir, "train/subject_train.txt"), as.is = TRUE)
        
        ### Convert to meaningful names.
        tracode <- read.table(paste0(my_dir, "train/y_train.txt"), as.is = TRUE)
        tr_activity <- join(activitylabels_fin, tracode, by = "V1", type = "right", match = "all")
        
        ### Combine the tables
        trainframe <- data.frame(trainsubjects, tr_activity[,2], trainframesub)
        colnames(trainframe) <- tidycolnames
        
        ### join the test and training data frames
        fullframe <- rbind(testframe, trainframe, deparse.level = 0)
        
        ### Create new data frame for each subject and activity
        averaging <- function(x) mean(x)
        new_frame <- ddply(fullframe, .(subjectid, activity), colwise(averaging))
        
        ### Write the new table to a file in the working directory
        write.table(new_frame, outputfile, sep = " ", row.names = FALSE)
}