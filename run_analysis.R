require(dplyr)
require(tidyr)
		
		## declare urls for 8 files
		activity_labels_path <- "UCI HAR Dataset/activity_labels.txt"
		features_path <- "UCI HAR Dataset/features.txt"
		x_train_path <- "UCI HAR Dataset/train/X_train.txt"
		y_train_path <- "UCI HAR Dataset/train/y_train.txt"
		subject_train_path <- "UCI HAR Dataset/train/subject_train.txt"
		x_test_path <- "UCI HAR Dataset/test/X_test.txt"
		y_test_path <- "UCI HAR Dataset/test/y_test.txt"
		subject_test_path <- "UCI HAR Dataset/test/subject_test.txt"

		## read activity labels
		activity_labels_raw <- read.table(activity_labels_path)

		## read, extract, transpose features file
		features_raw <- read.table(features_path)
		features <- t(features_raw['V2'])

		## read and add header to activity-train file 
		y_train_raw <- read.table(y_train_path)
		colnames(y_train_raw) <- "activity"

		##use activity-train value as index in activity-label array
		for(i in 1:length(y_train_raw$activity)){
				y_train_raw$activity[i] <- 
						toString(activity_labels_raw[y_train_raw$activity[i],2])
		}

		## get logical vector from features for column names including std and mean
		x <- (grepl("mean",features) | grepl("std",features))
		
		## read and add header to subject-train file
		subject_train_raw <- read.table(subject_train_path)
		colnames(subject_train_raw) <- "subject"

		## read and add headers to main train data
		x_train_raw <- tbl_df(read.table(x_train_path))
		colnames(x_train_raw) <- features
		
		## remove columns without mean or std in label
		x_train_short <- x_train_raw[,x]

		## add activity and subject columns to main train data
		x_train_assembling <- 
				mutate(x_train_short,
					activity = y_train_raw$activity,
					subject = subject_train_raw$subject,
					source = "train"
				)

		## read and add headers to main test data
		x_test_raw <- tbl_df(read.table(x_test_path))
		colnames(x_test_raw) <- features

		## remove columns without mean or std in label
		x_test_short <- x_test_raw[,x]

		## read and add header to activity-test file
		y_test_raw <- read.table(y_test_path)
		colnames(y_test_raw) <- "activity"

		## use activity-test value as index in activity-label array
		for(i in 1:length(y_test_raw$activity)){
				y_test_raw$activity[i] <- 
						toString(activity_labels_raw[y_test_raw$activity[i],2])
		}

		## read and add header to subject-test file
		subject_test_raw <- read.table(subject_test_path)
		colnames(subject_test_raw) <- "subject"

		## add activity and subject columns to main test data
		x_test_assembling <- 
				mutate(x_test_short,
					activity = y_test_raw$activity,
					subject = subject_test_raw$subject,
					source = "test"
				)

		## combine train and test data
		tidy_data <- union(x_train_assembling,x_test_assembling)

		## group data by activity and subject then get a mean for remaining columns
		tidy_data2 <- 
				tidy_data %>% 
				group_by(activity,subject,source) %>% 
				summarise_each(funs(mean))

		## write final data to file
		write.table(tidy_data2,file = 'results.txt',row.names = FALSE)
