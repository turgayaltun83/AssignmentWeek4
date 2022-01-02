library(reshape2)

# Some initial values
targetFolder <- 'UCI HAR Dataset'
filename <- 'getdata_dataset.zip'

# Checking if the user has already unzipped th file
if(!file.exists(targetFolder)) {
        # Do they at least have the zip file?
        if(!file.exists(filename)) {
                
                # They don't have the zip file, so downlaod it
                download.file(
                        'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip',
                        filename
                )
        }
        
        # Unzipping the file
        unzip(filename)
}

# 1. Merging the training and the test sets to create one data set.

# Reading in the data into the test and training sets
test.data <- read.table(file.path(targetFolder, 'test', 'X_test.txt'))
test.activities <- read.table(file.path(targetFolder, 'test', 'y_test.txt'))
test.subjects <- read.table(file.path(targetFolder, 'test', 'subject_test.txt'))

train.data <- read.table(file.path(targetFolder, 'train', 'X_train.txt'))
train.activities <- read.table(file.path(targetFolder, 'train', 'y_train.txt'))
train.subjects <- read.table(file.path(targetFolder, 'train', 'subject_train.txt'))

# Binding the rows for each of the data sets together
data.data <- rbind(train.data, test.data)
data.activities <- rbind(train.activities, test.activities)
data.subjects <- rbind(train.subjects, test.subjects)

# Combining all of of the different columns together into one table
full_data <- cbind(data.subjects, data.activities, data.data)

# 2. Extracting only the measurements on the mean and standard deviation for each measurement. 

# Grabbing the complete list of features
features <- read.table(file.path(targetFolder, 'features.txt'))

# Filtering to the features we want
requiredFeatures <- features[grep('-(mean|std)\\(\\)', features[, 2 ]), 2]
full_data <- full_data[, c(1, 2, requiredFeatures)]

# 3. Using descriptive activity names to name the activities in the data set

# Reading in the activity labels
activities <- read.table(file.path(targetFolder, 'activity_labels.txt'))

# Updating the activity name
full_data[, 2] <- activities[full_data[,2], 2]

# 4.Labelling the data set with descriptive variable names. 

colnames(full_data) <- c(
        'subject',
        'activity',
        # Remove the brackets from the features columns
        gsub('\\-|\\(|\\)', '', as.character(requiredFeatures))
)

# Coercing the data into strings
full_data[, 2] <- as.character(full_data[, 2])

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Melting the data so we have a unique row for each combination of subject and acitivites
final.melted <- melt(full_data, id = c('subject', 'activity'))

# Casting it getting the mean value
final.mean <- dcast(final.melted, subject + activity ~ variable, mean)

# Writing the data out to a text file
write.table(final.mean, file=file.path("tidy.txt"), row.names = FALSE, quote = FALSE)
