library(dplyr)

#read in the data
features <- read.table('./features.txt')
subject_test <- read.table('./test/subject_test.txt', col.names = "subject")
X_test <- read.table('./test/X_test.txt', col.names = features$V2)
y_test <- read.table('./test/y_test.txt', col.names = "activity")
subject_train <- read.table('./train/subject_train.txt', col.names = "subject")
X_train <- read.table('./train/X_train.txt', col.names = features$V2)
y_train <- read.table('./train/y_train.txt', col.names = "activity")

# merge the test and training data

test <- cbind(subject_test, X_test, y_test)
train <- cbind(subject_train, X_train, y_train)
merged <- rbind(test, train)

# get the colum names that will be selected
selectedvars <- unique(grep("mean|std", names(merged), value=T))
selectedname <- c(selectedvars, "subject", "activity")

# Extract the desired colums
selected <- merged[, names(merged) %in% selectedname]

# name the activities
labeled <- within(selected, activity <- factor(activity, 
                                            labels = c("WALKING", "WALKING_UPSTAIRS", 'WALKING_DOWNSTAIRS', 
                                                       'SITTING', 'STANDING', 'LAYING')))

# create a tidy dataset as instructed
means <- labeled %>% group_by(subject, activity) %>% summarise_each(funs(mean))

# write the result in a text file
write.table(means, file = "./results.txt", row.names=F)