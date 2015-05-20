library(data.table)

# returns the desired feature list specified by a vector of features
getFeatures <- function(types){
    features <- fread("features.txt")
    setnames(features, c("feature_id", "feature_name"))
    types <- gsub("\\(\\)", "\\\\(\\\\)", types) # escape brackets
    features[grepl(paste(types, collapse = "|"), feature_name)] # return filtered features
}

# returns a dataset of measurements including activity and subject ids given
# the type of measurement and desired features
getData <- function(type, features){
    # set the file paths
    pathMeasur <- paste0(type, "/X_", type, ".txt")
    pathActivities <- paste0(type, "/y_", type, ".txt")
    pathSubjects <- paste0(type, "/subject_", type, ".txt")
    
    # get the measurements
    measur <- as.data.table(read.table(pathMeasur))
    measur <- measur[, features$feature_id, with = FALSE]
    setnames(measur, features$feature_name)
    
    # get the activities
    activities <- fread(pathActivities)
    setnames(activities, "activity_id")
    
    # get the subjects
    subjects <- fread(pathSubjects)
    setnames(subjects, "subject_id")
    
    # merge all together
    mergedData <- cbind(subjects, activities, measur)
    
    # add measurement type column
    mergedData$measurement_type <- type
    
    mergedData
}

# returns the dataset with the activity_id column replaced by an
# activity_name column as specified in activity_labels.txt
renameActivities <- function(x){
    activityDict <- fread("activity_labels.txt")
    setnames(activityDict, c("activity_id", "activity_name"))
    setkey(activityDict, "activity_id")
    setkey(x, "activity_id")
    x <- activityDict[x]
    setkey(x, "subject_id")
    x[, activity_id := NULL]
}

# get the desired feature list
features <- getFeatures(c("mean()", "std()"))

# # get the test data, train data and merge them together
# testData <- getData("test", features)
# trainData <- getData("train", features)
dataset <- rbindlist(list(testData, trainData))

# give activities the descriptive name
dataset <- renameActivities(dataset)
overview <- dataset[, lapply(.SD, mean), by = .(subject_id, activity_name),
                    .SDcols = features$feature_name]