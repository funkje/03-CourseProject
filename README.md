# Getting and Cleaning Data Course Project

A script cleaning the provided Samsung dataset and returning a merged dataset with averages of all specified variables. The run_analysis.R file has to be placed in the same working directory as the original dataset.

The script first goes through the features.txt and returns only the specified features (std and mean measurements). Then both test and train data are obtained through the getData() function that combines "type/X_type.txt", "type/y_type.txt" and "type/subject_type.txt" files into one data.table object with appropriate variable names. The renameActivities() function is used to replace the activity id as found in "type/y_type.txt" with the appropriate activity name. The final dataset is called overview and is created by averaging each column grouped by subject_id and activity_name. All dataset operations were written using the data.table library.

A sample of the script output is located in the overview.txt file.

A codebook can be found in the codebook.md file.