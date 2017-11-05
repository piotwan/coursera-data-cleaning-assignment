##
## Getting and cleaning data - week 4 assignment
##

rm(list=ls())
library(reshape2)

import_and_merge <- function (FeatFname,ActFname,SubjFname,nrows=10) 
{
  feat <- read.table(FeatFname, 
                     header = FALSE,
                     nrows = nrows,
                     strip.white=TRUE)
  
  act <- read.table(ActFname,
                    header = FALSE,
                    nrows = nrows,
                    col.names = c("activity_id"))
  
  subj <- read.table(SubjFname,
                      header = FALSE,
                      nrows = nrows,
                      col.names = c("subject_id"))
  
  feat$activity_id <- act$activity_id
  feat$subject_id <- subj$subject_id
  feat
}


train_set <- import_and_merge("train/X_train.txt",
                              "train/Y_train.txt",
                              "train/subject_train.txt",
                              -1)

test_set <- import_and_merge("test/X_test.txt",
                              "test/Y_test.txt",
                              "test/subject_test.txt",
                              -1)

#Merges the training and the test sets to create one data set.

merged_set <- Reduce(function(...) merge(..., all=T), 
                         list(train_set, test_set))


#Appropriately labels the data set with descriptive variable names.
#2947+7352
features <- read.table("features.txt",
                       header = FALSE,
                       col.names = c('n','name'))
#10299

all_columns <- features;
all_columns <- rbind(all_columns,data.frame("n"=900,"name"="activity_id"))
all_columns <- rbind(all_columns,data.frame("n"=901,"name"="subject_id"))

colnames(merged_set) <- all_columns$name

#Extracts only the measurements on the mean and standard deviation for each measurement.

sel_columns <- all_columns[which(grepl("mean|std|activity_id|subject_id",all_columns$name)),]

merged_set_meanstd <- merged_set[,names(merged_set) %in% sel_columns$name]


#Uses descriptive activity names to name the activities in the data set

activities_dict <- read.table("activity_labels.txt",
                              header = FALSE,
                              col.names = c("activity_id","activity_name"))

merged_set_meanstd <- merge(merged_set_meanstd,activities_dict)

#From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for 
#each activity and each subject.

id_vars = c("activity_id","activity_name","activity_id","subject_id")
var_names = sel_columns[which( !(sel_columns$name %in% id_vars) ),]$name


merged_set_meanstd_flat <- melt(merged_set_meanstd,id.vars = id_vars,measure.vars = var_names)

agg_set <- aggregate(value ~ variable + activity_name + subject_id, merged_set_meanstd_flat, mean)

write.table(agg_set,"result_set.csv",row.names =FALSE)
