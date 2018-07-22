#set the working directory (pathwd must be customized)
pathwd<-"/home/superuser/Dropbox/R/Coursera/datacleaning/final_project"
setwd(pathwd)

#download the zip file with data for analysis in working directory
ulrzipfile<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(ulrzipfile,destfile="getdata_projectfiles_UCI HAR Dataset.zip",method="curl")

#unzip the data into the working directory
unzip("getdata_projectfiles_UCI HAR Dataset.zip")

#Create the activities data frame
activities<-read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "", dec = ".")
#Set the labels for activities data frame
names(activities)=c("id_activity","activity")

#Create the features data frame
features<-read.table("UCI HAR Dataset/features.txt", header = FALSE, sep = "", dec = ".")
#Set the labels for features data frame
names(features)=c("id_feature","feature")

#From dplyr library select only the rows features that contains 
#the required mean and standard deviation values
library(dplyr)
features_required<-features %>% filter(grepl("(mean()|std())",feature)) 

#START TRAINING ANALYSIS
#I read training set, training labels and subjects
x_train<-read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "", dec = ".")
y_train<-read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "", dec = ".")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "", dec = ".")

#select only the columns that refering required mean and standard 
#deviation values:
#1) i get the row vector with position of columns of x_train data frame 
#of required values
x_train_cols_req<-t(features_required['id_feature'])
x_train_cols_req<-as.numeric(x_train_cols_req[1,])
#2) i get the row vector with names of columns of x_train data frame 
#of required values
x_train_names_cols_req<-t(features_required['feature'])
x_train_names_cols_req<-as.character(x_train_names_cols_req[1,])
#3) I select only the desired columns 
x_train<-x_train %>% select(x_train_cols_req)
#4) I set comprensible names columns
names(x_train)<-x_train_names_cols_req

#I recover only activities labels from training label
y_train<-inner_join(y_train,activities,by=c("V1"="id_activity")) %>% 
        select(activity) 

#I rename the column of subject_train
names(subject_train)=c("id_subject")

#I create the final training data frame with merging subject_train, 
#x_train and y_train calculated
training<-data.frame(subject_train,y_train,x_train)
#i add a new column "type of set" for training data frame to indicate that 
#the observations are training set
training$'type of set'<-'training'

#START TEST ANALYSIS
#I read test set, test labels and subjects of test
x_test<-read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "", dec = ".")
y_test<-read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "", dec = ".")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "", dec = ".")

#I select only the columns that refering required mean and standard 
#deviation values:
#1) i get the row vector with position of columns of x_train data frame 
#of required values
x_test_cols_req<-x_train_cols_req
#2) i get the row vector with names of columns of x_train data frame 
#of required values
x_test_names_cols_req<-x_train_names_cols_req
#3) I select only the desider columns 
x_test<-x_test %>% select(x_test_cols_req)
#4) I set comprensible names columns
names(x_test)<-x_test_names_cols_req

#I recover only activities labels from training label
y_test<-inner_join(y_test,activities,by=c("V1"="id_activity")) %>% 
        select(activity) 

#I rename the column of subject_test
names(subject_test)=c("id_subject")

#I create the final training data frame with merging subject_train, 
#x_train and y_train calculated
test<-data.frame(subject_test,y_test,x_test)

#i add a new column "type of set" for test data frame to indicate that 
#the observations are test set
test$'type of set'<-'test'

#FINAL ANALYSIS
#I merge test and training data frames to generate the final data frame
HAR<-rbind(training,test)
#I modify labels names which have been changed by rbind function
names(HAR)<-gsub("\\.\\.","()",names(HAR))
names(HAR)<-gsub("\\.","-",names(HAR))

#SUMMARIZE DATA FRAME WITH AVARAGE OF EACH VARIABLE GROUP BY ACTIVITY AND SUBJECT
HAR_MEAN<-HAR %>% select(-'type of set') %>% 
        group_by(activity,id_subject) %>% 
        summarise_all(mean)
        

















