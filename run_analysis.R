inputfile1 <- c("test/X_test.txt")
inputfile2 <- c("test/y_test.txt")
inputfile3 <- c("test/subject_test.txt")
inputfile4 <- c("train/subject_train.txt")
inputfile5 <- c("train/X_train.txt")
inputfile6 <- c("train/y_train.txt")
inputfile7 <- c("activity_labels.txt")
activities_list <- read.table(inputfile7)
x_train1 <- read.table(inputfile5)
y_train1 <- read.table(inputfile6)
subject_train1 <- read.table(inputfile4)
x_test1 <- read.table(inputfile1)
y_test1 <- read.table(inputfile2)
subject_test1 <- read.table(inputfile3)
colnam <- read.table("features.txt")
colnames(x_test1) <- colnam[,2]
colnames(x_train1) <- colnam[,2]
x_test1 <- cbind(idx=1:nrow(x_test1),x_test1)
x_train1 <- cbind(idx=1:nrow(x_train1),x_train1)
y_test1 <- cbind(1:nrow(y_test1),y_test1)
colnames(y_test1) <- cbind("idx","activity")
y_train1 <- cbind(1:nrow(y_train1),y_train1)
colnames(y_train1) <- cbind("idx","activity")
subject_test1 <- cbind(1:nrow(subject_test1),subject_test1)
colnames(subject_test1)  <- cbind("idx","test_subject")
subject_train1 <- cbind(1:nrow(subject_train1),subject_train1)
colnames(subject_train1)  <- cbind("idx","test_subject")
merged_test1 <- merge(y_test1,subject_test1)
merged_test2 <- merge(merged_test1,x_test1)
merged_train1 <- merge(y_train1,subject_train1)
merged_train2 <- merge(merged_train1,x_train1)
merged_all <- rbind(merged_train2,merged_test2)
for (i in 1:nrow(merged_all)){
      old.value <- merged_all[[i,2]]
      new.value <- as.character(activities_list[old.value,2])
      merged_all[[i,2]] <- new.value
}
output_sel <- names(merged_all)
regexs <- list(c("(activity|test_subject|mean|std)","1"),c("(Freq)",""))
output_full <- cbind(col_name=names(merged_all))
output_c <- character(nrow(output_full))
for(i in seq_along(regexs)){
      output_c[grepl(x=output_full, pattern = regexs[[i]][1])] <- regexs[[i]][2]
}
output_map <- data.frame(cbind(output_sel,subset=output_c))
output_valid_col <- output_map[output_map$subset == "1",]
row.names(output_valid_col) <- c(1:nrow(output_valid_col))
col_subset_1 <- as.vector(output_valid_col[,1])
merged_subset <- try(merged_all[ , col_subset_1])
new_names <- c("Activity", "Test_Subject", "Time_Based_Body_Acceleration_Mean_X-axis", "Time_Based_Body_Acceleration_Mean_Y-axis", "Time_Based_Body_Acceleration_Mean_Z-axis", "Time_Based_Body_Acceleration_Standard_Deviation_X-axis", "Time_Based_Body_Acceleration_Standard_Deviation_Y-axis", "Time_Based_Body_Acceleration_Standard_Deviation_Z-axis", "Time_Based_Gravity_Acceleration_Mean_X-axis", "Time_Based_Gravity_Acceleration_Mean_Y-axis", "Time_Based_Gravity_Acceleration_Mean_Z-axis", "Time_Based_Gravity_Acceleration_Standard_Deviation_X-axis", "Time_Based_Gravity_Acceleration_Standard_Deviation_Y-axis", "Time_Based_Gravity_Acceleration_Standard_Deviation_Z-axis", "Time_Based_Body_Acceleration_Jerk_Mean_X-axis", "Time_Based_Body_Acceleration_Jerk_Mean_Y-axis", "Time_Based_Body_Acceleration_Jerk_Mean_Z-axis", "Time_Based_Body_Acceleration_Jerk_Standard_Deviation_X-axis", "Time_Based_Body_Acceleration_Jerk_Standard_Deviation_Y-axis", "Time_Based_Body_Acceleration_Jerk_Standard_Deviation_Z-axis", "Time_Based_Body_Gyroscopic_Mean_X-axis", "Time_Based_Body_Gyroscopic_Mean_Y-axis", "Time_Based_Body_Gyroscopic_Mean_Z-axis", "Time_Based_Body_Gyroscopic_Standard_Deviation_X-axis", "Time_Based_Body_Gyroscopic_Standard_Deviation_Y-axis", "Time_Based_Body_Gyroscopic_Standard_Deviation_Z-axis", "Time_Based_Body_Gyroscopic_Jerk_Mean_X-axis", "Time_Based_Body_Gyroscopic_Jerk_Mean_Y-axis", "Time_Based_Body_Gyroscopic_Jerk_Mean_Z-axis", "Time_Based_Body_Gyroscopic_Jerk_Standard_Deviation_X-axis", "Time_Based_Body_Gyroscopic_Jerk_Standard_Deviation_Y-axis", "Time_Based_Body_Gyroscopic_Jerk_Standard_Deviation_Z-axis", "Time_Based_Body_Acceleration_Magnitude_Mean", "Time_Based_Body_Acceleration_Magnitude_Standard_Deviation", "Time_Based_Gravity_Acceleration_Magnitude_Mean", "Time_Based_Gravity_Acceleration_Magnitude_Standard_Deviation", "Time_Based_Body_Acceleration_Jerk_Magnitude_Mean", "Time_Based_Body_Acceleration_Jerk_Magnitude_Standard_Deviation", "Time_Based_Body_Gyroscopic_Magnitude_Mean", "Time_Based_Body_Gyroscopic_Magnitude_Standard_Deviation", "Time_Based_Body_Gyroscopic_Jerk_Magnitude_Mean", "Time_Based_Body_Gyroscopic_Jerk_Magnitude_Standard_Deviation", "Frequency_Based_Body_Acceleration_Mean_X-axis", "Frequency_Based_Body_Acceleration_Mean_Y-axis", "Frequency_Based_Body_Acceleration_Mean_Z-axis", "Frequency_Based_Body_Acceleration_Standard_Deviation_X-axis", "Frequency_Based_Body_Acceleration_Standard_Deviation_Y-axis", "Frequency_Based_Body_Acceleration_Standard_Deviation_Z-axis", "Frequency_Based_Body_Acceleration_Jerk_Mean_X-axis", "Frequency_Based_Body_Acceleration_Jerk_Mean_Y-axis", "Frequency_Based_Body_Acceleration_Jerk_Mean_Z-axis", "Frequency_Based_Body_Acceleration_Jerk_Standard_Deviation_X-axis", "Frequency_Based_Body_Acceleration_Jerk_Standard_Deviation_Y-axis", "Frequency_Based_Body_Acceleration_Jerk_Standard_Deviation_Z-axis", "Frequency_Based_Body_Gyroscopic_Mean_X-axis", "Frequency_Based_Body_Gyroscopic_Mean_Y-axis", "Frequency_Based_Body_Gyroscopic_Mean_Z-axis", "Frequency_Based_Body_Gyroscopic_Standard_Deviation_X-axis", "Frequency_Based_Body_Gyroscopic_Standard_Deviation_Y-axis", "Frequency_Based_Body_Gyroscopic_Standard_Deviation_Z-axis", "Frequency_Based_Body_Acceleration_Magnitude_Mean", "Frequency_Based_Body_Acceleration_Magnitude_Standard_Deviation", "Frequency_Based_Body_Acceleration_Jerk_Magnitude_Mean", "Frequency_Based_Body_Acceleration_Jerk_Magnitude_Standard_Deviation", "Frequency_Based_Body_Gyroscopic_Magnitude_Mean", "Frequency_Based_Body_Gyroscopic_Magnitude_Standard_Deviation", "Frequency_Based_Body_Gyroscopic_Jerk_Magnitude_Mean", "Frequency_Based_Body_Gyroscopic_Jerk_Magnitude_Standard_Deviation")
colnames(merged_subset)  <- new_names
merged_subset <- merged_subset[order(merged_subset$Activity,merged_subset$Test_Subject),]
id_fmelt <- c( "Activity","Test_Subject")
obs_melt <- melt(merged_subset,id=id_fmelt)
obs_dcast <- dcast(obs_melt, Activity + Test_Subject ~ variable,mean)
write.table(obs_dcast, file="tidy_data.txt", row.name=FALSE)