library(caret)
library(randomForest)

# load data without setting strings to factor values
df_train = read.csv('pml-training.csv', na.strings=c("NA","#DIV/0!",""))
df_test = read.csv('pml-testing.csv', na.strings=c("NA","#DIV/0!",""))

str(df_train)
str(df_test)

# limit the  variables to those that are not completely empty (NA) in the testing set
dependent_vars = c("roll_belt", "pitch_belt", "yaw_belt", "total_accel_belt", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z", "accel_belt_x", "accel_belt_y", "accel_belt_z", "magnet_belt_x", "magnet_belt_y", "magnet_belt_z", "roll_arm", "pitch_arm", "yaw_arm", "total_accel_arm", "gyros_arm_x", "gyros_arm_y", "gyros_arm_z", "accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x", "magnet_arm_y", "magnet_arm_z", "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell")
all_vars_train = c("roll_belt", "pitch_belt", "yaw_belt", "total_accel_belt", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z", "accel_belt_x", "accel_belt_y", "accel_belt_z", "magnet_belt_x", "magnet_belt_y", "magnet_belt_z", "roll_arm", "pitch_arm", "yaw_arm", "total_accel_arm", "gyros_arm_x", "gyros_arm_y", "gyros_arm_z", "accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x", "magnet_arm_y", "magnet_arm_z", "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "classe")
all_vars_test = c("roll_belt", "pitch_belt", "yaw_belt", "total_accel_belt", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z", "accel_belt_x", "accel_belt_y", "accel_belt_z", "magnet_belt_x", "magnet_belt_y", "magnet_belt_z", "roll_arm", "pitch_arm", "yaw_arm", "total_accel_arm", "gyros_arm_x", "gyros_arm_y", "gyros_arm_z", "accel_arm_x", "accel_arm_y", "accel_arm_z", "magnet_arm_x", "magnet_arm_y", "magnet_arm_z", "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "problem_id")

# keep only the vars we want to keep
# data_train = df_train[,(names(df_train) %in% all_vars_train)]
# data_test = df_test[,(names(df_test) %in% all_vars_test)]
# more elegant method
data_train = subset(df_train, select=all_vars_train)
data_test = subset(df_test, select=all_vars_test)

#Split train data in a training set and a testing set for model validation
inTrain = createDataPartition(y=data_train$classe, p=0.7, list=F)
training = data_train[inTrain,]
testing  = data_train[-inTrain,]

# GBM model
GBM = train(as.factor(classe) ~ ., method="gbm",data=training,verbose=FALSE)
# RF model
RF = train(as.factor(classe) ~ ., method="gbm",data=training)

predictGBM = predict(GBM, testing)
confusionMatrix(testing$classe, predictGBM)
predictGBM_ans = predict(GBM, data_test)

predictRF = predict(RF, testing)
confusionMatrix(testing$classe, predictRF)
predictRF_ans = predict(RF, data_test)

# answers = c("A","B","C","C","C","A","B","B","C","A","A","B","C","E","E","A","E","C","D","E")
# 
# pml_write_files = function(x){
#   n = length(x)
#   for(i in 1:n){
#     filename = paste0("problem_id_",i,".txt")
#     write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
#   }
# }
# 
# pml_write_files(answers)
