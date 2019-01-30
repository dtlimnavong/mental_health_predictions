install.packages("tidyverse")

library(MASS)
library(randomForest)
library(randomForestExplainer)
library(tidyverse)
library(ggplot2)

### IMPORT DATASET
setwd("/Users/dylangreed/Desktop/CLASSES/ORIE4740/Project")
data = read.csv('survey.csv', header = T)

### CLEAN DATASET AND DEFINE LABEL: 3 different way to determine if individual has mental health issues

## USING treatment

## USING work_interfere
# data$work_interfere is defined as:
# "If you have a mental health condition, do you feel that it interferes with your work?"
# Assume a data entry of 'NA' corresponds to a person without a mental health condition
# and any other data entry corresponds to a person with a mental health condition
label = ifelse(is.na(data$work_interfere), "No", "Yes")
no = sum(label=="No") / length(label) *100
yes = sum(label =="Yes") / length(label)*100
# better assumption but very umbalanced dataset

# data$treatment is defined as:
# "Do you take any treatment for mental health issues"
# Assume a data entry of 'No' corresponds to a person without a mental health condition
# and  data entry of 'Yes' corresponds to a person with a mental health condition
label = data$treatment
no = sum(label=="No") / length(label) *100
yes = sum(label =="Yes") / length(label)*100

# balanced data but strong assumption

## USING treatment and work_interfere
data$work_interfere = ifelse(is.na(data$work_interfere), "0", data$work_interfere)
# 0 = NA, 1 = Never, 2 = Often, 3 = Rarely, 4 = Sometimes
# Yes = (treatment = Yes, (treatment = No, work_int = Often))
# No = ((treatment = No, work_int = Na), (treatment = No, work_int = Never), (treatment = No, work_int = Rarely),(treatment = No, work_int = Sometimes))
label = ifelse(data$treatment == "Yes", "Yes", ifelse((data$treatment == "No") & (data$work_interfere == "2"), "Yes", "No"))
no = sum(label=="No") / length(label) *100
yes = sum(label =="Yes") / length(label)*100



# convert into factor for random forest classification
label = as.factor(label)
# removing unwanted information a
#data = data[, !colnames(data) %in% c('Timestamp', 'state', 'work_interfere', 'comments')]
# removing treatment in addition (too similar to mental_health issues)
data = data[, !colnames(data) %in% c('treatment','Timestamp', 'state', 'work_interfere', 'comments')]

data = data.frame(data, label)
# removing missing values (row level)
data <- data[complete.cases(data),]

## SPLIT INTO TRAIN-TEST 
train = sample(1:nrow(data), 0.8*nrow(data))
data.test = data[-train,]
label.test = data$label[-train]
test.true = as.character(label.test)

## RANDOM FOREST: Using Out of Bag Data as Validation Set
# 
rf.data = randomForest(label~., data = data, subset = train, ntree = 100, importance = TRUE)
data.pred=predict(rf.data,data.test,type="response")
test.pred = as.character(data.pred)
test.true = as.character(label.test)

# Accuracy
error.table = table(test.pred,test.true)
tp <- error.table[2,2]
fn <- error.table[1,2]
tn <- error.table[1,1]
fp <- error.table[2,1]
error.table
# (FN+FP)/(FP+FN+TP+TN)
error.rf = (fn+fp)/(fp+fn+tp+tn)
accuracy_ov = 1 - error.rf
accuracy_ov
# label 1 = 0.23
# label 2 = 0.32
# label 3 = 0.30

# balanced accuracy: 0.5*tp/(tp+fn) + 0.5*tn/(tn+fp)
true_positive = tp / (tp +fn)
true_negative = tn / (tn + fp)
accuracy_bal = 0.5*(true_positive)+0.5*(true_negative)
accuracy_bal
# label 1 =0.50
# label 2 = 0.68
# label 3 = 0.70

## tuning parameters

# Define a named list of parameter values
ntree = c(100, 250, 500,1000,5000)
mtry = c(1,3, 5,10, 20)
cv.error_list = rep(0, length(ntree)*length(mtry))
a = 1

for (i in mtry){
  for (j in ntree){
    rf.data = randomForest(label~., data = data, subset = train, ntree = j, mtry = i, importance = TRUE)
    cv.error_list[a] = mean(rf.data$err.rate)
    a = a + 1 
  }
}
# heatmap 
df.gridsearch <- expand.grid(ntree = c("100", "250", "500","1000","5000"),
                            mtry = c("1","3", "5","10", "20")
)

# add variable: error

df.gridsearch$error <- cv.error_list


ggplot(data = df.gridsearch, aes(x = ntree, y = mtry)) +
  geom_tile(aes(fill = error)) 
xlab("number of trees")
ylab("number of features")
ggtitle("Heat mMp Graph of GridSearch for parameters optimization", subtitle = waiver())



## Implementing Threshold # parameters ntree = 500, mtry = 20
data.train = data[train,]
train_2 = sample(1:length(train), 0.5*length(train))
data.val = data.train[-train_2,]
label.val = data.train$label[-train_2]
val.true = as.character(label.val)

rf.data = randomForest(label~., data = data, subset = train_2, ntree = 500, mtry = 20, importance = TRUE)
data.prob=predict(rf.data,data.val,type="prob")

threshold = seq(0.1, 1, by=0.1)
overall_accuracyData = rep(0, length(threshold))
balancedData = rep(0, length(threshold))

for (i in 1:length(threshold)){
  survey_bin <- as.numeric(data.prob[,2] >= threshold[i])
  
  #Get the accuracy of our model
  t <- table(survey_bin,val.true)
  
  if (dim(t)[1]==1){
    if (row.names(t)[1]==0){
      tn <- t[1,1];fn <- t[1,2]
      fp <- 0; tp <- 0
      
    } else {
      tn <- 0; fn <- 0
      fp <- t[1,1]; tp <- t[1,2]
    }
    
  } else {
    fp <- t[2,1]; tn <- t[1,1]
    tp <- t[2,2]; fn <- t[1,2]
    
  } 
  
  # overall accuracy
  overall_accuracy = (tp+tn)/(tp+tn+fp+fn)
  # true positive
  true_positive = (tp)/(tp+fn)
  # true negative
  true_negative = tn/(tn+fp)
  # balanced
  balanced = 0.5*(tp)/(tp+fn)+0.5*tn/(tn+fp)
  
  overall_accuracyData[i] = overall_accuracy
  balancedData[i] = balanced
}

accuracyMax = which.max(overall_accuracyData)
balancedMax = which.max(balancedData)

plot(threshold,overall_accuracyData, type ='l', xlab = "Threshold", ylab = "Overal Accuracy", main = "Overal Accuracy vs. Threshold")
points(threshold[accuracyMax], overall_accuracyData[accuracyMax], pch = 4, col = "red", lwd = 4)

plot(threshold, balancedData, type ='l', xlab = "Threshold", ylab = "Balanced Accuracy", main = "Balanced Accuracy vs. Threshold")
points(threshold[balancedMax], balancedData[balancedMax], pch = 4, col = "blue", lwd = 4)

plot(overall_accuracyData, balancedData,  xlab = "Overal Accuracy", ylab = "Balanced Accuracy", main = "Balanced Accuracy vs. Overal Accuracy")
points(overall_accuracyData[balancedMax], balancedData[balancedMax], pch = 4, col = "blue", lwd = 4)
points(overall_accuracyData[accuracyMax], balancedData[accuracyMax], pch = 4, col = "red", lwd = 4)

final_thr = threshold[balancedMax]

# Importance

importance(rf.data)
varImpPlot(rf.data, main = "Feature Importance")

# prediction
final_thr = threshold[balancedMax]
rf.data = randomForest(label~., data = data, subset = train, ntree = 500, mtry = 20, importance = TRUE)
data.prob=predict(rf.data,data.test,type="prob")

survey_bin <- as.numeric(data.prob[,2] >= 0.7)

#Get the accuracy of our model
t <- table(survey_bin,test.true)
t
fp <- t[2,1]; tn <- t[1,1]
tp <- t[2,2]; fn <- t[1,2]

# overall accuracy
overall_accuracy = (tp+tn)/(tp+tn+fp+fn)
# true positive
true_positive = (tp)/(tp+fn)
# true negative
true_negative = tn/(tn+fp)
# balanced
balanced = 0.5*(tp)/(tp+fn)+0.5*tn/(tn+fp)
overall_accuracy
balanced

min_depth_frame <- min_depth_distribution(rf.data)

plot_min_depth_distribution(min_depth_frame)
plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)

importance_frame <- measure_importance(rf.data)
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

# https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html
explain_forest(rf.data, interactions = TRUE, data = data)

