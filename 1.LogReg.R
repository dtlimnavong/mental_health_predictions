#setwd("~/Documents/Cornell/SEM 7/ORIE 4740/Project")
install.packages('glmnet')
install.packages("ISLR")
install.packages('leaps')
install.packages('ROCR')
install.packages('grid')
install.packages('broom')
install.packages('caret')
install.packages('tidyr')
install.packages('dplyr')
install.packages('scales')
install.packages('ggplot2')
install.packages('data.table')
library(ISLR)
library(leaps)
library(glmnet)
library(ROCR)
library(grid)
library(broom)
library(caret)
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(data.table)
library(ROCR)


survey <- read.csv("survey.csv")
survey <- survey[, !colnames(survey) %in% c('comments', 'state', 'Timestamp', 'Country')]

# WE ASSUME THAT NA IN WORK_INTERFERE MEAN NO MENTAL HEALTH PROBLEMS;
# WE CREATE A COLUMN mental_health_issues WITH THIS INFO (CHANGING NA TO "None")
# CHANGE COLUMN mental_health_issues TO VARIABLE ment_iss

survey$mental_health_issues = survey$work_interfere
## Change NA to "None" in mental_health_issues
# Get levels and add "None"
levels <- levels(survey$mental_health_issues)
levels[length(levels) + 1] <- "No"
levels[length(levels) + 1] <- "Yes"


# refactor Species to include "None" as a factor level
# and replace NA with "None"
survey$mental_health_issues <- factor(survey$mental_health_issues, levels = levels)
survey$mental_health_issues[is.na(survey$mental_health_issues)] <- "No"

# Convert mental_health_issues to BINARY
survey$mental_health_issues[survey$mental_health_issues == "Rarely"] <- "Yes"
survey$mental_health_issues[survey$mental_health_issues == "Often"] <- "Yes"
survey$mental_health_issues[survey$mental_health_issues == "Sometimes"] <- "Yes"
survey$mental_health_issues[survey$mental_health_issues == "Never"] <- "Yes"


# Get levels and add "None"
levels <- levels(survey$mental_health_issues)
levels <- levels(droplevels(survey$mental_health_issues, exclude=c('Rarely', 'Sometimes', 'Often', 'Never')))
survey$mental_health_issues <- factor(survey$mental_health_issues, levels = levels)
survey <- survey[, !colnames(survey) %in% c('work_interfere')]  ## Deleting it because it's the same as mental_health_issues column I created

#### UNCOMMENT FOLLOWING CODE BASED ON WHAT YOU WANT TO ANALYZE


###   QUESTION 3: Which predictors are the most important in determining whether someone has mental health problems?   ###


# # Use only Age
# survey <- survey[, colnames(survey) %in% c('Age', 'mental_health_issues')]

# # Use only Gender
# survey <- survey[, colnames(survey) %in% c('Gender', 'mental_health_issues')]

# # Use only Country
# survey <- survey[, colnames(survey) %in% c('Country', 'mental_health_issues')]

# # Use only self_employed
# survey <- survey[, colnames(survey) %in% c('self_employed', 'mental_health_issues')]

# # Use only Family history
# survey <- survey[, colnames(survey) %in% c('family_history', 'mental_health_issues')]

# # Use only treatment
# survey <- survey[, colnames(survey) %in% c('treatment', 'mental_health_issues')]

# # Use only work_interfere
# survey <- survey[, colnames(survey) %in% c('work_interfere', 'mental_health_issues')]

# # Use only no_employees
# survey <- survey[, colnames(survey) %in% c('no_employees', 'mental_health_issues')]

###   QUESTION 1: Determine if tech has a disproportionate number of mental health cases   ###
# # Use only tech_company 
# survey <- survey[, colnames(survey) %in% c('tech_company', 'mental_health_issues')]

# # Use only benefits
# survey <- survey[, colnames(survey) %in% c('benefits', 'mental_health_issues')]

# # Use only care_options
# survey <- survey[, colnames(survey) %in% c('care_options', 'mental_health_issues')]

# # Use only wellness_program
# survey <- survey[, colnames(survey) %in% c('wellness_program', 'mental_health_issues')]

# # Use only care_options
# survey <- survey[, colnames(survey) %in% c('care_options', 'mental_health_issues')]

# # Use only seek_help
# survey <- survey[, colnames(survey) %in% c('seek_help', 'mental_health_issues')]

# # Use only anonymity
# survey <- survey[, colnames(survey) %in% c('anonymity', 'mental_health_issues')]

# # Use only leave
# survey <- survey[, colnames(survey) %in% c('leave', 'mental_health_issues')]

# # Use only mental_health_consequence
# survey <- survey[, colnames(survey) %in% c('mental_health_consequence', 'mental_health_issues')]

# # Use only phys_health_consequence
# survey <- survey[, colnames(survey) %in% c('phys_health_consequence', 'mental_health_issues')]

# # Use only coworkers
# survey <- survey[, colnames(survey) %in% c('coworkers', 'mental_health_issues')]

# # Use only supervisor
# survey <- survey[, colnames(survey) %in% c('supervisor', 'mental_health_issues')]

# # Use only mental_health_interview
# survey <- survey[, colnames(survey) %in% c('mental_health_interview', 'mental_health_issues')]

# # Use only phys_health_interview
# survey <- survey[, colnames(survey) %in% c('phys_health_interview', 'mental_health_issues')]

# # Use only mental_vs_physical
# survey <- survey[, colnames(survey) %in% c('mental_vs_physical', 'mental_health_issues')]

# # Use only obs_consequence
# survey <- survey[, colnames(survey) %in% c('obs_consequence', 'mental_health_issues')]


# Training and Test datasets
set.seed(1)
train_ind <- sample(1:nrow(survey), 2/3*nrow(survey))
survey_train <- survey[train_ind, ]
survey_test <- survey[-train_ind, ]

# Logistic Regression
survey_logit <- glm(mental_health_issues~., data = survey_train, family = binomial)

# Summary of Fitted Model
summary(survey_logit)

# Predictions
survey_pred <- predict(survey_logit, survey_test, type = "response")

threshold = seq(0, 1, by=0.01)
overall_accuracyData = c()
balancedData = c()
new_threshold = c()

for (i in threshold) {
  #ERROR HANDLING
  possibleError <- tryCatch({
    #Get binary prediction with threshold
    survey_bin <- as.numeric(survey_pred >= i)
    
    #Get the accuracy of our model
    t <- table(survey_bin, survey_test$mental_health_issues)
    tp <- t[2,2]
    fn <- t[1,2]
    tn <- t[1,1]
    fp <- t[2,1]
    # overall accuracy
    overall_accuracy = (tp+tn)/(tp+tn+fp+fn)
    # true positive
    true_positive = (tp)/(tp+fn)
    # true negative
    true_negative = tn/(tn+fp)
    # balanced
    balanced = 0.5*(tp)/(tp+fn)+0.5*tn/(tn+fp)
    
    # Create data for Max Accuracy
    new_threshold = c(new_threshold, i)
    overall_accuracyData = c(overall_accuracyData, overall_accuracy)
    balancedData = c(balancedData, balanced)
  }
  ,
  error=function(e) {
  }
  )
  
  if(inherits(possibleError, "error")) next
}

plot(new_threshold, overall_accuracyData)
plot(new_threshold, balancedData)
overallMax = which.max(overall_accuracyData)
balancedMax = which.max(balancedData)

#Get binary prediction with threshold
best_threshold = new_threshold[balancedMax]
survey_bin <- as.numeric(survey_pred >= best_threshold)

#Get the accuracy of our model
t <- table(survey_bin, survey_test$mental_health_issues)
tp <- t[2,2]
fn <- t[1,2]
tn <- t[1,1]
fp <- t[2,1]
# overall accuracy
overall_accuracy = (tp+tn)/(tp+tn+fp+fn)
overall_accuracy
# true positive
true_positive = (tp)/(tp+fn)
# true negative
true_negative = tn/(tn+fp)
# balanced
balanced = 0.5*(tp)/(tp+fn)+0.5*tn/(tn+fp)
balanced

# ### QUESTION 2: Among those with mental health issues, determine if whether they sought treatment or not is related to the way the company treats mental health   ###

# Use variables that related to how an employer treats mental health:
# treatment, benefits, wellness_program, seek_help, anonymity, mental_health_consequence,
# phys_health_consequence, coworkers, supervisor, mental_vs_physical, obs_consequence


# survey <- survey[, colnames(survey) %in% c('mental_health_issues','treatment','benefits','wellness_program','seek_help','anonymity','mental_health_consequence','phys_health_consequence','coworkers','supervisor','mental_vs_physical','obs_consequence')]
# 
# yes_ment_probs = survey[ !survey$mental_health_issues %in%  c('No'), ]
# no_ment_probs = survey[ !survey$mental_health_issues %in%  c('Yes'), ]
# 
# yes_ment_probs = survey[ , !colnames(yes_ment_probs) %in% c('mental_health_issues') ]
# no_ment_probs = survey[ , !colnames(no_ment_probs) %in% c('mental_health_issues') ]
# 
# survey = yes_ment_probs
# set.seed(1)
# train_ind <- sample(1:nrow(survey), 2/3*nrow(survey))
# survey_train <- survey[train_ind, ]
# survey_test <- survey[-train_ind, ]
# 
# survey_logit_yes_ment_probs <- glm(treatment~., data = survey_train, family = binomial)
# summary(survey_logit_yes_ment_probs)








