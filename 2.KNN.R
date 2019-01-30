library("class")

# Read data from csv file
data = read.csv('survey.csv', header = T)

# data$work_interfere is defined as:
# "If you have a mental health condition, do you feel that it interferes with your work?"
# Assume a data entry of 'NA' corresponds to a person without a mental health condition
# and any other data entry corresponds to a person with a mental health condition
illness = ifelse(is.na(data$work_interfere), 0, 1)

# Remove irrelevant data and add whether a person has a mental health condition
data = data[, !colnames(data) %in% c('Timestamp', 'state', 'work_interfere', 'comments')]

data = data.frame(data, illness)

# Convert factors to dummy variables
data$Gender = model.matrix( ~ Gender - 1, data=data )
data$Country = model.matrix( ~ Country - 1, data=data )
data$self_employed = model.matrix( ~ self_employed - 1, data=data )
data$family_history = model.matrix( ~ family_history - 1, data=data )
data$treatment = model.matrix( ~ treatment - 1, data=data )
data$no_employees = model.matrix( ~ no_employees - 1, data=data )
data$remote_work = model.matrix( ~ remote_work - 1, data=data )
data$tech_company = model.matrix( ~ tech_company - 1, data=data )
data$benefits = model.matrix( ~ benefits - 1, data=data )
data$care_options = model.matrix( ~ care_options - 1, data=data )
data$wellness_program = model.matrix( ~ wellness_program - 1, data=data )
data$seek_help = model.matrix( ~ seek_help - 1, data=data )
data$anonymity = model.matrix( ~ anonymity - 1, data=data )
data$leave = model.matrix( ~ leave - 1, data=data )
data$mental_health_consequence = model.matrix( ~ mental_health_consequence - 1, data=data )
data$phys_health_consequence = model.matrix( ~ phys_health_consequence - 1, data=data )
data$coworkers = model.matrix( ~ coworkers - 1, data=data )
data$supervisor = model.matrix( ~ supervisor - 1, data=data )
data$mental_health_interview = model.matrix( ~ mental_health_interview - 1, data=data )
data$phys_health_interview = model.matrix( ~ phys_health_interview - 1, data=data )
data$mental_vs_physical = model.matrix( ~ mental_vs_physical - 1, data=data )
data$obs_consequence = model.matrix( ~ obs_consequence - 1, data=data )

# Normalize data
data_normal <- scale(data[,!names(data) %in% 'illness'])

# Split data into training and test set
set.seed(1)
train_index <- sample(1:nrow(data), 4/5*nrow(data))
data_train <- data_normal[train_index, ]
data_test <- data_normal[-train_index, ]

data_train_illness = data$illness[train_index]
data_test_illness = data$illness[-train_index]

# Train k-NN model
K = c(1, 3, 5, 10, 20, 50, 100, 200, 300)
error = rep(0,6)
for (i in (1:length(K))){
  set.seed(1);
  knn.pred = knn(data_train, data_test, data_train_illness, k=K[i]);
  
  # Use balanced error since data is heavily skewed towards positives
  t = table(knn.pred, data_test_illness)
  tp = t[2,2]
  fn = t[1,2]
  tn = t[1,1]
  fp = t[2,1]
  error[i] = 0.5 * ( (tp / (tp + fn)) + (tn / (tn + fp)) )
}
error
