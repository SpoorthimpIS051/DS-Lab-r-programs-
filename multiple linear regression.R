
# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('heart.csv')

head(dataset)
str(dataset)
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$target, SplitRatio = 0.7)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = target ~ .,
               data = training_set)
regressor
# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred
summary(regressor)

regressor = lm(formula = target ~ (sex+cp+exang+ca),
               data = training_set)
summary(regressor)
