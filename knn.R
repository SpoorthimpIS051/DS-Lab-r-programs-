# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("class")

# Loading package
library(e1071)
library(caTools)
library(class)

# Loading data
dt = read.csv('heart.csv')
head(dt)

# Splitting data into train
# and test data
split <- sample.split(dt, SplitRatio = 0.7)
train_cl <- subset(dt, split == "TRUE")
test_cl <- subset(dt, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:14])
test_scale <- scale(test_cl[, 1:14])

# Fitting KNN Model
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$target,
                      k = 1)
classifier_knn

# Confusiin Matrix
cm <- table(test_cl$target, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$target)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$target,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$target)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$target,
                      k = 5)
misClassError <- mean(classifier_knn != test_cl$target)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$target,
                      k = 7)
misClassError <- mean(classifier_knn != test_cl$target)
print(paste('Accuracy =', 1-misClassError))

# K = 15
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$target,
                      k = 15)
misClassError <- mean(classifier_knn != test_cl$target)
print(paste('Accuracy =', 1-misClassError))

# K = 19
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$target,
                      k = 19)
misClassError <- mean(classifier_knn != test_cl$target)
print(paste('Accuracy =', 1-misClassError))



