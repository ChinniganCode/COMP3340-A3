library(e1071)
# Load the data
data <- read.csv("datasets/USPresidency.csv", header=TRUE)

# Split data into features and target
X <- data[,2:(ncol(data)-1)]
y <- data$Target


# Train an SVM with a linear kernel
svm_model <- svm(x=X, y=y, kernel="linear", cost=10, scale=FALSE)

# print(svm_model)
# Predict on the same data
y_pred <- predict(svm_model, newdata=X)

# print(y_pred)
# Calculate accuracy
accuracy <- sum(y_pred == y) / length(y)


# Print accuracy
print(accuracy)

formatted_data  <-data[,2:ncol(data)]

print(formatted_data)
f_svm <- svm(Target~., data = formatted_data)
plot(f_svm, formatted_data,  Q1 ~ Q2,
     slice = list(Q3 = 3, Q4 = 4))



data(iris)
m2 <- svm(Species~., data = iris)
#
# plot(m2, iris, Petal.Width ~ Petal.Length,
#      slice = list(Sepal.Width = 3, Sepal.Length = 4))
