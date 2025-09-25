# ML
install.packages("ISLR")
# Load the ISLR library
library(ISLR)
# Load the Smarket dataset
data(Smarket)
summary(Smarket)
# Fit the logistic regression model
logit_model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
 data = Smarket, family = binomial)
# View model summary
summary(logit_model)
logit_probs <- predict(logit_model, type = "response")
# Convert probabilities to class labels (Up if prob > 0.5)
logit_pred <- ifelse(logit_probs > 0.5, "Up", "Down")
# Confusion matrix
logit_cm <- table(Predicted = logit_pred, Actual = Smarket$Direction)
print(logit_cm)
# Calculate and print accuracy
logit_accuracy <- mean(logit_pred == Smarket$Direction)
print(paste("Logistic Regression Accuracy:", round(logit_accuracy, 3)))

 LDA
library(ISLR)
library(MASS) # for LDA
data(Smarket)
summary(Smarket)
train <- Smarket$Year < 2005
test <- Smarket$Year == 2005
lda_model <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
print(lda_model)
lda_pred <- predict(lda_model, newdata = Smarket[test, ])
confusion <- table(Predicted = lda_pred$class, Actual = Smarket$Direction[test])
print(confusion)
accuracy <- mean(lda_pred$class == Smarket$Direction[test])
print(paste("LDA Accuracy:", round(accuracy * 100, 2), "%"))


# Install required packages if not already installed
# install.packages("ISLR")
# install.packages("ggplot2")
library(ISLR)
library(ggplot2)
# Load the Wage dataset
data(Wage)
# Fit a 4th-degree polynomial regression model
fit_poly <- lm(wage ~ poly(age, 4), data = Wage)
# View summary
summary(fit_poly)
# Create the base plot with polynomial fit
ggplot(Wage, aes(x = age, y = wage)) +
geom_point(alpha = 0.3, color = "blue") + # Scatter plot of original data
stat_smooth(method = "lm", formula = y ~ poly(x, 4), color = "red", size = 1.2) +
labs(title = "4th Degree Polynomial Regression of Wage on Age",
x = "Age",
y = "Wage") +
theme_minimal()

 # Install if not already
# install.packages("rpart")
# install.packages("rpart.plot")
library(ISLR)
library(rpart)
library(rpart.plot)
# Prepare the data
data(Carseats)
Carseats$High <- as.factor(ifelse(Carseats$Sales > 8, "Yes", "No"))
# Fit classification tree using rpart
tree_model <- rpart(High ~ . - Sales, data = Carseats, method = "class")
# Plot using rpart.plot (cleaner visualization)
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE, cex = 0.7)

 Support vector machine
# Practical No. 6 : Support Vector Classifier
# Install required packages (run only once)
# install.packages("ISLR")
# install.packages("e1071")
# Load libraries
library(ISLR)
library(e1071)
# Load dataset
data("Carseats")
# Convert Sales into binary (High / Low)
Carseats$High <- ifelse(Carseats$Sales > 8, "Yes", "No")
Carseats$High <- as.factor(Carseats$High)
# Drop original Sales column
Carseats <- Carseats[, -which(names(Carseats) == "Sales")]
# Split into train and test (70/30)
set.seed(123)
index <- sample(1:nrow(Carseats), nrow(Carseats) * 0.7)
train_data <- Carseats[index, ]
test_data <- Carseats[-index, ]
# Fit SVM model (linear kernel)
svm_model <- svm(High ~ ., data = train_data, kernel = "linear", cost = 1, scale = TRUE)
# Model summary
summary(svm_model)
# Predictions on test data
pred <- predict(svm_model, test_data)
# Confusion matrix
confusion_matrix <- table(Predicted = pred, Actual = test_data$High)
print(confusion_matrix)
# Accuracy
accuracy <- mean(pred == test_data$High)
cat("Accuracy: ", accuracy, "\n")
# --- Visualization (Optional) ---
# Only works with two predictors
svm_plot <- svm(High ~ Price + Income, data = train_data, kernel = "linear", cost = 1)
plot(svm_plot, train_data[, c("Price", "Income", "High")])
