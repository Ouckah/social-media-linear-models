library(tidyverse)
library(glmnet)

# DATA READING -----------------------------------------------------------------

socialMediaData <- read_csv("./data.csv")
socialMediaData

hist(socialMediaData$`1. What is your age?`,
     main = "Distribution of Ages",
     xlab = "Age",
     ylab = "Frequency")

# -----------------------------------------------------------------------------

# DATA CLEANING ---------------------------------------------------------------

# change names
names(socialMediaData)[names(socialMediaData) == "1. What is your age?"]                                                                                                  <- "age"
names(socialMediaData)[names(socialMediaData) == "2. Gender"]                                                                                                             <- "gender"
# names(socialMediaData)[names(socialMediaData) == "6. Do you use social media?"]                                                                                         <- "uses_social_media"
names(socialMediaData)[names(socialMediaData) == "7. What social media platforms do you commonly use?"]                                                                   <- "platform_count"
names(socialMediaData)[names(socialMediaData) == "8. What is the average time you spend on social media every day?"]                                                      <- "avg_time"
names(socialMediaData)[names(socialMediaData) == "9. How often do you find yourself using Social media without a specific purpose?"]                                      <- "procrasinate_1"
names(socialMediaData)[names(socialMediaData) == "10. How often do you get distracted by Social media when you are busy doing something?"]                                <- "procrasinate_2"
names(socialMediaData)[names(socialMediaData) == "11. Do you feel restless if you haven't used Social media in a while?"]                                                 <- "addictive_1"
names(socialMediaData)[names(socialMediaData) == "12. On a scale of 1 to 5, how easily distracted are you?"]                                                              <- "procrasinate_3"
names(socialMediaData)[names(socialMediaData) == "14. Do you find it difficult to concentrate on things?"]                                                                <- "procrasinate_4"
names(socialMediaData)[names(socialMediaData) == "15. On a scale of 1-5, how often do you compare yourself to other successful people through the use of social media?"]  <- "self_image_1"
names(socialMediaData)[names(socialMediaData) == "16. Following the previous question, how do you feel about these comparisons, generally speaking?"]                     <- "self_image_2"
names(socialMediaData)[names(socialMediaData) == "17. How often do you look to seek validation from features of social media?"]                                           <- "self_image_3"
names(socialMediaData)[names(socialMediaData) == "18. How often do you feel depressed or down?"]                                                                          <- "self_image_4"
names(socialMediaData)[names(socialMediaData) == "20. On a scale of 1 to 5, how often do you face issues regarding sleep?"]                                               <- "addictive_2"

# delete unnecessary columns
socialMediaData <- socialMediaData[, -1]
socialMediaData <- socialMediaData[, -3]
socialMediaData <- socialMediaData[, -3]
socialMediaData <- socialMediaData[, -3]
socialMediaData <- socialMediaData[, -3]
socialMediaData <- socialMediaData[, -9]
socialMediaData <- socialMediaData[, -14]

# age_col <- data.frame(socialMediaData[, 2])
# age_data <- numeric(length(age_col))

# for(i in 1:481) {
#   age <- as.integer(age_col[i, ])
#   age_data[i] <- age
# }

# manipulate data
count_elements <- function(x) {
  if (is.na(x)) {
    return(0)
  } else {
    elements <- strsplit(as.character(x), ",")[[1]]
    return(length(elements))
  }
}

time_to_num <- function(x) {
  if (x == "Less than an Hour") {
    return(0.5)
  } else if (x == "Between 1 and 2 hours") {
    return(1.5)
  } else if (x == "Between 2 and 3 hours") {
    return(2.5)
  } else if (x == "Between 3 and 4 hours") {
    return(3.5)
  } else if (x == "Between 4 and 5 hours") {
    return(4.5)
  } else {
    return(5.5)
  }
}

gender_to_num <- function(x) {
  if (x == "Male") {
    return(0)
  } else if (x == "Female") {
    return(1)
  } else {
    return(2)
  }
}

# manipulate data
socialMediaData$platform_count <- sapply(socialMediaData$platform_count, count_elements)
socialMediaData$avg_time <- sapply(socialMediaData$avg_time, time_to_num)
socialMediaData$gender <- sapply(socialMediaData$gender, gender_to_num)

# -----------------------------------------------------------------------------

# MODEL BUILDING --------------------------------------------------------------

self_image_model <- lm(self_image_1 + self_image_2 + self_image_3 + self_image_4 ~ platform_count + avg_time + age + gender, data = socialMediaData)
addictive_model <- lm(addictive_1 + addictive_2 ~ platform_count + avg_time + age + gender, data = socialMediaData)
procrastination_model <- lm(procrasinate_1 + procrasinate_2 + procrasinate_3 + procrasinate_4 ~ platform_count + avg_time + age + gender, data = socialMediaData)

# -----------------------------------------------------------------------------

# LASSO MODEL BUILDING --------------------------------------------------------

# SELF IMAGE / MENTAL HEALTH MODEL
# define response variable
y <- socialMediaData$self_image_1 + socialMediaData$self_image_2 + socialMediaData$self_image_3 + socialMediaData$self_image_4

# define matrix of predictor variables
x <- data.matrix(socialMediaData[, c('platform_count', 'avg_time', 'age', 'gender')])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_self_image_lambda <- cv_model$lambda.min

best_self_image_model <- glmnet(x, y, alpha = 1, lambda = best_self_image_lambda)

# ADDICTIVE MODEL
y <- socialMediaData$addictive_1 + socialMediaData$addictive_2
x <- data.matrix(socialMediaData[, c('platform_count', 'avg_time', 'age', 'gender')])

cv_model <- cv.glmnet(x, y, alpha = 1)
best_addiction_lambda <- cv_model$lambda.min

best_addiction_model <- glmnet(x, y, alpha = 1, lambda = best_addiction_lambda)

# PROCRASTINATION MODEL
y <- socialMediaData$procrasinate_1 + socialMediaData$procrasinate_2 + socialMediaData$procrasinate_3 + socialMediaData$procrasinate_4
x <- data.matrix(socialMediaData[, c('platform_count', 'avg_time', 'age', 'gender')])

cv_model <- cv.glmnet(x, y, alpha = 1)
best_procrastination_lambda <- cv_model$lambda.min

best_procrastination_model <- glmnet(x, y, alpha = 1, lambda = best_procrastination_lambda)

# -----------------------------------------------------------------------------

# PLOTTING LINEAR REGRESSION LINES --------------------------------------------

# MODEL WITH ALL INPUT VARS
# Plot for self_image_model
plot(socialMediaData$platform_count + socialMediaData$avg_time + socialMediaData$age + socialMediaData$gender, socialMediaData$self_image_1 + socialMediaData$self_image_2 + socialMediaData$self_image_3 + socialMediaData$self_image_4,
     xlab = "", ylab = "Self Image Score",
     main = "Linear Regression for Self Image",
     ylim = c(-5, 25))

# Add linear regression line
abline(self_image_model, col = "blue")

# MODEL WITH ONLY avg_time
# Plot for self_image_model
plot(socialMediaData$avg_time, socialMediaData$self_image_1 + socialMediaData$self_image_2 + socialMediaData$self_image_3 + socialMediaData$self_image_4,
     xlab = "", ylab = "Self Image Score",
     main = "Linear Regression for Self Image",
     ylim = c(-5, 25))

# Add linear regression line
abline(self_image_model, col = "blue")

# -----------------------------------------------------------------------------

# PRINTING COST VALUES + COEFFS -----------------------------------------------

print("Self Image Cost")
best_self_image_lambda
print("Self Image Coefficients")
coef(best_self_image_model)

print("Addiction Cost")
best_addiction_lambda
print("Addiction Coefficients")
coef(best_addiction_model)

print("Procrastination Cost")
best_procrastination_lambda
print("Procrastination Coefficients")
coef(best_procrastination_model)

# -----------------------------------------------------------------------------

# READING + CLEANING REAL WORLD DATA ------------------------------------------

world_data <- read_csv("./world_data.csv")

# change names
names(world_data)[names(world_data) == "1. What is your age?"]                                                              <- "age"
names(world_data)[names(world_data) == "2. Gender"]                                                                         <- "gender"
# names(world_data)[names(world_data) == "6. Do you use social media?"]                                                     <- "uses_social_media"
names(world_data)[names(world_data) == "7. What social media platforms do you commonly use?"]                               <- "platform_count"
names(world_data)[names(world_data) == "8. What is the average time you spend on social media every day?"]                  <- "avg_time"
names(world_data)[names(world_data) == "9. How often do you find yourself using Social media without a specific purpose?"]  <- "procrasinate_1"
names(world_data)[names(world_data) == "10. How often do you get distracted by Social media when you are busy doing something?"]  <- "procrasinate_2"
names(world_data)[names(world_data) == "11. Do you feel restless if you haven't used Social media in a while?"]             <- "addictive_1"
names(world_data)[names(world_data) == "12. On a scale of 1 to 5, how easily distracted are you?"]                          <- "procrasinate_3"
names(world_data)[names(world_data) == "14. Do you find it difficult to concentrate on things?"]                            <- "procrasinate_4"
names(world_data)[names(world_data) == "15. On a scale of 1-5, how often do you compare yourself to other successful people through the use of social media?"] <- "self_image_1"
names(world_data)[names(world_data) == "16. Following the previous question, how do you feel about these comparisons, generally speaking?"]<- "self_image_2"
names(world_data)[names(world_data) == "17. How often do you look to seek validation from features of social media?"]                            <- "self_image_3"
names(world_data)[names(world_data) == "18. How often do you feel depressed or down?"]                                      <- "self_image_4"
names(world_data)[names(world_data) == "20. On a scale of 1 to 5, how often do you face issues regarding sleep?"]           <- "addictive_2"

# delete unnecessary columns
world_data <- world_data[, -1]
world_data <- world_data[, -3]
world_data <- world_data[, -3]
world_data <- world_data[, -3]
world_data <- world_data[, -3]
world_data <- world_data[, -9]
world_data <- world_data[, -14]

# manipulate data
world_data$platform_count <- sapply(world_data$platform_count, count_elements)
world_data$avg_time <- sapply(world_data$avg_time, time_to_num)
world_data$gender <- sapply(world_data$gender, gender_to_num)

# -----------------------------------------------------------------------------

# PREDICTION TESTING ----------------------------------------------------------

X <- data.matrix(world_data[c("platform_count", "avg_time", "age", "gender")])

# Use the predict function to get predictions based on the new data
predictions <- predict(best_self_image_model, newx = X)

print("Predictions")
print(predictions)
print("Y")
print(data.matrix(world_data$self_image_1 + world_data$self_image_2 + world_data$self_image_3 + world_data$self_image_4))

# -----------------------------------------------------------------------------