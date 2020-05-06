# LendingClub loan Data Analyses
# Read CSV into R and saves it in a new variable, MyData #----------------------------------------------------

#q1 <- read.csv(file="/Users/annasoloveva/OneDrive - Suffolk University/MSBA SPRING 20/FIN 830/Data Project(LendingClub)/Data/LoanStats_securev1_2019Q1.csv", header=TRUE, sep=",")
#q2 <- read.csv(file="/Users/annasoloveva/OneDrive - Suffolk University/MSBA SPRING 20/FIN 830/Data Project(LendingClub)/Data/LoanStats_securev1_2019Q2.csv", header=TRUE, sep=",")
#q3 <- read.csv(file="/Users/annasoloveva/OneDrive - Suffolk University/MSBA SPRING 20/FIN 830/Data Project(LendingClub)/Data/LoanStats_securev1_2019Q3.csv", header=TRUE, sep=",")
#MyData <- dplyr::bind_rows(q1, q2,q3)

MyData <- read.csv(file = "/Users/annasoloveva/OneDrive - Suffolk University/MSBA SPRING 20/FIN 830/Data Project(LendingClub)/Data/LendingClubSampleData.csv", header = TRUE, sep =',')
dim(MyData)

#write.csv(MyData,"/Users/annasoloveva/OneDrive - Suffolk University/MSBA SPRING 20/FIN 830/Data Project(LendingClub)/Data/TotalData.csv")

# Get the dimension of MyData - There is over 2.2 million rows (loan applications) and there is 145 data points on each loan application
#dim(MyData)
#names(MyData)

# Filter MyData and get first 10,000 observations - Not exactly a good way to proceeed: however it is not the goal of the project.
df <- head(MyData,10000)
dim(df) 
names(df)

# Optional: Remove MyData - to empty some space in R environment
# rm(MyData)

# Data Summary
summary(df$loan_amnt)
summary(as.numeric(sub("%","",df$int_rate))/100)
summary(df$open_acc_6m)
summary(df$term)

# changing the regression variables to the appropriate type
df$int_rate <- as.numeric(sub("%","",df$int_rate))/100
df$revol_util <- as.numeric(sub("%","",df$revol_util))/100

# Correlation matrix
library("corrplot")
library("psych")
library(dplyr)
library(treemap)

df$grade <- as.numeric(df$grade)
df$revol_util <- as.numeric(df$revol_util)
df$open_acc_6m<- as.numeric(df$open_acc_6m)

df$revol_util

cordata <- select (df, c(int_rate,revol_util, open_acc_6m,last_fico_range_low,annual_inc))
cormat <-cor(cordata)

#Correlation Plot

corrplot(cormat, method="number",tl.col = "black")


#Stepwise Selection

library(MASS)
stepwise <- lm(int_rate ~ loan_amnt + fico_range_low + last_fico_range_low + total_rev_hi_lim + inq_fi, data = df)
step <- stepAIC(stepwise, direction = "both")
step$anova
summary(step)

# Run a Linear Regression trying to model interest rate on a loan by 2 variables
# Model 1
fit <- lm(formula = int_rate ~ revol_util + open_acc_6m, data = df)
summary(fit)                   


#Model 2 - Linear Regression trying to model interest rate on a loan by 3 variables
fit_1 <- lm(formula = int_rate ~ revol_util + open_acc_6m + last_fico_range_low, data = df)
summary(fit_1)


#Model 3 -Linear Regression trying to model interest rate on a loan by 4 variables
fit_2 <- lm(formula = int_rate ~ revol_util + open_acc_6m + last_fico_range_low + annual_inc, data = df)
summary(fit_2)

# Which model is the best, fit, fit_1, fit_2?
# As the model fitted to the entire dataset, we cannot rely on the model results. Therefore, data particion is reqired.


# Defining training and test samples randomly
# runif(n, min = 0, max = 1)
test_obs <- runif(1000,1,10000)
test_obs <- trunc(test_obs,0)
df_test <- unique(df[test_obs, ])
df_training <-unique(df[-test_obs, ])

# Number of rows in the test data set
nrow(df_test)    
# Number of rows in the training data set
nrow(df_training) 


# Linear Regression model using training data set
# Model 1 - two variables
fit_4 <- lm(formula = int_rate ~ revol_util + open_acc_6m , data = df_training)
summary(fit_4)

# Model 2 - three variables
fit_5 <- lm( formula = int_rate ~ revol_util + open_acc_6m + last_fico_range_low, data = df_training)
summary(fit_5)


#Model 3 - four variables
fit_6 <- lm(formula = int_rate ~ revol_util + open_acc_6m + last_fico_range_low + annual_inc, data = df_training)
summary(fit_6)

#Estimate the errors using test sample

# Defining a vector of zeros for errors
error <- rep(nrow(df_test),0)

# Calculating estimation errors for test data set 
for (i in 1:nrow(df_test)) {
  est_int_rate <- summary(fit_6)$coefficients[1,1] +
    summary(fit_6)$coefficients[2,1] * df_test$revol_util[i] +
    summary(fit_6)$coefficients[3,1] * df_test$open_acc_6m[i] #+
    #summary(fit_6)$coefficients[4,1] * df_test$last_fico_range_low[i] #+
    #summary(fit_6)$coefficients[5,1] * df_test$annual_inc[i]
  error[i] <- df_test$int_rate[i] - est_int_rate
}

# Take the average of out of sample estimation erros
mean(error, na.rm = TRUE)

# Calculate mean squared error (MSE)
mse <- mean(error^2, na.rm = TRUE)
mse

# For 10,000 set
# Model 1 Mean (error)= 0.002827787 or MSE = 0.002551908
# Model 2 Mean (error)= 0.001450521 or MSE = 0.002283006
# Model 3 Mean (error)= 0.001494674 or MSE = 0.002235607


# Is error variable that we caculated a good measure of performance? 
# Mean Squared Error would be a better measure of a Linear Regression problems

# How would you change the above loop to test fit_1 model's out of sample performance? 
# In order to run fit_1 model with out sample performance, we need to create a new LR - fit_4 which fits the traininf data and than run the loop again but chanhing fit_4 and adding additional variable

# K-Nearest Neighbour (KNN) #----------------------------------------------------------------------------------------------------------
# Classify loan applications into grades

# Install and load data.table package to run KNN
install.packages("class")
library("class")
library(caret)

# Two and four variables are used to classify the Grade, in total four models
# KNN: try 4 (2*2) different specifications, 2 sets of inputs and 2 values for K.


# First we make sure we remove applications with NA values in both the training and the test dataset
df_training_knn <- df_training[which(!is.na(df_training$int_rate) & !is.na(df_training$revol_util) &  !is.na(df_training$last_fico_range_low) & !is.na(df_training$open_acc_6m) & !is.na(df_training$annual_inc)),]
df_test_knn <- df_test[which(!is.na(df_test$revol_util) & !is.na(df_test$last_fico_range_low) & !is.na(df_test$open_acc_6m) & !is.na(df_test$annual_inc)),]

# cl is the true classification of the training data
cl <- df_training_knn[,c('grade')]

# test_cl is the true classification of the test data
test_cl <- df_test_knn[,c('grade')]

# Keeping only the relevant variables
df_training_knn <- df_training_knn[,c('revol_util','last_fico_range_low','open_acc_6m','annual_inc')]
df_test_knn <- df_test_knn[,c( 'revol_util','last_fico_range_low','open_acc_6m','annual_inc')]

#Apply KNN - k can change - knn_classification stores the classifications generated by KNN
knn_cl <- knn(df_training_knn, df_test_knn, cl, k = 10, l = 0, prob = FALSE, use.all = TRUE)

#Compare classification that KNN generates with true classification of the test dataset. 
#How many loan applications could be correctly classified using KNN??
knn_performance <- knn_cl == test_cl

sum(knn_performance)
# K=5, n=4, 2079

summary(knn_performance)
# K=5, n=4 
#    Mode   FALSE    TRUE 
# logical    4184    2079 

mean(knn_cl == test_cl)
#K=5, n=4, 0.3319495

# In order to build Confusion Matrix, data types should be the same for both actual and predicted data sets
test_cl <- as.factor(test_cl)
typeof(test_cl)
typeof(knn_cl)

#Building a Confusion Matrix
confusionMatrix(knn_cl, test_cl)


# Neural Networks  # --------------------------------------------------------------------------------------
# ANN: try 4 (2*2) different specifications, 2 sets of inuputs and 2 different network layouts (e.g. 1 hidden layer with 5 nodes and 2 hidden layers with 5 and 3 nodes.)
install.packages("neuralnet")
library("neuralnet")

# For the sake of speed, only 10,000 observations used
df_training_nn <- head(df_training,10000)


# First we make sure we remove applications with NA values in both the training and the test dataset

# Removing observations with NAs
df_training_nn <- df_training[which(!is.na(df_training$int_rate) & !is.na(df_training$revol_util) & !is.na(df_training$last_fico_range_low) & !is.na(df_training$open_acc_6m) & !is.na(df_training$annual_inc)),]
df_test_nn <- df_test[which(!is.na(df_test$revol_util) & !is.na(df_test$last_fico_range_low) & !is.na(df_test$open_acc_6m) & !is.na(df_test$annual_inc)),]

# Building ANN
# hidden - a vector of integers specifying the number of hidden neurons (vertices) in each layer.
# stepmax - the maximum steps for the training of the neural network
# rep - the number of repetitions for the neural network's training
# linear.output - (Regresssion = True, Classification = False) If act.fct should not be applied to the output neurons set linear output to TRUE, otherwise to FALSE.
# act.fct - a differentiable function that is used for smoothing the result of the cross product of the covariate or neurons and the weights. Additionally the strings, 'logistic' and 'tanh' are possible for the logistic function and tangent hyperbolicus.

nn <- neuralnet(int_rate ~ revol_util + last_fico_range_low +  open_acc_6m +annual_inc,
                data=df_training_nn, 
                hidden= 2,
                linear.output=T, 
                stepmax=1e6)

#Plot the network
plot(nn)


# Using the trained network on the test data to predict interest rate
predict_test_nn <- predict(nn, df_test_nn[, c('revol_util','last_fico_range_low','open_acc_6m','annual_inc')])

# Neural Network Performance - Compare NN prediction with true interest rate
nn_error <- predict_test_nn - df_test_nn$int_rate
mean(nn_error^2)   # returns the average error for the above neural network model on the test data 

# Is average error a good measure?? 
# MSE would be a better measure

# Model 1 (2 variables - one hidden layer of 2 nodes):       MSE  0.002518
# Model 2 (2 variables - two hidden layers of 5 and 2 nodes) MSE  0.002519
# Model 3 (4 variables - one hidden layer of 2 nodes)        MSE  0.002516
# Model 4 (4 variables - two hidden layers of 5 and 2 nodes) MSE  0.002518

