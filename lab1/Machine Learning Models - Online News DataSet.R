# Lab 1: Classification Techniques - Online News Popularity Data Set 


###### -------------------------------------------------- ######

#### setting the working directory
setwd("C:/Users/zusha01/Desktop/HU Analytics/Sem 3/ANLY 530 Machine Learning 1/Week 6/Lab 1")


#### installing required packages
#install.packages("C50")
#install.packages("gmodels")
#install.packages("kernlab")
#install.packages("rpart")
#install.packages("rpart.plot")


### Step 1: Collecting the Data & Data- Pre-Processing- Common for all Methods
#### loading the data
olnewspop <- read.csv("OnlineNewsPopularity.csv")
#### checking the structure of the data
str(olnewspop)
#### checking NA's
sum(is.na(olnewspop))
#### removing non-predictive variables from the data
popnews <- olnewspop[,3:61]
str(popnews)


## Method 1: Tree-Based Classification 


#### summarizing the shares variable
summary(popnews$shares)

#### taking the median and creating a  variable to indicate whether the online news is popular or not
popnews$popular<- as.factor(ifelse(popnews$shares>1400, "yes", "no"))
str(popnews)

#### randomizing the data
set.seed(12345)
popnews_rand <- popnews[order(runif(39644)),]
str(popnews_rand)

#### creating training & testing dataset (75% observation allocated to training)
popnews_train <- popnews_rand[1: 29733, ]
popnews_test <- popnews_rand[29734:39644, ]


#### checking the percentage of split in train and test dataset
prop.table(table(popnews_train$popular))
prop.table(table(popnews_test$popular))


#loading the library
library(C50)


#### traing the model
#### creating & calling the model
popnews_model <- C5.0.default(x = popnews_train[c(-59, -60)], y= popnews_train$popular)
popnews_model


#### examining the decision tree
summary(popnews_model)

### Evaluating Model Performance

#### fitting the model onto test dataset
popnews_pred <- predict(popnews_model, popnews_test)

#### creating a confusion matrix to evaulate the performance
#### installing and loading gmodels package
library(gmodels)

#### creating confusion matrix
CrossTable(popnews_test$popular, popnews_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c("Actual Popular", "Predicted Popular"))

###### -------------------------------------------------- ######
## Method 2: Support Vector Machines


#### installing and loading the kernlab package
library(kernlab)

#### converting shares to factors, data preparation
popnews$Fshares <- as.factor(popnews$shares)
str(popnews)
news <- popnews[, c(1:58,61)]
str(news)

#### creating training & testing dataset (75% observation allocated to training)
news_train <- news[1: 29733, ]
news_test <- news[29734:39644, ]

#### checking the percentage of split in train and test dataset
prop.table(table(news_train$Fshares))
prop.table(table(news_test$Fshares))

#### creating the model
news_classifier <- ksvm(Fshares~., data= news_train, kernel = "vanilladot")
news_classifier

#### fitting the model onto test dataset
news_predictions <- predict(news_classifier, news_test)

#### confusion table
table(news_predictions, news_test$Fshares)

#### summarizing the predictions
agreement <- news_predictions == newss_test$Fshares
table(agreement)



###### -------------------------------------------------- ######
## Method 3: Adding Regression to Trees


#### splitting the data into training and test
N_train <- popnews[1: 29733, ]
N_test <- popnews[29734:39644, ]

### Step 3: Training the model on data


#### installing and loading the rpart package

library(rpart)

#### creating the model
m.rpart <- rpart(popular ~ ., data=N_train)
m.rpart

#### creating the tree plot

library(rpart.plot)

#### plot 1
rpart.plot(m.rpart, digits = 3)

#### plot 2
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type=3, extra = 101)

### Step 4: Evaluating Model Performance


#### fitting the model onto test dataset
p.rpart <- predict(m.rpart, N_test)


#### summarizing the model
summary(p.rpart)
summary(N_test$popular)

