# Lab 1: Classification Techniques

###### -------------------------------------------------- ######
## Method 1: Tree-Based Classification 



### Step 1: Collecting the Data


#### setting the working directory
setwd("C:/Users/zusha01/Desktop/HU Analytics/Sem 3/ANLY 530 Machine Learning 1/Week 6/Lab 1")

#### loading the data
credit <- read.csv("credit.csv")

#### checking the structure of the data
str(credit)


### Step 2: Exploring the Data


#### summarizing the amount variable
summary(credit$amount)

#### evaluating number of loans defaulted vs not
table(credit$default)

#### randomizing the data
set.seed(12345)
credit_rand <- credit[order(runif(1000)),]

#### evaluating there are no substantive changes in the data
summary(credit$amount)
summary(credit_rand$amount)

#### creating training & testing dataset (90% observation allocated to training)
credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]

#### checking the percentage of split in train and test dataset
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

### Step 3: Training the model on Data



#### getting the C50 package
install.packages("C50")
library(C50)

#### creating & calling the model
credit_model <- C5.0.default(x = credit_train[-17], y= credit_train$default)
credit_model

#### examining the decision tree
summary(credit_model)

### Step 4: Evaluating Model Performance


#### fitting the model onto test dataset
cred_pred <- predict(credit_model, credit_test)

#### creating a confusion matrix to evaulate the performance
#### installing and loading gmodels package
install.packages("gmodels")
library(gmodels)

#### creating confusion matrix
CrossTable(credit_test$default, cred_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c("Actual Default", "Predicted Default"))

###### -------------------------------------------------- ######
## Method 2: Support Vector Machines



### Step 1: Collecting the Data


#### loading the data
letters <- read.csv("letterdata.csv")

#### checking the structure of the data
str(letters)

### Step 2: Preparing  the Data


#### splitting the data into training and test
letters_train <- letters[1:18000,]
letters_test <- letters[18001:20000,]

### Step 3: Training the model on data


#### installing and loading the kernlab package
install.packages("kernlab")
library(kernlab)

#### creating the model
letter_classifier <- ksvm(letter~., data= letters_train, kernel = "vanilladot")
letter_classifier

### Step 4: Evaluating Model Performance


#### fitting the model onto test dataset
letter_predictions <- predict(letter_classifier, letters_test)

#### confusion table
table(letter_predictions, letters_test$letter)

#### summarizing the predictions
agreement <- letter_predictions == letters_test$letter
table(agreement)

###### -------------------------------------------------- ######
## Method 3: Adding Regression to Trees



### Step 1: Collecting the Data


#### loading the data
wine <- read.csv("whitewines.csv")

#### checking the structure of the data
str(wine)

#### checking the normality of the data
hist(wine$quality)
# Yes data is normal #

### Step 2: Preparing  the Data


#### splitting the data into training and test
wine_train <- wine[1:3750,]
wine_test <- wine[3751:4898,]

### Step 3: Training the model on data


#### installing and loading the rpart package
install.packages("rpart")
library(rpart)

#### creating the model
m.rpart <- rpart(quality ~ ., data=wine_train)
m.rpart

#### creating the tree plot
install.packages("rpart.plot")
library(rpart.plot)

#### plot 1
rpart.plot(m.rpart, digits = 3)

#### plot 2
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type=3, extra = 101)

### Step 4: Evaluating Model Performance


#### fitting the model onto test dataset
p.rpart <- predict(m.rpart, wine_test)


#### summarizing the model
summary(p.rpart)
summary(wine_test$quality)

#### checking the correlation
cor(p.rpart, wine_test$quality)