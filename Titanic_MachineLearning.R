getwd();

setwd("C:/Users/Hp/Documents/R/Titanic ML");

# importing data wrangling packages

install.packages("stringr");
install.packages("caTools");
install.packages("dplyr");


library(stringr);
library(dplyr);
library(caTools);

train <- read.csv("trainTitanic.csv");
test <- read.csv("testTitanic.csv");

#creating a new column called Set, after combining both these datasets.
# This column would have value as training for train datasetrs and testing for test data.
# For testing creating column called Survived (which is the predicter variable) adding NA as default values to it.

train$set = "training";
test$set = "testing";
test$Survived = NA;

completedata <- rbind(train,test);
completedatatest <- rbind(train,test);

# check structure of the data
str(completedata); 

#  data dimension ( no of rows and columns)
dim(completedata); 

# Unique values per column

lapply(completedata, function(x) length(unique(x)));

#Check for Missing values
missing_values <- completedata %>% summarize_all(funs(sum(is.na(.))/length(.)))

completedatatest$Age = ifelse(is.na(completedatatest$Age),ave(completedatatest$Age,FUN = function(x) mean(x, na.rm = TRUE)),completedatatest$Age)

completedata$Survived = ifelse(is.na(completedata$Survived),mean(completedata$Survived, na.rm = TRUE),completedata$Survived)

completedata$Fare = ifelse(is.na(completedata$Fare),mean(completedata$Fare, na.rm = TRUE),completedata$Fare)
?summary


completedata$Age = ifelse(is.na(completedata$Age),mean(completedata$Age, na.rm = TRUE),completedata$Age)

#make gender and embraced as factors
completedata$Sex = factor(completedata$Sex,levels = c("male","female"),labels = c(0,1))

completedata$Embarked = factor(completedata$Embarked, levels = c("C","Q","S"), labels = c(0,1,2))


# To build correlation plot

install.packages("corrplot")
library(corrplot)


y = select_if(completedata, is.numeric)
z=scale(y)

mat = cor(y)
corrplot(mat,method = "circle",addCoef.col = "black")

# Basic Logistic Regression model building

modelLR1 = glm(Survived ~ PassengerId+Survived+Pclass+Sex+Age+SibSp+Parch+Fare,family = binomial,data = train)
summary(modelLR1)

modelLR2 = glm(Survived ~ Pclass + Sex + Age + SibSp , family = binomial,data = train)
summary(modelLR2)

# Prediction using this model for the testing datasets

predictLR = predict(modelLR2, data = test)
predictLR = ifelse(predictLR>0.5,1,0)
mean(predictLR==test$Survived)


