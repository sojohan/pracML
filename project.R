#install.packages("plyr")
#install.packages("dplyr")

require(plyr)
require(dplyr)

#install.packages("caret")
library(caret)

# import the data

train_data <- read.csv("C:/Projects/Coursera/PracticalMachineLearning/pml-training.csv",header=TRUE, na.strings=c("NA", "#DIV/0!"))
test_data <- read.csv("C:/Projects/Coursera/PracticalMachineLearning/pml-testing.csv", header=TRUE,na.strings=c("NA", "#DIV/0!"))


# The data set contains variables like mean, standard deviation,kurtosis, min & max. All these variables are derived
# from the core variables. Going forward I only use these core measured variables and not the derived. 

# In addition we need to take out all NA's':

reduce_data<-train_data[, apply(train_data, 2, function(x) !any(is.na(x)))] 
reduce_data<-reduce_data[,-(1:7)]


#The same variables as in the training data 
CleanTdata<-test_data[,names(reduce_data[,-53])]
reduce_data$classe<-as.factor(reduce_data$classe)

# A quick overview of the variables I conclude that there is no NA's in the dataset
#summary(reduce_data)

# Correlation
# I use the findCorrelation function from the caret packages to identify
# higly correlated variables. So correlations above 0.75 will be removed. 
# 
#temp<-reduce_data
# Select only the numerical values
# Calculate the correlations
#correl_dat<-cor(reduce_data[,2:30])
# Find the correlations that have correlations higher then +-0.75 
#highlyCor <- findCorrelation(correl_dat, cutoff = .75)
#reduce_data<- reduce_data[,-highlyCor]
#reduce_data<-cbind(user_name=temp$user_name,reduce_data)



set.seed(12550)
trainIndex <- createDataPartition(reduce_data$classe, p = 0.60,list=FALSE)
training<- reduce_data[trainIndex,]
testing <- reduce_data[-trainIndex,]

# GBM model
set.seed(13333)
fitcontrol1 <- trainControl(method = "cv", number = 5,allowParallel = T,verbose=T)
mod2 <- train(training$classe ~ .,method="gbm", data=training,  trControl = fitcontrol1,verbose = F)


pred2 <- predict(mod2,testing)
confusionMatrix(pred2, testing$classe)

# Random forest model
set.seed(13333)
fitcontrol2<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
mod3 <- train(training$classe ~ .,method="rf", trControl=fitcontrol2, data=training,verbose = F)

pred3 <- predict(mod3,testing)
confusionMatrix(pred3, testing$classe)

# The model winner is the random forest model with accuracy at 0.98 
predict(mod3, CleanTdata)
