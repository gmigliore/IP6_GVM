# Importing the dataset
ds=read.csv("heart.data.csv")

#Displaying the count of null values per column
colSums(is.na(ds))


# Missing data
#na. rm = TRUE to exclude missing values
ds$biking[is.na(ds$biking)]<-mean(ds$biking,na.rm=TRUE)
ds$smoking[is.na(ds$smoking)]<-mean(ds$smoking,na.rm=TRUE)
ds$heart.disease[is.na(ds$heart.disease)]<-mean(ds$heart.disease,na.rm=TRUE)
#Create multiple copies of the dataset with no missing data
MLR=ds
SVR=ds
TR=ds
RFR=ds 
##################################################################
## Multiple Linear Regression

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)


# Fitting Multiple Linear Regression to the Training set
split=sample.split(MLR$heart.disease,SplitRatio=0.8)
training_set=subset(MLR,split==TRUE)
testing_set=subset(MLR,split==FALSE)


# Predicting the Validation set results
regressor_MLR<-lm(formula=heart.disease~.,data=training_set)
#new <- data.frame( )
#predict(regressor, newdata = new)
y_pred=predict(regressor_MLR,newdata=testing_set)
new<-data.frame(biking=30.77,smoking=23.61)
new
predict(regressor_MLR,newdata=new)


#RMSE
#sqrt(mean((dataset$y_test-y_pred)^2))
library(caret)
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)
########################################################
#Support Vector Regressor
# Splitting the dataset into the Training set and Test set
regressor_SVR=svm(formula=heart.disease~.,data=training_set,
                  type='eps-regression',
                  kernal='radial')
y_pred=predict(regressor_SVR,newdata=testing_set)
# Fitting SVR to the dataset
library(e1071)

# Predicting the Validation set results
new<-data.frame(biking=30.77,smoking=23.61)
new
predict(regressor_SVR,newdata=new)
#new <- data.frame( )

#RMSE
#sqrt(mean((dataset$y_test-y_pred)^2))
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)
########################################################
#Decision Tree Regressor
# Splitting the dataset into the Training set and Test set
regressor_DT=rpart(formula=heart.disease~.,data=training_set)
y_pred=predict(regressor_DT,newdata=testing_set)
# Fitting to the dataset
library(rpart)

# Predicting the Validation set results
new<-data.frame(biking=30.77,smoking=23.61)
new
predict(regressor_DT,newdata=new)
#new <- data.frame( )

#RMSE
#sqrt(mean((dataset$y_test-y_pred)^2))
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)
########################################################
#Random Forest Regressor
# Splitting the dataset into the Training set and Test set
set.seed(1234)
regressor_RF=randomForest(x=training_set[,1:2],
                          y=training_set$heart.disease,
                          ntree=20)
y_pred=predict(regressor_RF,newdata=testing_set)
# Fitting to the dataset
library(randomForest)

# Predicting the Validation set results
new<-data.frame(biking=30.78,smoking=23.61)
new
predict(regressor_RF,newdata=new)

#new <- data.frame( )

#RMSE
#sqrt(mean((dataset$y_test-y_pred)^2))
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)
