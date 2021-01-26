library(caret)
library(dplyr)
library(ggplot2)
library(MASS)
library(DataExplorer)
library(e1071)
library(randomForest)
        x<-read.csv("x.csv")
        glimpse(x)

#checking missing values
        plot_missing(x)
        x<-dplyr::select(x,-ratio)

#checking outliers using boxplot IQR
        boxplot(x$Price)
        out<-boxplot.stats(x$Price)$out # shows outliers Values
        out_ind<-which(x$Price %in% c(out)) #showing row number of outliers

#using mtext() to detect outliers
        boxplot(x$Price,ylab = "hwy",main = "Boxplot of highway miles per gallon")
        mtext(paste("Outliers: ", paste(out, collapse = ", ")))

#outlier detection using Percentile
        lower_bound <- quantile(x$Price, 0.025)
        upper_bound <- quantile(x$Price, 0.975)
        outlier_ind <- which(x$Price < lower_bound | x$Price > upper_bound)
        x[outlier_ind, "Price"]

#Remove outliers from Dataset
        x<-x[-which(x$Price %in% out),]
#OR
        x<-x[-which(x$Price < lower_bound | x$Price > upper_bound),]
        boxplot(x$Price) 

#changing category to dummy vars 
        x<-dummy_cols(x,select_columns = "Engine") #example

# drop unnessory colums from data
        x<-drop_columns(x,ind = "Engin_NA")

# Handling Missing Values in R
        x$MPG<-impute(x$MPG,mean) #By mean
        x$MPG<-impute(x$MPG,median) #By median
        x$Price<-impute(x$Price,mean)
        x$Drivetrain<-impute(New$x,mode)
        plot_missing(x)

#Normalization of response or target or dependent Var
#Firstly, Checking the skewness of Price
        skewness(x$Price)
        skewness(log1p(x$Price))
        skewness(sqrt(x$Price))

#Histogram
        histogram(x$Price)
        histogram(log1p(x$Price))
        histogram(sqrt(x$Price))

#changing data table to numeric for Pearson Correlation
        x<-lapply(x,as.numeric)
        x<-as.data.frame(x) #must change in data frame type for Pearson

#Pearson Correlation
        corr<-cor(x,method = "pearson")
        View(corr)
 
#Choose the features from Pearson Correlation Data
#Split the data into train and test
        set.seed(7267166) 
        trainIndex <- createDataPartition(x$Price, p = 0.8,list = FALSE)
        train <- x[trainIndex,] 
        test <- x[-trainIndex,] 
 
#These Models are built using log1p(Price) "Normalization of Response or Target Vars" and Removing Outlier for Prices
 
 
 #build the multiple regression model using all vars
         model1<-lm(Price~.,data = train)
         summary(model1)
 
 #Predict model using test
         predict1<-predict(model1,test)
 
 #Must Remove Nulls and outliers
 #RMSE and R2 for Model Comparison
 
#With normalization
        RMSE(predict1,test$Price)
# 0.1177331
#for removing na , have to use hydroGOF library
        rmse(predict1,test$Price,na.action=TRUE) #RMSE and rmse's results are the same.
        R2(predict1,test$Price)
#0.8667364

#Without(Normalization) for Multiple Regression
        RMSE(predict1,test$Price)
#4085.752
        R2(predict1,test$Price)
#0.7367931


#For RF,have to change the format/true data type(numeric,...)
#build the RandomForest model using all vars

#With Nor 
        model2<-randomForest(Price~.,data=train,importance=TRUE)
        predict2<-predict(model2,test)summary(lm(Price~Mileage,data = train))

        RMSE(predict2,test$Price)
#0.05279622(nor)
        R2(predict2,test$Price)
#0.9738934(nor)

#Without Normalization for Random Forest 
        RMSE(predict2,test$Price)
#2136.949 #without nor
        R2(predict2,test$Price)
#0.9313983 #without nor

#Changing the log values to numeric forms
        exp(predict2)
        exp(log1p(x$Price))

#Calculating the residual values for Random Forest Model
        res<-test$Price-model2$predicted
        histogram(res)

# YOU SHOULD CHECK NORMALITY OF ERRORS AFTER MODELLING , ERRORS ARE ASSUMED TO FOLLOW A NORMAL DISTRIBUTION WITH A MEAN OF zero.
# LOGARITHMIC TRANSFORMATION IS A CONVIENT MEAN OF TRANSFORMING A HIGHLY SKEWED VARIABLE INTO A MORE NORMALIZED DATASET.
# wHEN MODELING VARS WITH NON-LINEAR RELATIONSHIPS,CHANCES OF PRODUCING ERRORS MAY ALSO BE SKEWED NEGATIVELY.
# IN THEORY,WE WANT TO PRODUCE THE SMALLEST ERROR POSSIBLE WHEN MAKING A PREDICTION,WHILE ALSO TAKING INTO ACCOUNT THAT WE SHOULD NOT BE OVERFITTING MODEL
# USING THE LOGARITHMIC OF ONE OR MORE VARIABLES IMPROVES THE FIT OF MODEL BY TRANSFORMING THE DISTRIBUTION OF FEATURES TO A NORMALLY-SHAPED BELL CURVE.




