
#install.packages("ggcorrplot")
#install.packages("ROCR")

setwd("C:\\Users\\User\\Downloads")
#Reading the dataset


# About the Data
# This dataset was sourced from the National Institute of Diabetes and Digestive 
# and Kidney Diseases. The dataset is used to predict whether or not a patient has 
# diabetes, based on certain diagnostic measurements. Several constraints were 
# placed on the selection criteria from a larger database, meaning that the 
# observations are female patients at least 21 years old of Pima Native American 
# heritage.

getwd()
diab<-read.csv("diabetes.csv")
View(diab)
dim(diab)
str(diab)
names(diab)
diab$Outcome=as.factor(diab$Outcome)

head(diab)

str(diab)

# It's clear that we have zeroes in the BloodPressure, Skin Thickness & BMI attributes.
# Other attributes also have zeroes, however, without knowing information about
# how the data was collected I am hesitant to impute zeroes in other features such 
# as Insulin, for example.You can think of zeroes as you would missing values.
# Let's address these zeroes with Imputation.

tail(diab)

summary(diab)

levels(factor(diab$Pregnancies))

levels(factor(diab$Glucose))

levels(factor(diab$BloodPressure))

levels(factor(diab$SkinThickness))

levels(factor(diab$Insulin))

levels(factor(diab$BMI))

levels(factor(diab$DiabetesPedigreeFunction))

levels(factor(diab$Age))


# no null vlaues found       
anyNA(diab)



Graphs3 <- function(data, var=1:ncol(data))
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in var)
  {
    
    if(length(unique(data[,i])) > 20)
    {
      par(mfrow=c(2,1))
      
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "Count", col = "lightgreen", border=F)
      
    }
    else
    {
      par(mfrow=c(1,2))
      barplot(table(data[,i]), main = paste("Barchart of", names(data)[i]), 
              xlab = names(data)[i], ylab = "Count", col = "red", border=F)
      
      pie(table(data[,i]), main =paste("Piechart of", names(data)[i]) )
      
    }
    
  }
}


# put the graphs of BMI, GLUCOSE, BLOOD PRESSURE, INSULIN, SKIN THICKNESS
# These columns contain outliers
Graphs3(diab)



# removing those observation rows with 0 in any of the variables
# this is for outlier removal
for (i in 2:6) 
{
  diab <- diab[-which(diab[, i] == 0), ]
}

dim(diab)
library("ggcorrplot")
library("ROCR")
library(ggplot2)
# Compute correlation matrix
db_cor <- round(cor(diab[1:9]),1)
db_cor

# No strong correlation observed between variables. 
# So, no need to drop any of them for analysis
ggcorrplot(db_cor)




#plot graphs using this again after the outliers have been removed
# put this in slide
Graphs3(diab)


dim(diab)



# ========================================================================================

##install.packages("caTools")
library(caTools)


##Splitting the Data into Test and Train
split_data<-sample.split(diab,SplitRatio = 0.8)
split_data
train_Data<-subset(diab,split_data=="TRUE")
test_Data<-subset(diab,split_data=="FALSE")
View(train_Data)
dim(train_Data)
dim(test_Data)
summary(test_Data)
##Fitting the Logistic Regression model########################################################################3
fit <- glm(formula = Outcome~Pregnancies+BloodPressure+Insulin+DiabetesPedigreeFunction+Glucose+SkinThickness+BMI+Age,data=train_Data,family=binomial())
summary(fit)
pred<-predict(fit,test_Data,type = "response")
pred
confusion_table <- table(ActualValue=test_Data$Outcome,PredictedValue=pred>0.6)
n <- sum(confusion_table) # number of instances
diag <- diag(confusion_table) 

accuracy <- sum(diag) / n


# this is the accuracry for logistic reggression
accuracy


# ROC Curve for Logit Model
#install.packages("ROCR")
library(ROCR)

# PUT THIS IN SLIDE \/
# ========================================
ROCRPred=prediction(pred,test_Data$Outcome)
ROCRPref=performance(ROCRPred,"tpr","fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
#==========================================

# Implementing the decision tree on the Data set
library(rpart)
model=rpart(Outcome~Pregnancies+BloodPressure+Insulin+DiabetesPedigreeFunction+Glucose+SkinThickness+BMI+Age,data=train_Data)

# PUT THIS IN THE SLIDE \/
#================================
plot(model,uniform = T,margin = 0.05,branch = 0.5)
text(model,cex=0.8,pretty = 0)

#================================


printcp(model)
pred_DT<-predict(model,test_Data,type = "class")
pred_DT
confusion_table_DT = table(ActualValue=test_Data$Outcome,PredictedValue=pred_DT)
n <- sum(confusion_table_DT) # number of instances
diag <- diag(confusion_table_DT) 

accuracy_DT <- sum(diag) / n

# this is the accuracry for Decision Tree Model
accuracy_DT


# Pruning the Tree
pfit<- prune(model, cp=   model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
summary(pfit)
pred_DT_1<-predict(pfit,test_Data,type = "class")
confusion_table_DT_prune= table(ActualValue=test_Data$Outcome,PredictedValue=pred_DT_1)
n <- sum(confusion_table_DT_prune) # number of instances
diag <- diag(confusion_table_DT_prune) 

accuracy_DTP <- sum(diag) / n

# this is the accuracry for Decision Tree Pruned Model
accuracy_DTP

# RANDOM FOREST
install.packages("randomForest")
library(randomForest)

rf <- randomForest(Outcome~.,data=train_Data)

pred = predict(rf, newdata=test_Data) #
confusion_table_RM = table(test_Data[,9], pred)
n <- sum(confusion_table_RM) # number of instances
diag <- diag(confusion_table_RM) 

accuracy_RM <- sum(diag) / n

# this is the accuracry for Random Forest Model
accuracy_RM

# this has the highest accuracy
accuracy_RM

# Since this is a classification problem, we use a confusion matrix to evaluate 
# the performance of our model. Recall that values on the diagonal correspond to
# true positives and true negatives (correct predictions) whereas the others 
# correspond to false positives and false negatives.

# PLEASE NOTE AS THE SAMPLES ARE RANDOMLY GENERATED EACH TIME WE RUN THE CODE WE WILL GET DIFFERENT CONFUSION MATRIX
#HENCE THE ACCURACY AND TPR an FPR will vary as well. 


