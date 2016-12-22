cf <- read.csv("Customer _Churn_Data.csv",stringsAsFactors = F,header = T)
df <- data.frame(cf)
# Boruta algorithm application
#install.packages("Boruta")
library(Boruta)
library(ranger)
convert <- c(2,5,22)
df[,convert] <- data.frame(apply(df[convert], 2, as.factor))
set.seed(123)
boruta.train <- Boruta(churn~., data = df, doTrace = 2)
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
      at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

# least important variables removed
cf$Id <- NULL
cf$state <- NULL
cf$area_code <- NULL
cf$phone_number <- NULL
df <- data.frame(cf)

# Applying Naive Bayes Model
library(caret)
library(klaR)
library(MASS)
# define training control
train_control <- trainControl(method="cv", number=10)
# train the model
model <- train(churn~., data=df, trControl=train_control, method="nb")
#print(model)
predictions<- predict(model,df)
# append predictions
mydat<- cbind(df,predictions)
#head(mydat)
# summarize results
result <- confusionMatrix(mydat$predictions,mydat$churn)
print(result)
precision <- result$byClass['Pos Pred Value']    
recall <- result$byClass['Sensitivity']
f_measure <- 2 * ((precision * recall) / (precision + recall))
names(f_measure) <- "F-Measure"
print(f_measure)

# Decision Tree
library(rpart)
library(caret)
# define training control
train_control <- trainControl(method="cv", number=10)
# train the model
model <- train(churn~., data=df, trControl=train_control, method="rpart")
# summarize results
predictions<- predict(model,df)
# append predictions
mydat<- cbind(df,predictions)
#head(mydat)
# summarize results
result <- confusionMatrix(mydat$predictions,mydat$churn)
print(result)
precision <- result$byClass['Pos Pred Value']    
recall <- result$byClass['Sensitivity']
f_measure <- 2 * ((precision * recall) / (precision + recall))
names(f_measure) <- "F-Measure"
print(f_measure)

# SVM-Radial
library(caret)
library(kernlab)
# define training control
train_control<- trainControl(method="cv", number=10)
# train the model 
model<- train(churn~., data=df, trControl=train_control, method="svmRadial")
# make predictions
predictions<- predict(model,df)
# append predictions
mydat<- cbind(df,predictions)
#head(mydat)
# summarize results
result <- confusionMatrix(mydat$predictions,mydat$churn)
print(result)
precision <- result$byClass['Pos Pred Value']    
recall <- result$byClass['Sensitivity']
f_measure <- 2 * ((precision * recall) / (precision + recall))
names(f_measure) <- "F-Measure"
print(f_measure)

#SVM-Poly
library(caret)
library(kernlab)
# define training control
train_control<- trainControl(method="cv", number=10)
# train the model 
model<- train(churn~., data=df, trControl=train_control, method="svmPoly")
# make predictions
predictions<- predict(model,df)
# append predictions
mydat<- cbind(df,predictions)
#head(mydat)
# summarize results
result <- confusionMatrix(mydat$predictions,mydat$churn)
print(result)
precision <- result$byClass['Pos Pred Value']    
recall <- result$byClass['Sensitivity']
f_measure <- 2 * ((precision * recall) / (precision + recall))
names(f_measure) <- "F-Measure"
print(f_measure)
