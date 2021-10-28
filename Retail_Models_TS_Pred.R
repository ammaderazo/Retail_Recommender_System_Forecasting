library(readr)
data_df <- read_csv("data.csv")
data_df <- data_df[,c(2:3,5:110)]


#making the Recommendation a categorical data and making the values YES/NO
data_df$Recommendation[(data_df$Recommendation==0)] <- 'NO'
data_df$Recommendation[(data_df$Recommendation==1)] <- 'YES'
table(data_df$Recommendation)   #NO-288 YES-209
data_df$Recommendation <- as.factor(data_df$Recommendation)

library(caTools)
set.seed(111)
sel <- sample.split(data_df$Rating,SplitRatio = 0.7)
data_train <- subset(data_df,sel==TRUE)
data_test <- subset(data_df,sel==FALSE)

####################DECISION TREE###############################################
library(caret)
library(rpart)
library(rpart.plot)
set.seed(111)
dt_model <- rpart(Recommendation~., data=data_train, method='class')
rpart.plot(dt_model,extra=100)
dt_pred <- predict(dt_model, data_test, type= 'class')
confusionMatrix(table(dt_pred, data_test$Recommendation), positive='YES')

##################RANDOM FOREST################################################
library(randomForest)
set.seed(111)
rf_model <- randomForest(Recommendation~., data_train,ntree=500)
rf_pred <- predict(rf_model, data_test, type = 'class')
confusionMatrix(table(rf_pred, data_test$Recommendation),positive='YES')

mtry <- tuneRF(data_train[-2], data_train$Recommendation, ntreeTry = 100,
               stepFactor= 1.5, improve= 0.1, trace= TRUE, plot=TRUE)
best.m <- mtry[mtry[,2]==min(mtry[,2]),1]
set.seed(111)
rf_model <- randomForest(Recommendation~., data_train, mtry=best.m,
                         importance = TRUE, ntree=500)
rf_pred <- predict(rf_model, data_test, type = 'class')
confusionMatrix(table(rf_pred, data_test$Recommendation))

##################K NEAREST NEIGHBOR############################################
library(class)
data_df <- data_df[,c(2:3,5:110)]
normalize <- function(x){
  (x-min(x)/(max(x)-min(x)))}
data_norm <- as.data.frame(lapply(data_df[-2],normalize))
data_norm <- cbind(data_norm, data_df$Recommendation)
names(data_norm)[108] <- 'Recommendation'

#split the dataset 
set.seed(111)
sel <- sample.split(data_norm$Rating,SplitRatio = 0.7)
norm_train <- subset(data_norm,sel==TRUE)
norm_test <- subset(data_norm,sel==FALSE)
set.seed(111)
knn_model_22 <- knn(norm_train, norm_test, cl= norm_train$Recommendation, k=22) #higher accuracy 
acc_22 <- 100*sum(norm_test$Recommendation == knn_model_22)/NROW(norm_test$Recommendation)
knn_model_23 <- knn(norm_train, norm_test, cl= norm_train$Recommendation, k=23)
acc_23 <- 100*sum(norm_test$Recommendation == knn_model_23)/NROW(norm_test$Recommendation)

confusionMatrix(table(knn_model_22,norm_test$Recommendation))
################SUPPORT VECTOR MACHINE##########################################
library(e1071)
data_test_sc <- scale(data_test[-2])
data_test_sc <- as.data.frame(cbind(data_test_sc, data_test$Recommendation))
names(data_test_sc)[108] <- 'Recommendation'
data_train_sc <- scale(data_train[-2])
data_train_sc <- as.data.frame(cbind(data_train_sc, data_train$Recommendation))
names(data_train_sc)[108] <- 'Recommendation'
data_test_sc[is.na(data_test_sc)] <- 0
data_train_sc[is.na(data_train_sc)] <- 0
set.seed(111)
svm_model <- svm(Recommendation~., data_train_sc, type='C', scale= FALSE)
svm_pred <-  predict(svm_model, data_test_sc, type='response')
confusionMatrix(table(svm_pred,data_test_sc$Recommendation))


#################### TIME SERIES FORECASTING###################################

dress_sales <- read_excel("data.csv", 
                          sheet = "Dress Sales", col_types = c("numeric","numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric"))
str(dress_sales)
data_df <- dress_sales[,-1]
dates <- seq(as.Date('2013-08-29'), as.Date('2013-10-12'), by='2 days')
i=1
while (i <24){
  names(data_df)[i] <- as.character(dates[i])
  i =i+1}
data_df[is.na(data_df)] <- 0


library(forecast)
library(tseries)
library(fpp2)

#testing more accurate algorithm between arima and holt 
train_set <- data_df[,c(1:16)]
test_set <-  data_df[,-c(1:16)]
train_data <- train_set[1,]
train_data <- ts(t(train_data))
test_data <- test_set[1,]
autoplot(train_data)
forecast_arima <- forecast(auto.arima(train_data),h=7)
forecast_holt <- holt(train_data,7)
values <- as.data.frame(rbind(test_data,forecast_holt$mean,forecast_arima$mean))
values <- as.data.frame(t(values))
values <- round(values,0)
names(values)[1] <- 'Actual'
names(values)[2] <- 'Holt'
names(values)[3] <- 'Arima'

#arima will be used 
data_ts <- data_df[1,]
data_ts <- ts(t(data_ts))
autoplot(data_ts)
forecast_arima <- forecast(auto.arima(data_ts), h=3)
autoplot(forecast_arima)
forecasted_sales <- as.data.frame(forecast_arima$mean)

#loop for the dresses 
i =2 
while (i <= 500) {
  data_ts <- ts(t(data_df[i,]))
  forecast_arima <- forecast(auto.arima(data_ts),h=3)
  forecasted_sales <- cbind(forecasted_sales, forecast_arima$mean)
  i=i+1}
forecasted_sales <- as.data.frame(t(forecasted_sales))
forecasted_sales <- round(forecasted_sales, digits=0)
colnames(forecasted_sales) <- c('2013-10-14','2013-10-16','2013-10-18')
full_dataset <- cbind(dress_sales$Dress_ID,data_df, forecasted_sales)
row.names(full_dataset) <- 1:500
names(full_dataset)[1] <- 'Dress_ID'





