#libraries
library(rpart)
library(rpart.plot)
library(caret)
library(rsample)
library(ggplot2)
library(ROCR)
library(e1071)
library(randomForest)
library(pROC)
library(neuralnet)
library(dplyr)

#Divide data of the previous dataframe in train and test data
set.seed(33)     
dt = sort(sample(nrow(data), nrow(data)*.7))
data_Train<-data[dt,]
data_Test<-data[-dt,]

#Check how many benign and malicious app there are in train and test sets
count(data_Test, data_Test$type)
count(data_Train, data_Train$type)

#Check if randomization process is correct
prop.table(table(data_Train$type))
prop.table(table(data_Test$type))
#in both cases, the amount of mal and benign is more or less the same

#Create Decision Tree 

fit <- rpart(type~ vulume_bytes+ tcp_packets+ remote_app_packets+source_app_bytes+remote_app_bytes+source_app_packets+external_ips+dist_port_tcp, 
             data = data_Train, method = 'class')
prp(fit, digits=-2)

#decision tree summary

summary(fit)


#Make predictions on Decion Tree
pred <- predict(fit, data_Test, type = 'class')

#Confusion matrix
confusionMatrix(pred,data_Test$type)


#Implement random forest model

model_rf <- randomForest(
  type ~ tcp_packets+vulume_bytes+remote_app_packets+source_app_bytes+remote_app_bytes+source_app_packets+external_ips+dist_port_tcp,ntree=500, mtry=3, importance=TRUE,
  data=data_Train
)  
model_rf

#List of the most important variables used in our Random Forest
varImp(model_rf)
varImpPlot(model_rf)

#Make predictions on Random Forest
pred2 <- predict(model_rf, data_Test)

#Confusion matrix 
confusionMatrix(pred2,data_Test$type)

#AUC curve
library(pROC)
results2 <- cbind.data.frame(apptype2=data_Test$type,pred2)
res.roc <- roc(as.numeric(results2$apptype2), as.numeric(results2$pred2))
results3 <- cbind.data.frame(apptype=data_Test$type,pred)
res.roc2 <- roc(as.numeric(results3$apptype), as.numeric(results3$pred))

#plot ROC curve random forest
plot.roc(res.roc, col=3, print.auc = TRUE)
auc(res.roc)

#plot ROC curve decision tree

plot.roc(res.roc2, col=2, print.auc=TRUE)
auc(res.roc2)

#ROC curve for RF and DT
testforest = predict(model_rf, newdata = data_Test, type="prob")
forestpred = prediction(testforest[,2], data_Test$type)
forestperf = performance(forestpred, "tpr", "fpr")
testdecision = predict(fit, newdata = data_Test, type="prob")
dtpred = prediction(testdecision[,2],data_Test$type)
dtperf = performance(dtpred,"tpr","fpr")
plot(perf, main="ROC")
plot(forestperf, col=3, add=TRUE)
plot(dtperf, col=2, add=TRUE)

######
#After having cleaned fdata as for the analysis:
data$type <- ifelse(data$type=='benign',0,1)
data[1]<- list(NULL)

#normalize the data
normalize <- function(x){
  return ((x-min(x))/ max(x)-min(x))
}
maxmindf <- as.data.frame(lapply(data,normalize))

#split data into train and test
set.seed(33)
dt = sort(sample(nrow(maxmindf),nrow(maxmindf)*.7))
data_Train <- mamind[dt,]
data_test <- maxmindf[-dt,]

#check how many benign and malicious app there are in train and tst sets

count(data_Test, data_Test$type)
count(data_Train, data_Train$type)

#implement ANN on data

nn= neuralnet(type~tcp_packets+vulume_bytes+dist_port_tcp+remote_app_packets+source_app_bytes+remote_app_bytes+source_app_packets,
              data=data_Train,hidden=5, linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)

temp_test = data_Test %>% dplyr::select(tcp_packets,vulume_bytes,dist_port_tcp,remote_app_packets,source_app_bytes,remote_app_bytes,source_app_packets)
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = data_Test$type, prediction = nn.results$net.result)
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

#Confusion matrix for ANN
confusionMatrix(factor(prediction, levels=min(data_Test$type):max(data_Test$type)),
                factor(actual, levels=min(data_Test$type):max(data_Test$type)))

