#libraries
library(caret)

#After having cleaned data as for the analysis:
#Convert type column with numeric, 0 stays for benign and 1 for malicious
data$type <- ifelse(data$type=='benign',0,1)
str(data) 

#transorm variables in numeric
data[1:10] <- lapply(data[1:10], as.numeric)

str(data)

#create data test and data train
set.seed(33)     #178 #188
dt = sort(sample(nrow(data), nrow(data)*.7))
data_Train<-data[dt,]
data_Test<-data[-dt,]
count(data_Test, data_Test$type)
count(data_Train, data_Train$type)
prop.table(table(data_Train$type))
prop.table(table(data_Test$type))

#Prepare data for PCA
pca<-data

#Standardize independent variables
x<-subset(select(pca,-type))
head(x)
x<-scale(x)

#Center the dependent variable
y<-pca$type
y<-scale(y,scale =F)

#Implement pca on indepenedent variables
comp<-prcomp(na.omit(x))
comp
summary(comp)
x1<-subset(select(data_Train,-type))
y1<-data_Train$type

#Pca correlation with variables
comp$rotation
comp$rotation[1:5,1:4] 


#Double check for PCA correlation with variables
topN <- 5
load.rot <- comp$rotation
names(load.rot[,2][order(abs(load.rot[,2]),decreasing=TRUE)][2:topN])

#Calculate variance and cumulative variance
std_dev <- comp$sdev
std_dev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
prop_varex

#Plot Variance Explained by PCA
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

# Plot Cumulative Variance Explained by PCA
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#Running ANN, after having implemented PCA
xpc <- data_Test 

modelfit <-pcaNNet(
  type~.,data=data_Train,size=5,ncomp=5)
prednn=predict(modelfit,data_Test)
pcrdf2<-data.frame(obs=data_Test$type,Predictions=prednn)
pcrdf2
table(prediction = prednn, actual = data_Test$type)
#Confusion matrix for ANN
confusionMatrix(factor(prednn, levels=min(data_Test$type):max(data_Test$type)),
                factor(data_Test$type, levels=min(data_Test$type):max(data_Test$type)))
#AUC curve
results4 <- cbind.data.frame(apptype4=data_Test$type,prednn)
res.roc4 <- roc(as.numeric(results4$apptype4), as.numeric(results4$prednn))
plot.roc(res.roc4, col=4, print.auc=TRUE)
auc(res.roc4)