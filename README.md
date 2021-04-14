# Supervised and Unsupervised Machine Learning applied to Network Traffic

## About the Project

The project is about Android applications, which are vulnerable to malicious attacks because of their open-source code and the fact that third parties could install applications without any central control. The aim of the project is to try predicting malicious activities, using network traffic features and to try reducing the FP and FN cases. 

## About the dataset

The dataset has been taken from Kaggle and has been made using data from different pcaps files getting Android apps and Malware apps in 2018. It includes information about 112 application regarding network traffic, more exactly file transfer measured through the number of tcp and other packets sent and received and internet traffic measured in byte volumes.

## About ML algorithms

The first part of the project is focused on the creation of predictive models to forecast the type of application (benign/malicious) on the basis of data traffic using the following supervised learning 
techniques: 
- Decision trees 
- Random forest 
- ANN 

The second part is focused on PCA technique that has been implemented to reduce the 
complexity of the model and to allow ANN to perform better. 


