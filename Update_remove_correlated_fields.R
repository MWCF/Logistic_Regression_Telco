# dataset from kaggle - transformed - data source https://www.kaggle.com/vhcg77/telcom-churns-dataset

# dataset is transformed and divided into Test and Train data. 
Churn_data <- read.csv("C:/Users/wai_f/Documents/Telco_revised_data_3.csv") #import transformed data
#check collinearity
train_corr<-Churn_data[,-c(1)]
library (corrplot)
M<-cor(train_corr)
corrplot(M, method = "number")
#remove NoInternetfield as above .7 has high correlation
train_corr2<-train_corr[,-c(23)]
M2<-cor(train_corr2)
corrplot(M2, method ="number")
#No correlation over/under 0.7 Use dataset without No Internet service
#Create new dataframe
data_final<-Churn_data[,-c(24)]
library(caret)
levels(data_final$Churn) <- make.names(levels(factor(data_final$Churn)))
levels(data_final$Churn)
str(data_final)
#scale numeric data
data_final$tenure <- scale(data_final$tenure)
data_final$MonthlyCharges <-scale(data_final$MonthlyCharges)
#split data into train and test data
smp_size <- floor(0.75 * nrow(data_final))
set.seed(111)
train_ind <- sample(seq_len(nrow(data_final)), size = smp_size)
train <-data_final[train_ind,]
test <-data_final[-train_ind,]
#create model
train_control <- trainControl(method = "cv", number = 10, savePredictions = TRUE, classProbs = TRUE, sampling = "up", summaryFunction = twoClassSummary)
fit <- train(Churn~., data=train, trControl=train_control, method="glm", family = binomial, metric="ROC")
# run test data through model and predict
pred = predict(fit, type = 'raw', newdata = test[, 1:23])

# look at predictions

pred

# combine prediction with data

pred_data<-data.frame(pred)

result<-cbind(pred_data,test)

# check results with confusion matrix

library(e1071)

confusionMatrix<- confusionMatrix(result$pred,result$Churn, positive = "X1")

confusionMatrix