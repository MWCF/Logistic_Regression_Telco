# dataset from kaggle - transformed - data source https://www.kaggle.com/vhcg77/telcom-churns-dataset
# dataset is transformed and divided into Test and Train data. 
Churn_data <- read.csv("D:/Telco_revised_data_1.csv") #import transformed data
Corr_data<-Churn_data[,-c(1)]
str(Corr_data)
library(corrplot)
m<-cor(Corr_data)
corrplot(m, method = "numeric")
Churn_data$Churn <- as.factor(Churn_data$Churn) # convert result into factor
str(Churn_data) #Check structure
# rename the predictor variable so the glm model will work with caret
library(caret)
levels(Churn_data$Churn) <- make.names(levels(factor(Churn_data$Churn)))
levels(Churn_data$Churn)
#scale numeric data
Churn_data$tenure <- scale(Churn_data$tenure)
Churn_data$MonthlyCharges <-scale(Churn_data$MonthlyCharges)
Churn_data$InternetService <-scale(Churn_data$InternetService)
#split data into train and test data
smp_size <- floor(0.75 * nrow(Churn_data))
set.seed(110)
train_ind <- sample(seq_len(nrow(Churn_data)), size = smp_size)
train <-Churn_data[train_ind,]
test <-Churn_data[-train_ind,]
#create model
train_control <- trainControl(method = "cv", number = 10, savePredictions = TRUE, classProbs = TRUE, sampling = "up", summaryFunction = twoClassSummary)
fit <- train(Churn~., data=train, trControl=train_control, method="glm", family = binomial, metric="ROC")
# run test data through model and predict
pred = predict(fit, type = 'raw', newdata = test[, 1:25])
# look at predictions
pred
# combine prediction with data
pred_data<-data.frame(pred)
result<-cbind(pred_data,test)
# check results with confusion matrix
library(e1071)
confusionMatrix<- confusionMatrix(result$pred,result$Churn, positive = "X1")
confusionMatrix
#Results output 76% accuracy
#Confusion Matrix and Statistics
#
#          Reference
#Prediction  X0  X1
#        X0 965  94
#        X1 322 380
#                                          
#               Accuracy : 0.7638          
#                95% CI : (0.7432, 0.7834)
#    No Information Rate : 0.7308          
#    P-Value [Acc > NIR] : 0.0008821       
#                                          
#                  Kappa : 0.4788          
#                                          
# Mcnemar's Test P-Value : < 2.2e-16       
#                                          
#            Sensitivity : 0.8017          
#            Specificity : 0.7498          
#         Pos Pred Value : 0.5413          
#         Neg Pred Value : 0.9112          
#             Prevalence : 0.2692          
#         Detection Rate : 0.2158          
#   Detection Prevalence : 0.3986          
#      Balanced Accuracy : 0.7757          
#                                          
#       'Positive' Class : X1     

# Check Correlation
train_corr<-train[,-c(1)]
library (corrplot)
M<-cor(train_corr)
corrplot(M, method = "number")
