rm(list=ls());gc()
library(data.table)
library(ROSE)
library(corrplot)
library(caret)
library(C50)
library(rpart)
library(rpart.plot)
library(DMwR)
library(class)
library(mice)
library(vegan)
library(randomForest)
library(inTrees)
library(e1071)
library (ROCR)
library (MLmetrics)
setwd('D:/r')
#install.packages("MLmetrics")

########讀取資料
train_data = read.csv(file="train.csv",header = T,stringsAsFactors = F)
test_data = read.csv(file="test.csv",header = T,stringsAsFactors = F)
#as.factor
train_data$Attrition_Flag = as.factor(train_data$Attrition_Flag)
test_data$Attrition_Flag = as.factor(test_data$Attrition_Flag)

###################model1###################
# Build the classification model using randomForest
model = randomForest(Attrition_Flag ~ ., data=train_data, 
                     keep.forest=TRUE, ntree=500) 

# Print and understand the model
print(model)
plot(model)
#Important attributes
round(importance(model), 2)  

#Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(model$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]
rf_Imp_Attr

#plot (directly prints the important attributes)
varImpPlot(model)

#Predict on Train data
pred_Train_rd = predict(model, 
                        train_data[,setdiff(names(train_data), "Attrition_Flag")],
                        type="response", 
                        norm.votes=TRUE)

cm_Train = table("actual"= train_data$Attrition_Flag, "predicted" = pred_Train_rd);
modle1_accu_Train= sum(diag(cm_Train))/sum(cm_Train)
#rm(pred_Train_rd, cm_Train)

pred_Test_rd = predict(model, test_data[,setdiff(names(test_data),
                                                 "Attrition_Flag")],
                       type="response", 
                       norm.votes=TRUE)

cm_Test = table("actual"=test_data$Attrition_Flag, "predicted"=pred_Test_rd);
modle1_accu_Test= sum(diag(cm_Test))/sum(cm_Test)


modle1_accu_Train
modle1_accu_Test

###################model2###################
#Build randorm forest using top 9 important attributes.
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:9])

#Build the classification model using randomForest
model_Imp = randomForest(Attrition_Flag~.,
                         data=train_data[,c(top_Imp_Attr,"Attrition_Flag")], 
                         keep.forest=TRUE,ntree=500) 

#Print and understand the model
print(model_Imp)

#Important attributes
model_Imp$importance


#Predict on Train data
pred_Train_rd_attr = predict(model_Imp, train_data[,top_Imp_Attr],
                             type="response", norm.votes=TRUE)

#Build confusion matrix and find accuracy
cm_Train = table("actual" = train_data$Attrition_Flag, 
                 "predicted" = pred_Train_rd_attr);
modle2_accu_Train = sum(diag(cm_Train))/sum(cm_Train)
#rm(pred_Train, cm_Train)

#Predicton Test Data
pred_Test_rd_attr = predict(model_Imp, test_data[,top_Imp_Attr],
                            type="response", norm.votes=TRUE)

#Build confusion matrix and find accuracy
cm_Test = table("actual" = test_data$Attrition_Flag, 
                "predicted" = pred_Test_rd_attr);
modle2_accu_Test = sum(diag(cm_Test))/sum(cm_Test)

modle2_accu_Train
modle2_accu_Test

###################model3###################
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:9])
set.seed(123)
x <- train_data[,!(names(train_data) %in% c("Attrition_Flag"))]
y <- train_data[,(names(train_data) %in% c("Attrition_Flag"))]
str(y)
tuneRF(x, y, ntreeTry = 500, trace=TRUE, plot=TRUE, doBest = TRUE)

tunedmodel = randomForest(Attrition_Flag ~., data = train_data, ntree = 500, mtry = 20, importance = TRUE, proximity = TRUE)
library(magrittr)
print(tunedmodel)
tunedmodel$importance %>% View()

varImpPlot(tunedmodel)

pred_Train_rd_tune = predict(tunedmodel, train_data,
                             type="response", norm.votes=TRUE)

cm_Train = table("actual" = train_data$Attrition_Flag, 
                 "predicted" = pred_Train_rd_tune);
modle3_accu_Train = sum(diag(cm_Train))/sum(cm_Train)

pred_Test_rd_tune = predict(tunedmodel, test_data,
                            type="prob", norm.votes=TRUE)
cm_Test = table("actual" = test_data$Attrition_Flag, 
                "predicted" = pred_Test_rd_tune);
modle3_accu_Test = sum(diag(cm_Test))/sum(cm_Test)


modle3_accu_Train
modle3_accu_Test


recall_test_rd <- sensitivity(pred_Test_rd, test_data$Attrition_Flag)
precision_test_rd <- posPredValue(pred_Test_rd, test_data$Attrition_Flag)
model1_F1 <- (2 * precision_test_rd * recall_test_rd) / (precision_test_rd + recall_test_rd)
model1_F1

recall_test_rd_attr <- sensitivity(pred_Test_rd_attr, test_data$Attrition_Flag)
precision_test_rd_attr <- posPredValue(pred_Test_rd_attr, test_data$Attrition_Flag)
model2_F1 <- (2 * precision_test_rd_attr * recall_test_rd_attr) / (precision_test_rd_attr + recall_test_rd_attr)
model2_F1

recall_test_rd_tune <- sensitivity(pred_Test_rd_tune, test_data$Attrition_Flag)
precision_test_rd_tune <- posPredValue(pred_Test_rd_tune, test_data$Attrition_Flag)
model3_F1 <- (2 * precision_test_rd_tune * recall_test_rd_tune) / (precision_test_rd_tune + recall_test_rd_tune)
model3_F1

library(ROCR)
#2.6
pred_d <- prediction(pred_Test_rd_tune[,2], test_data$Attrition_Flag,)
perf <- performance(pred_d, measure = "tpr", x.measure = "fpr")
#計算AUC
ROC_auc <- performance(pred_d, "auc")
plot(perf, colorize=TRUE, main = "RF ROC curve",
     xlab = "Specificity(FPR)", ylab = "Sensitivity(TPR)",
     text(1,0.15,labels=paste("AUC = ",round(ROC_auc@y.values[[1]],digits=2),sep=""),adj=1))

tunedmodel$variable.importance


imp = as.data.frame(as.numeric(tunedmodel$importance))
var = as.data.frame((row.names(as.data.frame(tunedmodel$importance))))
DF = cbind(var, imp)
colnames(DF) = c("var", "imp")
DF$var = as.character(DF$var)

ggplot(DF , aes(x=reorder(var,imp), y=imp, fill=imp)) + 
  geom_bar(stat="identity", position="dodge") + 
  coord_flip() + ylab("變數重要性") + xlab("") + 
  scale_fill_gradient(low = "#FF9A00", high = "#0033FF") + 
  theme(axis.title = element_text(size=10), 
        axis.text = element_text(size=10))

pred_data = fread("pred_data.csv") 


predict(tunedmodel, pred_data, norm.votes=TRUE)

tunedmodel
