library(data.table)
library(xgboost)
library(Matrix)
library(Ckmeans.1d.dp)
library(pROC)
library(MLmetrics)

train = read.csv("train.csv")
test = read.csv("test.csv")

#資料處理
train <- train[,-1]     #刪除第一行的會員編號
test <- test[,-1]
X_train <- train[,-1]   #訓練的資料，刪除目標變數
y_train <- train[,1]    #label(目標變數)
X_test <- test[,-1]
y_test <- test[,1]

#轉換資料格式
train_matrix <- sparse.model.matrix(Attrition_Flag ~ .-1, data = train)  
test_matrix <- sparse.model.matrix(Attrition_Flag ~ .-1, data = test)
train_fin <- list(data=train_matrix,label=y_train)                       #轉成list
test_fin <- list(data=test_matrix,label=y_test) 
dtrain <- xgb.DMatrix(data = train_fin$data, label = train_fin$label)    #轉成matrix
dtest <- xgb.DMatrix(data = test_fin$data, label = test_fin$label)

#xgboost建模
xgb <- xgboost(data = dtrain, max.depth = 8,
                 eta = 0.1, nthread = 2, nround = 2, objective = "binary:logistic")

#重要特徵值
importance <- xgb.importance(train_matrix@Dimnames[[2]], model = xgb)  
head(importance)
xgb.ggplot.importance(importance)


#混淆矩陣
pre_xgb = round(predict(xgb,newdata = dtest))
t = table(y_test,pre_xgb,dnn=c("true", "pre"))
t

accuracy = (t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
accuracy
test
recall_test_rd_tune <- sensitivity(xgb, test$Attrition_Flag)
precision_test_rd_tune <- posPredValue(xgb, test$Attrition_Flag)


F1_Score(y_test, pre_xgb)
precision = t[2,2]/(t[2,2]+t[1,2])
recall = t[2,2]/(t[2,2]+t[2,1])
f1_score =  (2*precision*recall)/(precision+recall)

#ROC曲線
xgboost_roc <- roc(y_test,as.numeric(pre_xgb))

pred_d <- prediction(y_test, pre_xgb)
perf <- performance(pred_d, measure = "tpr", x.measure = "fpr")

ROC_auc <- performance(pred_d, "auc")
plot(perf, colorize=TRUE, main = "XGB ROC curve",
     xlab = "Specificity(FPR)", ylab = "Sensitivity(TPR)",
     text(1,0.15,labels=paste("AUC = ",round(ROC_auc@y.values[[1]],digits=2),sep=""),adj=1))

