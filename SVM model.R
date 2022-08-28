library(e1071)
library(mlbench)
library(caret)
library(MLmetrics)
library(ROCR)

train_data = read.csv(file="train.csv",header = T,stringsAsFactors = F)
test_data = read.csv(file="test.csv",header = T,stringsAsFactors = F)

#as.factor
train_data$Attrition_Flag = as.factor(train_data$Attrition_Flag)
test_data$Attrition_Flag = as.factor(test_data$Attrition_Flag)

model = svm(formula = Attrition_Flag ~ .,  
            data = train_data)
summary(model)

#建立特徵重要度


# 預測
train.pred = predict(model, train_data)
test.pred = predict(model, test_data)

# 訓練資料的混淆矩陣
table(real=train_data$Attrition_Flag, predict=train.pred)

# 訓練資料的分類準確率
confus.matrix = table(real=train_data$Attrition_Flag, predict=train.pred)
sum(diag(confus.matrix))/sum(confus.matrix)
  ##F1-score
    F1_Score(train_data$Attrition_Flag, train.pred)

# 測試資料的混淆矩陣
table(real=test_data$Attrition_Flag, predict=test.pred)

# 測試資料的分類準確率
confus.matrix = table(real=test_data$Attrition_Flag, predict=test.pred)
sum(diag(confus.matrix))/sum(confus.matrix)
  ##F1-score
    F1_Score(test_data$Attrition_Flag, test.pred)

#ROC curve
    data(ROCR.simple)
    pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
    perf <- performance(pred,"tpr","fpr")
    plot(perf,colorize=TRUE)
