# import as we need
library(ggplot2)
library(tidyverse)
library(plyr)
library(dplyr)

# read file 

bk_chu <- read.csv("~/datasets/BankChurners.csv")

# 轉成 dataframe
bk_chu_df <- data.frame(bk_chu)

# 敘述把X軸的內容放在前面，Y軸的內容放在後面
#===============
# 關係1 (收入類別 & 信用卡總周轉餘額)

# Income_Category and Total_Revolving_Bal

# 畫出盒狀圖

ggplot(bk_chu_df, 
       aes(x=Income_Category, y=Total_Revolving_Bal)) + 
  geom_boxplot()

# 從這張盒狀圖裡面可以看出不論收入的高低，信用卡總周轉餘額大概都落在
# 1250-1500元之間，表示收入與總周轉餘額間並無明顯顯著關係

# ==============

# 關係2 (信用卡的持卡等級 & 教育程度的高低)

# Card_Category and Education level

# 畫出長條圖
ggplot(bk_chu_df, aes(x = Card_Category, fill = Education_Level)) + 
  geom_bar(position = "fill")+
  scale_fill_discrete(name="教育程度",
                      labels=c("大學","博士","研究所","高中","學士後","未受教育","未知"))+
  labs( x="信用卡的持卡等級",y = "比率")+
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) ,
                     labels =c("0%","10%","20%","30%","40%","50%",
                               "60%","70%","80%","90%","100%"))

# 從這張長條圖裡面可以看出教育程度為College的客戶辦Platinum(白金卡)的比例是0%
# 而有Doctorate(博士)辦Platinum的比例高過其他三種類型的信用卡。


# 關係3 (收入& 信用額度)

# Income_Category and Credit_Limit

# 畫出長條圖

  ggplot(bk_chu_df, aes(x = Income_Category, fill = Credit_Limit)) + 
  geom_bar(position = "dodge")+
  labs( x= "收入等級",y = "信用卡額度")+
  scale_y_continuous(breaks = c(300,600,900,1200,1500,1800,2100,2400,2700,3000) ,
                     labels =c("3000","6000","9000","12000","15000","18000","21000","24000","27000","30000"))

# 從這張圖可以看出，收入最低的人反而具有最高的信用額度
# 而收入最高者，所具有的信用額度反而最低。

  
  
  
  
# 關係4 (銀行產品數量 & 性別)
  
# Total_Relationship_Count and Gender 
  
# 畫出長條圖
  ggplot(bk_chu_df, aes(x = Total_Relationship_Count, fill = Gender)) + 
    geom_bar(position = "dodge")+
    scale_fill_discrete(name="性別",
                        labels=c("女性","男性"))+
    labs( x= "總銀行商品購買數量",y = "人數")+
    scale_x_continuous(breaks = c(1,2,3,4,5,6) ,
                       labels =c("1","2","3","4","5","6"))

  
# 從這張圖可以看出，多數的男性與女性在銀行商品的數量選擇上是3項
  
  
  
  