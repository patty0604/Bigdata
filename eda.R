library(readr)
library(dplyr)
library(ggplot2)
library(ranger)
library(fastDummies)
library(corrplot)
library(hrbrthemes)
BankChurners = read_csv('D:/bigdata/midterm/dataset/BankChurners.csv')
BankChurners = BankChurners[,-22:-23]
str(BankChurners) #查看資料集結構
#ranger需要num型態才可以跑，所以裡面char的要轉成factor
BankChurners=lapply(BankChurners, function(x){
    if(is.character(x))
        return(as.factor(x))
    else
        return(x)
}) %>% as.data.frame() #轉完後形成(?)新的資料框

#將類別資料轉換虛擬變數(Dummy variable)
library(fastDummies)
Bank_dummydata = 
    dummy_cols(BankChurners,select_columns = c("Attrition_Flag","Gender","Education_Level",
                                               "Marital_Status","Income_Category",
                                               "Card_Category"),remove_selected_columns=T)

#隨機森林查看重要變數(放在模型那)
churners_model = ranger(Attrition_Flag ~ . ,
                        data = BankChurners,  
                        seed = 1,
                        importance = 'impurity', #impurity不純度 gini(?)
                        mtry = NULL,
                        verbose = TRUE,
                        num.trees = 50,
                        write.forest=TRUE)

var_impor = churners_model$variable.importance %>% as.data.frame()
var_impor=data.frame(var_name = row.names(var_impor) , value=var_impor$.)
var_impor
#畫出重要性變數
bar_plot <- ggplot(var_impor, mapping =  aes(x = reorder(var_name,value), y = value , fill = var_name)) +
    labs(x ="變數名稱" , y = "重要性" )+
    geom_bar(stat = "identity") + coord_flip()

bar_plot

##做特徵視覺化

##目前的客戶狀況(可以做趴數、標題顯示不出來) 圓餅圖
cust_attripie = ggplot(BankChurners,
                       aes(x="",fill = Attrition_Flag)) +
    geom_bar(width = 1) + labs(title = "客戶狀況")
cust_attripie = cust_attripie + coord_polar(theta = "y") 
cust_attripie

##客戶的男女比率 圓餅圖
gender_pie = ggplot(BankChurners,
                    aes(x="",fill = Gender)) +
    geom_bar(width = 1) + labs(title = "客戶男女比例")
gender_pie = gender_pie + coord_polar(theta = "y") 
gender_pie

##客戶的教育程度 長條圖

Education_pie = ggplot(BankChurners,
                     aes(x="",fill = Education_Level)) +
    geom_bar(width = 1) + labs(title = "客戶的教育程度")
Education_pie = Education_pie + coord_polar(theta = "y") 
Education_pie



##婚姻狀況 圓餅圖
Marital_pie = ggplot(BankChurners,
                    aes(x="",fill = Marital_Status)) +
    geom_bar(width = 1) + labs(title = "客戶婚姻狀況")
Marital_pie = Marital_pie + coord_polar(theta = "y") 
Marital_pie

##收入類別 圓餅圖
Income_pie = ggplot(BankChurners,
                    aes(x="",fill = Income_Category )) +
    geom_bar(width = 1) + labs(title = "收入類別")
Income_pie = Income_pie + coord_polar(theta = "y") 
Income_pie


##性別和流失客戶

ggplot(data = BankChurners, aes(factor(Gender),fill=Attrition_Flag)) + 
    geom_bar(position="dodge")+
    labs(x ="性別" ,title = "性別和流失客戶")

a=BankChurners %>% group_by(Gender,Attrition_Flag) %>% summarise(c=n())
a$c=c(0.1735722,0.8264278,0.1461522,0.8538478) 

ggplot(data=a, aes(factor(Gender),y= c,fill=Attrition_Flag)) + 
    geom_bar(position="dodge",stat = "identity")+
    labs(x ="性別" ,title = "性別和流失客戶")

##年齡和流失客戶
ggplot(data = BankChurners, aes(x= Customer_Age,fill=Attrition_Flag)) + 
    geom_bar(position="dodge")+
    labs(x ="年齡" ,title = "年齡和流失客戶")

##信用卡類別和流失客戶

ggplot(data = BankChurners, aes(factor(Card_Category),fill=Attrition_Flag)) + 
    geom_bar(position="dodge")+
    labs(x ="信用卡類別" ,title = "信用卡類別和流失客戶")

##教育程度和流失客戶

ggplot(data = BankChurners, aes(x= Education_Level,fill=Attrition_Flag)) + 
    geom_bar(position="dodge")+
    labs(x ="教育程度" ,title = "教育程度和流失客戶")

##客戶扶養人數、卡類型 箱型圖，
Dependent_box = ggplot(BankChurners, aes(x= Card_Category , y = Dependent_count , fill = Card_Category ))+
    geom_boxplot()+
    coord_flip()
Dependent_box 

##客戶扶養人數、客戶類型 箱型圖
Dependatt_box = ggplot(BankChurners, aes(x= Attrition_Flag , y = Dependent_count , fill = Attrition_Flag ))+
    geom_boxplot()+
    coord_flip()
Dependatt_box

##客戶年齡和流失率 箱型圖(廢)
Age_box = ggplot(BankChurners, aes(x= Attrition_Flag,y = Customer_Age , fill = Attrition_Flag)) +
    geom_boxplot()+
    coord_flip()
Age_box 

##閒置幾個月沒用卡和流失率 箱型圖
Inactive_box = ggplot(BankChurners, aes(x= Attrition_Flag,y = Months_Inactive_12_mon , fill = Attrition_Flag)) +
    geom_boxplot()+
    coord_flip()
Inactive_box  

##聯繫銀行的次數和流失率 箱型圖
Contacts_box = ggplot(BankChurners, aes(x= Attrition_Flag,y = Contacts_Count_12_mon , fill = Attrition_Flag)) +
    geom_boxplot()+
    coord_flip()
Contacts_box  


    ggplot(BankChurners, aes(x= Contacts_Count_12_mon, color = Attrition_Flag)) +
    geom_density()
Contacts_box  


##跟銀行的關係期和客戶的流失率
ggplot(BankChurners, aes(x = Attrition_Flag, y = Months_on_book, fill = Attrition_Flag )) + 
    geom_boxplot(width = .5) + 
    coord_flip()

##總交易金額和總交易次數
ggplot(BankChurners, aes( x = Total_Trans_Ct  , y = Total_Trans_Amt, color = Attrition_Flag)) + 
    labs(x ="總交易次數" , y = "總交易金額" )+
    geom_point() +
    theme(axis.title.x = element_text(colour="grey20",size=20), 
          axis.title.y = element_text(colour="grey20",size=20,angle=90)) 
theme_ipsum()

##客戶類型、平均卡利用率 長條圖
BankChurners %>% group_by(Attrition_Flag) %>% summarise(Avg_Utilization_Ratio = mean(Avg_Utilization_Ratio)) %>%
    ggplot(aes(fill=Attrition_Flag, y=Avg_Utilization_Ratio, x=Attrition_Flag)) + 
    geom_bar(position="dodge", stat="identity")+
    labs(x ="流失客戶" , y = "平均卡利用率" )+
    theme(axis.title.x = element_text(colour="grey20",size=15), 
          axis.title.y = element_text(colour="grey20",size=15))

##平均卡利用率、性別

BankChurners %>% group_by(Gender) %>% summarise(Avg_Utilization_Ratio = mean(Avg_Utilization_Ratio)) %>% 
    ggplot(aes(fill=Gender, y=Avg_Utilization_Ratio, x=Gender )) + 
    geom_bar(position="dodge", stat="identity")+
    labs(x ="性別" , y = "平均卡利用率" )+
    theme(axis.title.x = element_text(colour="grey20",size=15), 
          axis.title.y = element_text(colour="grey20",size=15))




##信用卡額度、性別
BankChurners %>% group_by(Gender) %>% summarise(Credit_Limit = mean(Credit_Limit)) %>% 
    ggplot(aes( x = Gender, y= Credit_Limit ,fill = Gender)) + 
    geom_bar(position="dodge", stat="identity")+
    labs(x ="性別" , y = "平均信用卡額度" )+
    theme(axis.title.x = element_text(colour="grey20",size=15), 
          axis.title.y = element_text(colour="grey20",size=15))


##性別和收入類別 熱度圖

library(d3heatmap)
library(tidyr)
a = table(BankChurners$Income_Category,BankChurners$Gender) %>% as.data.frame()
b = spread(a, Var2, Freq)
rownames(b) = b[, 1]
b = b[, -1]
d3heatmap(b, dendrogram = "none", colors = "YlOrRd")



#把客戶流失類別轉成1，0
sub_target <- function(x){
    if(x == "Existing Customer"){
        return(0)
    } else {
        return(1)
    }
}
BankChurners_corr = BankChurners
BankChurners_corr$Attrition_Flag <- sapply(BankChurners$Attrition_Flag, sub_target)

##相關係數圖(相關係數圖只能分析連續型的變數，因此把類別型去掉:性別、教育程度、婚姻狀況、薪水區間、信用卡ˋ種類)
# Removing columns that will not be used during analysis
library(corrplot)
BankChurners_corr <- select(BankChurners_corr, -c("CLIENTNUM", "Gender","Education_Level","Marital_Status","Income_Category","Card_Category"))
bank_cor = cor(BankChurners_corr)
a = ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)
corrplot.mixed(bank_cor)


##總交易金額和總交易次數
ggplot(BankChurners, aes( x = Total_Trans_Ct  , y = Total_Trans_Amt, color = Attrition_Flag)) + 
    labs(x ="總交易次數" , y = "總交易金額" )+
    geom_point() +
    theme_ipsum()+
    theme(axis.title.x = element_text(colour="grey20",size=20), 
          axis.title.y = element_text(colour="grey20",size=20,angle=90))


##卡的類型 客戶流失(廢)
c = table(BankChurners$Card_Category,BankChurners$Attrition_Flag) %>% as.data.frame()
d = spread(c, Var2, Freq)
rownames(d) = d[, 1]
d = d[, -1]
d3heatmap(d, dendrogram = "none", colors = "YlOrRd")


##信用卡額度和客戶流失(連續和類別)箱型圖 密度圖
#ggplot(BankChurners,aes(x=Credit_Limit,colour=Attrition_Flag))+geom_density()


Credit_Limit_box = ggplot(BankChurners, aes(x= Attrition_Flag,y = Credit_Limit , fill = Attrition_Flag)) +
    geom_boxplot()+
    coord_flip()
Credit_Limit_box  

##總交易金額和信用卡額度和客戶流失 
ggplot(BankChurners, aes( x = Total_Trans_Amt , y = Credit_Limit, color = Attrition_Flag)) + 
    geom_point() +
    geom_vline(xintercept=11500,color="#984B4B",size=2.5)+
    labs(x ="總交易金額" , y = "平均信用卡額度" )+
    theme(axis.title.x = element_text(colour="grey20",size=20), 
          axis.title.y = element_text(colour="grey20",size=20,angle=90))+
theme_ipsum()

#分配給持卡人帳戶的信用額度與該帳戶當前餘額之間的差額之平均和客戶流失率
Avg_Open_To_Buy_box = ggplot(BankChurners, aes(x= Attrition_Flag,y = Avg_Open_To_Buy , fill = Attrition_Flag)) +
    geom_boxplot()
Avg_Open_To_Buy_box  

#分配給持卡人帳戶的信用額度與該帳戶當前餘額之間的差額之平均 && 平均卡利用率和客戶流失率
ggplot(BankChurners, aes( x = Avg_Open_To_Buy , y = Avg_Utilization_Ratio, color = Attrition_Flag )) + 
    geom_point() +
    # geom_vline(xintercept=11500,color="#984B4B",size=2.5) + 
    labs(x ="額度與餘額的差額之平均" , y = "平均卡利用率" )+
    theme(axis.title.x = element_text(colour="grey20",size=20), 
          axis.title.y = element_text(colour="grey20",size=20,angle=90))

#客戶年齡和客戶流失率
Customer_Age_box = ggplot(BankChurners, aes(x= Attrition_Flag,y = Customer_Age , fill = Attrition_Flag)) +
    geom_boxplot()
Customer_Age_box  

##客戶年齡和信用卡額度(廢)
ggplot(BankChurners, aes( x = Customer_Age , y = Credit_Limit, color = Attrition_Flag )) + 
    geom_point() +
    labs(x ="客戶年齡" , y = "信用卡額度" )+
    theme(axis.title.x = element_text(colour="grey20",size=20), 
          axis.title.y = element_text(colour="grey20",size=20,angle=90))

##信用卡總周轉餘額 與 信用卡平均利用率 和 客戶流失率 	Total_Revolving_Bal & Avg_Utilization_Ratio
ggplot(BankChurners, aes( x = Total_Revolving_Bal , y = Avg_Utilization_Ratio, color = Attrition_Flag )) + 
    geom_point() +
    labs(x ="信用卡總周轉餘額" , y = "平均卡利用率" )+
    theme(axis.title.x = element_text(colour="grey20",size=20), 
          axis.title.y = element_text(colour="grey20",size=20,angle=90))
 

chisq.test(BankChurners$Gender,BankChurners$Attrition_Flag)

b = aov( Months_on_book ~ Attrition_Flag, data = BankChurners)
summary(b)
