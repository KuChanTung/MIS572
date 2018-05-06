##步驟1 資料整理
########################################################################################
#load dataset
######################################################################################################################
library(data.table)
#訓練資料集筆數26729筆
#利用fread讀入資料，保留data.frame類型，設定參數data.table=F
train=fread("Shelter Animal Outcomes/train_OK.csv",data.table = F,header = T,sep = ",",stringsAsFactors = F)
#測試資料集筆數11456筆
test=fread("Shelter Animal Outcomes/test_OK.csv",data.table=F,header = T,sep = ",",stringsAsFactors = F)
#刪除ID欄位 因為用不到
train=train[-1]
#test=test[-1]
#########################################################################################
#整理測試資料
#########################################################################################
train$Name=factor(train$Name,levels = c("Y","N"))
train$OutcomeSubtype=factor(trainl$OutcomeSubtype)
train$AnimalType=factor(train$AnimalType,levels = c("Dog","Cat"))
train$Fertility=factor(all$Fertility,levels = c("Y","N","Unknown"))
all$sex=factor(all$sex,levels = c("Male","Female","Unknown"))
all$Hour=factor(all$Hour)
all$Weekday=factor(all$Weekday)
all$Year=factor(all$Year)
all$Month=factor(all$Month)
all$TimeofDay=factor(all$TimeofDay,levels = c("morning","afternoon","everning","night"))
all$WorkDay=factor(all$WorkDay,levels = c("Y","N"))
all$Mix=factor(all$Mix,levels = c("Y","N"))
all$SimpleBreed=factor(all$SimpleBreed)
all$PureColor=factor(all$PureColor)
all$SimpleColor=factor(all$SimpleColor)
train$OutcomeType=factor(train$OutcomeType,levels =c("Adoption","Died","Euthanasia","Return_to_owner","Transfer"))



library(C50)
c50_model=C5.0(train[-2],train$OutcomeType,trials = 10)
c50_pre=predict(c50_model,test,type = "prob")

ID=c(1:11456)
c50_data=data.frame(ID,c50_pre)
fwrite(c50_data,"Shelter Animal Outcomes/c50.csv",row.names =T)