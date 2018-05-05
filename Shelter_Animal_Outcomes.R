########################################################################################
##步驟1 資料整理
########################################################################################

#1.1 load dataset
######################################################################################################################

library(data.table)
#訓練資料集筆數26729筆
#利用fread讀入資料，保留data.frame類型，設定參數data.table=F
train_data_original=fread("Shelter Animal Outcomes/train.csv",data.table = F,header = T,sep = ",",stringsAsFactors = F)
str(train_data_original)
#測試資料集筆數11456筆
test_data_original=fread("Shelter Animal Outcomes/test.csv",data.table=F,header = T,sep = ",",stringsAsFactors = F)
str(test_data_original)
#刪除ID欄位 因為用不到
train_new=train_data_original[-1]
test_new=test_data_original[-1]
#將訓練與測試資料集合併，方便後續整理
library(dplyr)
all=bind_rows(train_new, test_new)

#1.2 欄位資料整理
################################################################################

#將沒有姓名的動物作為其中一個特徵
#先把姓名欄空白填N(No)，在把不等於N填Y(Yes)
all$Name[all$Name==""]="N"
all$Name[all$Name!="N"]="Y"

####################################################################################

#處理動物年齡（AgeuponOutcome）屬性
#先把動物年齡的數字切出來，設定為TimeVal
all$TimeVal=sapply(all$AgeuponOutcome,function(x) strsplit(x, split = ' ')[[1]][1])
#把動物年齡的計量單位切出來，設定為UnitTime
all$UnitTime=sapply(all$AgeuponOutcome,function(x) strsplit(x, split = ' ')[[1]][2])
#由於原始資料計量單位有year or years 利用gsub函數進行字串替換處理，全部改成year
all$UnitTime=gsub('s', '', all$UnitTime)
#把TimeVal的字元屬性更改為數字
all$TimeVal=as.numeric(all$TimeVal)
#寫一個判斷式準備用來轉換為統一計量單位天(Day)
mp=ifelse(all$UnitTime == 'day', 1,ifelse(all$UnitTime == 'week', 7,ifelse(all$UnitTime == 'month', 30,ifelse(all$UnitTime == 'year', 365, NA))))
# 轉換成天(Day)，並寫入AgeDays欄位
all$AgeDays=all$TimeVal * mp

#####################################################################################

#將動物性別與生育能力（SexuponOutcome）拆成2個特徵，即"性別（sex）"與是否能"生育能力（fertility)"
#先把登記為Unknown全部更改為Unknown Unknown，就是性別與是否能生育皆為未知
#Neutered (雄性动物不能生育)、Spayed Female(雌性动物不能生育)
#Intact Male(雄性动物能够生育)、Intact Female(雌性动物能生育)、Unknown(未知)
all$SexuponOutcome[all$SexuponOutcome=="Unknown"]="Unknown Unknown"
#把生育能力從SexuponOutcome中切出來設定為Fertility（生育能力）
all$Fertility=sapply(all$SexuponOutcome,function(x) strsplit(x, split = ' ')[[1]][1])
#把性別從SexuponOutcome中切出來設定為Sex（性別）
all$sex=sapply(all$SexuponOutcome,  function(x) strsplit(x, split = ' ')[[1]][2])
#處理Fertility欄位，分類成可以生育(Y)與不能生育(N)，未知則填入NA
all$Fertility[all$Fertility=="Intact"]="Y"
all$Fertility[all$Fertility=="Spayed"]="N"
all$Fertility[all$Fertility=="Neutered"]="N"
all$Fertility[all$Fertility=="Unknown"]=NA
#處理sex欄位，把未知則填入NA
all$sex[all$sex=="Unknown"]=NA

######################################################################################

#處理時間（DateTime）屬性
#因為測試資料集的時間欄位格式不同，先把它切割出來，另外處理
train_time=all[1:26729, ]
test_time=all[26730:nrow(all), ]
#載入lubridate套件，準備用來轉換時間格式
library(lubridate)
#把時間欄位裡的年月日切出來，並設定為ymd欄位
test_time$ymd=sapply(test_time$DateTime,function(x) strsplit(x, split = ' ')[[1]][1])
#把時間欄位裡的時、分切出來，並設定為hs欄位
test_time$hs=sapply(test_time$DateTime,function(x) strsplit(x, split = ' ')[[1]][2])
#利用ymd函式把ymd欄位裡的資料由年/月/日轉換為年-月-日
test_time$ymd=ymd(test_time$ymd)
#利用paste函式把ymd與hs連接在一起，重新寫回DateTime欄位
test_time$DateTime=paste(test_time$ymd,test_time$hs,sep = " ")
#把ymd與hs等欄位刪除，準備接下來的重組
test_time=test_time[c(-15,-16)]
#訓練資料集與測試資料集重組
all=bind_rows(train_time, test_time)
#利用strptime()函數，將字元屬性轉為POSIXlt，而且忽略秒單位
all$DateTime=strptime(all$DateTime, "%Y-%m-%d %H:%M")
#把小時、週、月、年切出來
all$Hour=hour(all$DateTime)
all$Weekday=wday(all$DateTime)
all$Month=month(all$DateTime)
all$Year=year(all$DateTime)
#再將時段分為早上、下午、傍晚、晚上等四類型
all$TimeofDay=ifelse(all$Hour > 5 & all$Hour < 11, 'morning',ifelse(all$Hour > 10 & all$Hour < 16, 'afternoon',ifelse(all$Hour > 15 & all$Hour < 20, 'everning', 'night')))
#將工作日與假日區分出來，是工作日顯示，非工作日顯示N，寫入WorkDay欄位
all$WorkDay=ifelse(all$Weekday<=5,'Y',ifelse(all$Weekday>5,"N",NA))

####################################################################################################

#處理品種（Breed）屬性
#確認動物是否為混種
all$Mix=ifelse(grepl('Mix', all$Breed),"Y","N")
#擷取品種歸類，消除混種註記
all$SimpleBreed=sapply(all$Breed , function(x) gsub(' Mix', '', strsplit(x, split = '/')[[1]][1]))
#處理動物毛色（Color）屬性
# 將動物區分成雜色或純色
all$PureColor=ifelse(grepl('/| ',all$Color),"Pure","Mix")
# 對於雜色動物消除Mix註記
all$SimpleColor=sapply(all$Color, function(x) strsplit(x, split = '/| ')[[1]][1])

#######################################################################################

#1.3 整理資料集
################################################################################

#刪除不需要的欄位
all=all[c(-2,-6,-7,-8,-9,-10,-11,-15,-16)]
#欄位轉為向量（包括分類結果（OutcomeType))
all$Name=factor(all$Name,levels = c("Y","N"))
all$AnimalType=factor(all$AnimalType,levels = c("Dog","Cat"))
all$Fertility=factor(all$Fertility,levels = c("Y","N"))
all$sex=factor(all$sex,levels = c("Male","Female"))
all$Year=factor(all$Year)
all$Month=factor(all$Month)
all$TimeofDay=factor(all$TimeofDay)
all$WorkDay=factor(all$WorkDay)
all$Mix=factor(all$Mix)
all$SimpleBreed=factor(all$SimpleBreed)
all$PureColor=factor(all$PureColor)
all$SimpleColor=factor(all$SimpleColor)
all$OutcomeType=factor(all$OutcomeType)
################################################################################

#各欄位敘述統計資料
summary(all)
################################################################################

#1.4 檢查遺漏數值
################################################################################

#缺值檢查
library(VIM)
aggr(all[,c(1,4:15)])

#填補 AgeDays 的缺失值
#對於動物的年齡，我們認為其可能會與品種，
#是否有名字，是否能生育等因素有一定的相關性，
#因此採用 rpart套件 anova的方法 來預測動物年齡的方式對其進行填補可能是一個比較合理的方案：
library(rpart)
am=rpart(AgeDays ~ AnimalType + sex + Fertility + SimpleBreed + Name, data = all[!is.na(all$AgeDays), ], method = 'anova')
all$AgeDays[is.na(all$AgeDays)] =predict(am, all[is.na(all$AgeDays), ])

##################################################################################################################

#敘述統計
##################################################################################################################

#動物年齡最小0天、最大8030天、Q1=60、Q3=1095、中位數365、平均788.3
summary(all)
#四分數間距Q3-Q1=IQR=1035
IQR(all$AgeDays)
#標準差1079.771
sd(all$AgeDays)
#四分位數0%-0 25%-60 50%-365 75%-1095 100%-8030
quantile(all$AgeDays)

######################################################################################
#因為動物年齡轉換為天數後，數值區間很大，準備轉換成幼齡(kid)、成年(adult)及老年（Old)
#先把貓跟狗分開，因為貓狗的成熟年齡不同
#找出資料集動物標視為狗
dog=which(all$AnimalType=="Dog")
#抓出動物是狗的資料表
all_dog=all[dog,]
#抓出動物是貓的資料表
all_cat=all[-dog,]
#把狗區分小於2歲的kid,成犬為大於2歲小於7歲，老犬為大於7歲
all_dog$Age=ifelse(all_dog$AgeDays<730,'Kid',ifelse(all_dog$AgeDays>=730 & all_dog$AgeDays<=2555,'Adult','Old'))
#把貓區分小於1歲的kid,成貓為大於1歲小於7歲，老貓為大於7歲
all_cat$Age=ifelse(all_cat$AgeDays<365,'Kid',ifelse(all_cat$AgeDays>=365 & all_cat$AgeDays<=2535,'Adult','Old'))
#重新組合資料集
all=bind_rows(all_dog, all_cat)
#把形容動物年齡的欄位向量化
all$Age=factor(all$Age)
#刪除原本動物年齡天數欄位
all=all[-5]

#填補 sex 的缺漏值
#因為原有雄性數量比雌性多，所以把缺漏值全部填成雄性
all$sex[is.na(all$sex)]="Male"
#填補 Fertility的缺漏值
#因為原有"不能生育"比"可生育"的多，所以把缺漏值全部填成"不能生育"
all$Fertility[is.na(all$Fertility)]="N"

#再次檢查缺失值，所有缺失值都已經填補完成
aggr(all[,c(1,4:15)])
##################################################################################

#1.5 資料整理完成
#################################################################################
#把資料集分開成訓練與測試等資料集，準備建立模型
#重新依據OutcomeType進行Dataset排序
all=all[order(all$OutcomeType),]
fwrite(all,file = "Shelter Animal Outcomes/all.csv",row.names = T)
#把訓練資料集濾出來
train=all[1:26729, ]
#寫入檔案
fwrite(train,file="Shelter Animal Outcomes/train_OK.csv",row.names = T)
#把測試資料集濾出來
test=all[26730:nrow(all), ]
#寫入檔案
fwrite(test,file="Shelter Animal Outcomes/test_OK.csv",row.names = T)
######################################################################################