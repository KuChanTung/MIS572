#load dataset
train_data=read.csv("Shelter Animal Outcomes/train.csv",stringsAsFactors = F)
test_data=read.csv("Shelter Animal Outcomes/test.csv",stringsAsFactors = F) 

str(train_data)
train=train_data[-1]
test=test_data[-1]
library(dplyr)
full=bind_rows(train, test)

#將沒有姓名的動物作為其中一個特徵
full$Name[full$Name==""]="No"
full$Name[full$Name!="No"]="Yes"

#處理動物年齡（AgeuponOutcome）屬性
full$TimeValue=sapply(full$AgeuponOutcome,function(x) strsplit(x, split = ' ')[[1]][1])
full$UnitofTime=sapply(full$AgeuponOutcome,function(x) strsplit(x, split = ' ')[[1]][2])
full$UnitofTime=gsub('s', '', full$UnitofTime)
full$TimeValue=as.numeric(full$TimeValue)
multiplier=ifelse(full$UnitofTime == 'day', 1,
                     ifelse(full$UnitofTime == 'week', 7,
                            ifelse(full$UnitofTime == 'month', 30,
                                   ifelse(full$UnitofTime == 'year', 365, NA))))

# 通通轉換成天，並寫入AgeiDays欄位
full$AgeinDays=full$TimeValue * multiplier

#將 性別與生育能力（SexuponOutcome）拆成2個特徵，即性別與是否能生育, Unknown的情形認為是缺失值
full$SexuponOutcome[full$SexuponOutcome=="Unknown"]="Unknown Unknown"
full$bear=sapply(full$SexuponOutcome,function(x) strsplit(x, split = ' ')[[1]][1])
full$sex=sapply(full$SexuponOutcome,  function(x) strsplit(x, split = ' ')[[1]][2])
full$bear[full$bear=="Intact"]="Can"
full$bear[full$bear=="Spayed"]="Cannot"
full$bear[full$bear=="Neutered"]="Cannot"
full$bear[full$bear=="Unknown"]=NA
full$sex[full$sex=="Unknown"]=NA

#處理時間（DateTime）屬性
#先利用strptime()函數，將字元屬性轉為POSIXlt
full$DateTime=strptime(full$DateTime, "%Y-%m-%d %H:%M:%S")
library(lubridate)
full$Hour=hour(full$DateTime)
full$Weekday=wday(full$DateTime)
full$Month=month(full$DateTime)
full$Year=year(full$DateTime)

#再將時段分為早上、下午、傍晚、晚上等四類型
full$TimeofDay=ifelse(full$Hour > 5 & full$Hour < 11, 'morning',
                         ifelse(full$Hour > 10 & full$Hour < 16, 'midday',
                                ifelse(full$Hour > 15 & full$Hour < 20, 'lateday', 'night')))

#將工作日與假日區分出來
full$isWorkday=ifelse(full$Weekday<=5,'WorkDay',ifelse(full$Weekday>5,"Rest",NA))

#處理品種（Breed）屬性
#確認動物是否為混種
full$IsMix=ifelse(grepl('Mix', full$Breed),"Yes","No")
#擷取品種歸類，消除混種註記
full$SimpleBreed=sapply(full$Breed , function(x) gsub(' Mix', '', strsplit(x, split = '/')[[1]][1]))

#處理動物毛色（Color）屬性
# 將動物區分成雜色或純色
full$isPureColor=ifelse(grepl('/| ',full$Color),"Pure","Mix")
# 對於雜色動物消除Mix註記
full$SimpleColor=sapply(full$Color, function(x) strsplit(x, split = '/| ')[[1]][1])

#刪除不需要的欄位
full=full[c(-2,-6,-7,-8,-9,-10,-11,-15,-16)]

#欄位轉為向量（包括分類結果（OutcomeType))
full$Name=factor(full$Name,levels = c("Yes","No"))
full$AnimalType=factor(full$AnimalType,levels = c("Dog","Cat"))
full$bear=factor(full$bear,levels = c("Can","Cannot"))
full$sex=factor(full$sex,levels = c("Male","Female"))
full$Year=factor(full$Year)
full$Month=factor(full$Month)
full$TimeofDay=factor(full$TimeofDay)
full$isWorkday=factor(full$isWorkday)
full$IsMix=factor(full$IsMix)
full$SimpleBreed=factor(full$SimpleBreed)
full$isPureColor=factor(full$isPureColor)
full$SimpleColor=factor(full$SimpleColor)

full$OutcomeType=factor(full$OutcomeType)

#檢查缺失值
library(VIM)
aggr(full[,c(1,4:15)])

#填補 AgeinDays 的缺失值
#對於動物的年齡，我們認為其可能會與品種，
#是否有名字，是否能生育等因素有一定的相關性，
#因此採用 rpart 來預測動物年齡的方式對其進行填補可能是一個比較合理的方案：
library(rpart)
age_model=rpart(AgeinDays ~ AnimalType + sex + bear + SimpleBreed + Name, data = full[!is.na(full$AgeinDays), ], method = 'anova')
full$AgeinDays[is.na(full$AgeinDays)] =predict(age_model, full[is.na(full$AgeinDays), ])

full$Age=ifelse(full$AgeinDays<190,'Little',ifelse(full$AgeinDays>=190 & full$AgeinDays<365,'Mid','Old'))
full$Age=factor(full$Age)
full=full[-5]

#填補 sex 和 bear 的缺失值
full$sex[is.na(full$sex)]="Male"
full$bear[is.na(full$bear)]="Cannot"


#填補Month值
month_model=rpart(Month ~ AnimalType + sex + bear + IsMix + SimpleBreed + SimpleColor, data = full[!is.na(full$Month), ], method = 'anova')
full$Month[is.na(full$Month)]=predict(month_model, full[is.na(full$Month), ])
#再次檢查缺失值
aggr(full[,c(1,4:15)])