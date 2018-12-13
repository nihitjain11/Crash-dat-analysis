dataset=read.csv("F:/crash-dat-analysis-master/crash-dat-analysis-master/Crash.csv",sep = '\t')
ds = dataset[,]
View(ds)
mean(ds$WEIGHT,na.rm=TRUE)
ds$CHEST_IN[is.na(ds$CHEST_IN)]<-mean(ds$CHEST_IN,na.rm=TRUE)
ds$HEAD_INJ[is.na(ds$HEAD_IN)]<-mean(ds$HEAD_IN,na.rm=TRUE)
ds$LLEG_INJ[is.na(ds$LLEG_IN)]<-mean(ds$LLEG_IN,na.rm=TRUE)
ds$RLEG_INJ[is.na(ds$RLEG_IN)]<-mean(ds$RLEG_IN,na.rm=TRUE)
ds$DOORS=as.numeric(ds$DOORS)
ds$DOORS[is.na(ds$DOORS)]<-mean(ds$DOORS,na.rm=TRUE)
ds$WEIGHT[is.na(ds$WEIGHT)]<-mean(ds$WEIGHT,na.rm=TRUE)
ds$SIZE2[is.na(ds$SIZE2)]<-mean(ds$SIZE2,na.rm=TRUE)
ds$PROTECT2[is.na(ds$PROTECT2)]<-mean(ds$PROTECT2,na.rm=TRUE)
lv = levels(ds$MAKE)
lb = length(levels(ds$MAKE))
ds$MAKE=as.numeric(factor(ds$MAKE,
                levels=lv,
                labels = c(1:lb)) )
ds$DRIV_PAS=factor(ds$DRIV_PAS,
                   levels=c('Driver','Passen'),
                    labels=c(1,2))
ind=sample(2,nrow(ds),replace=TRUE,prob = c(0.8,0.2))
tdata=ds[ind==1,]
vdata=ds[ind==2,]
#Multiple regression
result1=lm(HEAD_INJ~CHEST_IN+MAKE+LLEG_INJ+RLEG_INJ+DRIV_PAS+DOORS+YEAR+WEIGHT+SIZE2+PROTECT2,tdata)
ds$SIZE2=as.numeric(ds$SIZE2)
library(caTools)
set.seed(123)
split=sample.split(ds$CHEST_IN,splitRatio=0.8)
training_set=subset(ds,split==TRUE)
test_set=subset(ds,split==FALSE)
regressor=lm(formula=HEAD_INJ ~.,
             data=tdata)
summary(result1)
y_pred=predict(result1,newdata = tdata)
step(result1, direction = "backward")
library(ggplot2)
cor()

summary(result1)
class(ds$SIZE2)
