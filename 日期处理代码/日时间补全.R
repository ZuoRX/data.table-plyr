rm(list=ls())
library(rJava)
library(xlsx)     #读表
library(dplyr)   #截取   很强大
library(lpSolveAPI)   #这些包要用R的32位操作系统
library(ucminf)
library(Benchmarking)
library(zoo)
library(xts)
library(Rlibeemd)
library(hht)
cat("\014") 

#-----------------ISIL-------------------------------#
ISIL<-read.xlsx("E:/GTD/adjust/aaa/adjust_time/date-score_ISIL.xlsx",1)
### 指定起始时间 
startdate <- as.Date("2013-5-17") #生成日序列
enddate <- as.Date("2016-12-31")
ndays <- enddate - startdate + 1
tt <- ts(1:ndays, frequency =1, start =as.Date("2013-5-17"))
ss <- as.Date("2013-5-17")
dates <- seq(from=ss, by=1, length.out=ndays)
tt <- data.frame(dates,tt)
tt[,2]<-rep(0,1325)
ISIL<-ISIL[,-1]
names(tt)<-c("date","tt")
ISIL1<-merge(tt,ISIL,by="date")

for (i in 1:1325) {
  for (j in 1:991) {
    if(tt[i,1]==ISIL[j,1]){
      tt[i,2]=ISIL[j,2]
    }
  }
}
write.csv(tt,"E:/GTD/adjust/aaa/adjust_time/ISIL_new.csv")

#精度分布，密率分布yu

#-----------------SL-------------------------------#
rm(list=ls())
SL<-read.xlsx("E:/GTD/adjust/aaa/adjust_time/date-score_SL.xlsx",1)
### 指定起始时间 
startdate <- as.Date("1978-8-24") #生成日序列
enddate <- as.Date("2016-12-31")
ndays <- enddate - startdate + 1
tt <- ts(1:ndays, frequency =1, start =as.Date("1978-8-24"))
ss <- as.Date("1978-8-24")
dates <- seq(from=ss, by=1, length.out=ndays)
tt <- data.frame(dates,tt)
tt[,2]<-rep(0,14010)
# names(tt)<-c("date","tt")
# ISIL1<-merge(tt,SL,by="date")

for (i in 1:14010) {
  for (j in 1:1684) {
    if(tt[i,1]==SL[j,1]){
      tt[i,2]=SL[j,2]
    }
  }
}
write.csv(tt,"E:/GTD/adjust/aaa/adjust_time/SL_new.csv")

#-----------------Taliban-------------------------------#
rm(list=ls())
Taliban<-read.xlsx("E:/GTD/adjust/aaa/adjust_time/date-score_Taliban.xlsx",1)
### 指定起始时间 
startdate <- as.Date("1992-12-29") #生成日序列
enddate <- as.Date("2016-12-31")
ndays <- enddate - startdate + 1
tt <- ts(1:ndays, frequency =1, start =as.Date("1992-12-29"))
ss <- as.Date("1992-12-29")
dates <- seq(from=ss, by=1, length.out=ndays)
tt <- data.frame(dates,tt)
tt[,2]<-rep(0,1)
# ISIL<-ISIL[,-1]
# names(tt)<-c("date","tt")
# ISIL1<-merge(tt,ISIL,by="date")

for (i in 1:1325) {
  for (j in 1:991) {
    if(tt[i,1]==Taliban[j,1]){
      tt[i,2]=Taliban[j,2]
    }
  }y
}




#------------ISIL起始时间一致-----------#
startdate <- as.Date("1978-8-24") #生成日序列
enddate <- as.Date("2013-5-16")
ndays <- enddate - startdate + 1
tt <- ts(1:ndays, frequency =1, start =as.Date("1978-8-24"))
ss <- as.Date("1978-8-24")
dates <- seq(from=ss, by=1, length.out=ndays)
tt <- data.frame(dates,tt)
tt[,2]<-rep(0,12685)
write.csv(tt,"E:/GTD/adjust/aaa/adjust_time/ISIL_sup.csv")


#------------Taliban起始时间一致-----------#
rm(list=ls())
startdate <- as.Date("1978-8-24") #生成日序列
enddate <- as.Date("1992-12-28")
ndays <- enddate - startdate + 1
tt <- ts(1:ndays, frequency =1, start =as.Date("1978-8-24"))
ss <- as.Date("1978-8-24")
dates <- seq(from=ss, by=1, length.out=ndays)
tt <- data.frame(dates,tt)
tt[,2]<-rep(0,5241)
write.csv(tt,"E:/GTD/adjust/aaa/adjust_time/full_time/Taliban_sup.csv")






