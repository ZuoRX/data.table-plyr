rm(list=ls())
library(rJava)
library(xlsx)     #读表
library(dplyr)   #截取   很强大
library(lpSolveAPI)   #这些包要用R的32位操作系统
library(ucminf)
library(Benchmarking)
library(zoo)
library(xts)
cat("\014")  


#------------------#
#塔利班数据优化处理#
#------------------#
Taliban<-read.csv("E:\\GTD\\adjust/Taliban/Taliban.csv",1)
#攻击类型替换成得分
Taliban$attacktype1[which(Taliban$attacktype1==1)]<-0.063  #Assassination
Taliban$attacktype1[which(Taliban$attacktype1==2)]<-0.258  #Armed Assault
Taliban$attacktype1[which(Taliban$attacktype1==3)]<-0.339  #Bombing/Explosion
Taliban$attacktype1[which(Taliban$attacktype1==4)]<-0.061  #Hijacking
Taliban$attacktype1[which(Taliban$attacktype1==5)]<-0.415  #Hostage Taking (Barricade Incident)
Taliban$attacktype1[which(Taliban$attacktype1==6)]<-0.092  #Hostage Taking (Kidnapping)
Taliban$attacktype1[which(Taliban$attacktype1==7)]<-0.001  #Facility/Infrastructure Attack
Taliban$attacktype1[which(Taliban$attacktype1==8)]<-1.000  #Unarmed Assault
Taliban$attacktype1[which(Taliban$attacktype1==9)]<-0.275  #Unknown

aa<-function(y,m,d,at,pk,pw,pc,vk,vw,tk,tw){
  k<-matrix(NA,ncol=11,nrow=4942)   
  for(i in 1:4941) {
    if(y[i]==y[i+1] & m[i]==m[i+1] & d[i]==d[i+1]){
      at[i+1]<-max(at[i],at[i+1]);          #求最大值
      pk[i+1]<-pk[i]+pk[i+1];#死（perpetrator）
      pw[i+1]<-pw[i]+pw[i+1];#伤
      pc[i+1]<-pc[i]+pc[i+1];#抓
      vk[i+1]<-vk[i]+vk[i+1];#victim死
      vw[i+1]<-vw[i]+vw[i+1];#victim伤
      tk[i+1]<-tk[i]+tk[i+1];#总亡
      tw[i+1]<-tw[i]+tw[i+1] #总伤
    }
    else{
      a<-cbind(y[i],m[i],d[i],at[i],pk[i],pw[i],pc[i],vk[i],vw[i],tk[i],tw[i])
      k[i, ]=a
    }
  }
  return(k)
}

Taliban1<-aa(y<-Taliban$iyear,
        m<-Taliban$imonth,
        d<-Taliban$iday,
        at<-Taliban$attacktype1,
        pk<-Taliban$nkillter,
        pw<-Taliban$nwoundte,
        pc<-Taliban$nperpcap,
        vk<-Taliban$nkillv,
        vw<-Taliban$nwoundv,
        tk<-Taliban$nkill,
        tw<-Taliban$nwound) 
write.csv(Taliban1,"E:\\GTD\\adjust/Taliban/Taliban1.csv")
# #先计算重复日期次数，再求其中攻击类型平均值
# num_rep<-function(y,m,d){
#   j<-matrix(1,nrow=4942,ncol = 1)
#   k<-matrix(NA,ncol=4,nrow=4942)
#   for(i in 1:4941) {
#     if(y[i]==y[i+1] & m[i]==m[i+1] & d[i]==d[i+1]){
#       j[i+1]=j[i]+1
#     }
#     else{
#       a<-cbind(y[i],m[i],d[i],j[i])
#       k[i, ]=a
#     }
#   }
#   return(k)
# }
# 
# rep1<-num_rep(y<-Taliban$iyear,
#               m<-Taliban$imonth,
#               d<-Taliban$iday)
# 
# Taliban2<-cbind(Taliban1,rep1[,4])
# write.csv(Taliban2,"E:\\GTD\\a_all/Taliban2.csv")


#--------------------#
#塔利班数据可视化处理#
#--------------------#
rm(list=ls())
Taliban_score<-read.csv("E:\\GTD\\adjust/Taliban/Data_score.csv",1)
hh_score<-xts(Taliban_score$score,as.Date(Taliban_score$date,format='%Y/%m/%d'))
plot(hh_score,type = 'l',main='Talibna Efficiency ')  

eemd(hh)
imfs=eemd(hh)
ts.plot
ts.plot(rowSums(imfs[,1:ncol(imfs)]))
plot(imfs)
plot(imfs[,1])
plot(imfs[,2])
plot(imfs[,3])
plot(imfs[,4])
plot(imfs[,5])
plot(imfs[,6])
plot(imfs[,7])
plot(imfs[,8])
ts.plot(imfs[,9])
ts.plot(imfs[,10])
ts.plot(imfs[,11])


#-----------月频率统计---------------#
num_rep<-function(y,m,d){
  j<-matrix(1,nrow=4942,ncol = 1)
  k<-matrix(NA,ncol=4,nrow=4942)
  for(i in 1:4941) {
    if(y[i]==y[i+1] & m[i]==m[i+1]){
      j[i+1]=j[i]+1
    }
    else{
      a<-cbind(y[i],m[i],d[i],j[i])
      k[i, ]=a
    }
  }
  return(k)
}

rep1<-num_rep(y<-Taliban$iyear,
              m<-Taliban$imonth,
              d<-Taliban$iday)
write.csv(rep1,"E:\\GTD\\adjust/Taliban/mon_freq1.csv")             

#-------攻击类型翻转处理（便于观看）------------#
TALIBAN2<-read.csv("E:\\GTD\\a_all/Taliban/TALIBAN2.csv",1)
#攻击类型替换成得分
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.001)]<-"Hostage Taking (Kidnapping)"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.084)]<-"Bombing/Explosion"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.088)]<-"Hostage Taking (Barricade Incident)"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.132)]<-"Hijacking"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.333)]<-"Assassination"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.364)]<- "Armed Assault"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.393)]<- "Unknown"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==0.492)]<-"Facility/Infrastructure Attack"
TALIBAN2$attacktype2[which(TALIBAN2$attacktype2==1)]<-"Unarmed Assault"  

write.csv(TALIBAN2,"E:\\GTD\\a_all/Taliban/TALIBAN3.csv")




#画频率图
freq_mon<-read.csv("E:\\GTD\\a_all/Taliban/mon_freq.csv",1)        #月频率序列（少）
hh<-xts(freq_mon$freq,as.Date(freq_mon$DATE,format='%Y/%m/%d'))

freq_mon1<-read.csv("E:\\GTD\\a_all/Taliban/mon_freq1.csv",1)      #月频率序列（多）
hh1<-xts(freq_mon1$Freq,as.Date(freq_mon1$DATE,format='%Y/%m/%d'))

Taliban_score<-read.csv("E:\\GTD\\a_all/Taliban/Date_score.csv",1) #效率得分图（多）
hh_score<-xts(Taliban_score$Score,as.Date(Taliban_score$Date,format='%Y/%m/%d'))

plot(hh_score,type = 'l',main='Talibna Efficiency ') 
par(new=TRUE)
lines(hh1,col = "red",axis(4,col="red",col.ticks="red",col.axis="red"))


library(plotrix)
xpos <-Taliban_score$Date
y1<-Taliban_score$Score
y2<-freq_mon1$Freq
twoord.plot(xpos,y1,xpos,y2,
            lylim=c(0,2),rylim=c(0,200),
            lcol=4,rcol=2,xlab="DATE",ylab="Efficiency",rylab="frequency/(month)",
            xtickpos=as.numeric(Taliban_score$Date),
            xticklab = as.character(Taliban_score$Date),
            type=c("l","l"),halfwidth=0.2)
length(xtickpos=as.numeric(Taliban_score$Date))








ts.plot(hh,hh_score,
        gpars=list(xlab="time series",
                   ylab="frequency/month", 
                   lty=c(1:2),
                   axis(4,c(0,150),col = "violet", col.axis = "dark violet", lwd = 2))
)








par(mar=c(5,5,4,5)+0.1)
bar<-barplot(hh1,axes=F,main='Taliban Month Frequency',col ="yellow") 
lines(hh_score)
par(new=TRUE)
plot(bar,hh_score,axes=F,ylim=c(100,190),xlab="",ylab="",col="red",type="b")










d <- data.frame(name=c("zhao","qian","sun","li"),weight=c(62,58,79,60),height=c(178,169,180,173))
x <- d$name
y1 <- d$weight
y2 <- d$height
par(mar=c(5,5,4,5)+0.1)
bar <- barplot(y1,xlim=c(0,5),ylim=c(0,100),ylab="Weight (kg)",col="blue",col.axis="blue",col.lab="blue")
mtext(x,side=1,line=1,at=bar,col="black")
mtext("Name",side=1,line=3,col="black")
par(new=T)
plot(bar,y2,axes=F,xlim=c(0,5),ylim=c(100,190),xlab="",ylab="",col="red",type="b")
axis(4,col="red",col.ticks="red",col.axis="red")
mtext("Heigth (cm)",side=4,line=3,col="red")








