rm(list=ls())
library(rJava)
library(xlsx)     #读表
library(dplyr)   #截取   很强大
library(lpSolveAPI)   #这些包要用R的32位操作系统
library(ucminf)
library(Benchmarking)
cat("\014")  


Taliban<-read.xlsx("e:/GTD/accum_Taliban.xlsx",1,encoding="UTF-8")
#选择官方定性为成功的事件
Taliban<-filter(Taliban,success==1)
names(Taliban)
#要编函数，让相同日期的数据相加合并


 aa<-function(y,m,d,pc,pk,vw,vk){
   k<-matrix(NA,ncol=7,nrow=3503)   #关键
  for(i in 1:3502) {
   if(y[i]==y[i+1] & m[i]==m[i+1] & d[i]==d[i+1]){
     pc[i+1]<-pc[i]+pc[i+1];
     pk[i+1]<-pk[i]+pk[i+1];
     vw[i+1]<-vw[i]+vw[i+1];
     vk[i+1]<-vk[i]+vk[i+1]
   }
   else{
    a<-cbind(y[i],m[i],d[i],pc[i],pk[i],vw[i],vk[i])
    k[i, ]=a
   }
  }
   return(k)
}
 
adj<-aa(y<-Taliban$iyear,
        m<-Taliban$imonth,
        d<-Taliban$iday,
        pc<-Taliban$adj_nperpcap,
        pk<-Taliban$adj_nkillter,
        vw<-Taliban$adj_nwound,
        vk<-Taliban$adj_nkill) 
 
#先计算重复日期次数，再求其中攻击类型平均值
num_rep<-function(y,m,d){
  j<-matrix(1,nrow=3503,ncol = 1)
  k<-matrix(NA,ncol=4,nrow=3503)
  for(i in 1:3502) {
    if(y[i]==y[i+1] & m[i]==m[i+1] & d[i]==d[i+1]){
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













