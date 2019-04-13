library(plyr)




#===============#
#====第一周=====#
#===============#

# Split-Apply-Combine的概念
# Split-Apply-Combine的作用
# Split-Apply-Combine实现的平台

#plyr切分的假设前提：切分后，数据是不重复的

# 主函数
# **ply() 函数族
# aaply()函数
# adply()函数
# alply()函数
# daply()函数
# ddply()函数
# dlply()函数
# m*ply()函数
# 
# 辅助函数
# m*ply()函数
# splat()函数
# each()函数
# colwise()函数
# failwith()函数
# as.data.frame.function()函数
# arrang()函数
# rename()函数
# count()函数
# match_df()函数
# join()函数

# 作用：拆分数据，应用函数，再组合函数作用后的数据
# 输入：array，dataframe，list三种格式
# 输出：array，dataframe，list，
#discareded（有输入没输出，如作图，用下划线表示）四种格式


#----------------------------------------------------------------#

# d*ply(.data, .variables, .fun, ..., .progress = "none")
# 参数
# .variables指定要   按其分割的变量名称



names=c("John","Mary","Alice","Peter","Roger","Phyillis")
age=c(13,15,14,13,14,13)
sex=c("Male","Female","Female","Male","Male","Female")
data=data.frame(names,age,sex)


#age mean考虑单独构建函数
amean=function(data){
  agemean=mean(data[,2])
  return(agemean)
}
daply(data,.(age),.fun=amean)#区分变量age
daply(data,.(sex),.fun=amean)
daply(data,.(age,sex),.fun=amean)#得出array的结果形式
ddply(data,.(sex),.fun=amean)    #得出dataframe的形式
dlply(data,.(sex),.fun=amean)    #list的输出格式

#----------------------------------------------------------------#

l*ply(.data, .fun, ..., .progress = "none")
# 列表类型的数据是最简单的数据，因为它已经被分割成一个个了
#（也就是列表数据的一个个元素），所以这类函数没有参数用来描述是按什么进行切分的。
# 代码
a=c(1,2,3,4,1,5,7,8,9,4,2)
b=c(1,2,5,7,6,4,8,7)
c=c(4,8,9,1,2,3,1)
list=list(a,b,c)
llply(list,mean)
laply(list,mean)
ldply(list,mean)




#===============#
#====第二周=====#
#===============#

# splat()函数
# 作用：与使用众多的参数不同，该函数把原函数中多个参数打包为一个list作为参数，然后输出新的函数
# 优点：当你想把数据框或者数组里的   一行   的数据作为参数赋给一个函数，用splat()函数就可以省去人为把数据框拆分的麻烦
# 例子
head(mtcars,5)
hp_per_cyl<-function(hp, cyl,...) {#没加,...就运行不出来，啥原理？
  hp/ cyl
}
splat(hp_per_cyl)(mtcars[1,])
splat(hp_per_cyl)(mtcars)

#----------------------------------------------------------------#

# each(.fun)
# 作用：用一系列的函数作用在输入的数据上，并返回一个已命名的向量
# 不足：不能给作用的函数指定附加的参数
# 例子：fun<-function(x) c(min = min(x), max = max(x),mean=mean(x))
# 与each(min,max,mean)的作用相同
a=c(1,2,3,4,1,5,7,8,9,4,2)
each(min,max,mean)(a)
each(length,mean,var)(rnorm(100))
fun<-function(x) c(min = min(x), max = max(x),mean=mean(x))
fun(a)

#----------------------------------------------------------------#

# colwise(.fun, .cols, …)
# 作用：把作用于数据框   行向量   的函数（如mean，median）
#转化为作用于数据框     列向量     的函数
# ，可以结合base R 的函数使用，与d*ply一起使用时十分方便
# 参数: .fun是要转化的函数；.cols可以是测试数据框的列是否应包含的判别函数或者是
# ·要包含的列的名称
# 另外还有衍生的catcolwise()和numcolwise()函数，
# 它们分别针对的是函数只在离散和数值型的变量上操作


#例子：统计baseball各列向量的缺失值个数
require(plyr)
head(baseball,5)

#-------------超级有用的自定义函数------------#
nmissing=function(x){
  sum(is.na(x))
}

colwise(nmissing)(baseball)

f=colwise(nmissing)#定义为了新函数

f(baseball)


ddply(baseball,.(year),colwise(nmissing))
#只看某些列的缺失值
#写法一
ddply(baseball,.(year),colwise(nmissing,.(sb,cs,so)))
#写法二 （我比较习惯）
ddply(baseball,.(year),colwise(nmissing,c("sb","cs","so")))
#写法三  （python风格）
ddply(baseball,.(year),colwise(nmissing,~sb+cs+so))

str(baseball)
#只作用在字符型的数据上
ddply(baseball,.(year),colwise(nmissing,is.character))
ddply(baseball,.(year),catcolwise(nmissing))#等价写法
#只作用在数值型格式的数据上
ddply(baseball,.(year),colwise(nmissing,is.numeric))
ddply(baseball,.(year),numcolwise(nmissing))#等价写法

ddply(baseball,.(year),colwise(nmissing,is.discrete))

#----------------------------------------------------------------#

failwith(default=NULL, f, quiet=FALSE)
# 作用：修正一个函数，使得当该函数出现错误时返回一个默认值
# 参数：default；f是要修正的函数；quiet是设定错误信息是否显示
# ，默认值为FALSE，为显是设定要返回的默认值示错误信息

f=function(x){
  if(x==1)
    stop("Error!")
  else
    0
}

f(1)
f(2)
#修改报错信息
safef=failwith(NULL,f)
safef(1)
safef(2)
safef=failwith(NULL,f,quiet=TRUE)#沉默报错信息
safef(1)
safef(2)

#-------------------------排序---------------------------------------#

arrange(df, .(var1), .(var2))
# 作用：按照列给数据框排序
# 参数：df为数据框；.var是要按照排序的变量

mtcars
head(mtcars)
arrange(mtcars,cyl,disp)#先排cyl，再排disp
Arrange(mtcars,disp,cyl)

# 但是可以发现这样排序的话车子的名称（即行名）消失了，与初衷不符合，
# 下面我们不妨把行名加上去再重新排序
cars=cbind(vehicle=row.names(mtcars),mtcars)
arrange(cars,cyl,disp)
arrange(cars,cyl,desc(disp))#按降序排列 descend  .v   descent .n



#第二个视频
#----------------------------------------------------------------#

rename(x, replace, warn_missing=TRUE)
# 作用：通过名字修改名字，而不是根据它的位置
# 参数：x是要操作的数据；replace是指定的替换的字符向量（包括新的和旧的字符）
# ；warm_missing是指
# 定当旧的字符不存在于x的时候，是否显示错误信息，默认值为TRUE，表示显示此信息

x=c("a"=1,"b"=2,"c"=3,"d"="c")
x

rename(x,replace=c("c"="e"))
rename(x,replace=c("e"="c"),warn_missing=FALSE)

#----------------------------------------------------------------#

count(df,vars=NULL ,wt_ar=NULL)
# 作用：数变量中观测值出现的个数
# 参数：df是要处理的数据框；vars是指定要进行数数的变量；wt_var是指定作为权重的变量

a=data.frame(names=c("a","b","c","d","a","a","a","b","b","c"),
             wt=c(1,1,1,1,2,2,2,2,2,2))
a
count(a,vars="names")
count(a,vars="names",wt_var="wt")

count(a,"names","wt")    #"wt"为权重
count(a,c("names","wt")) #wt与names综合一起计算

#----------------------------------------------------------------#
require(plyr)
head(baseball,5)


match_df(x, y, on=NULL)
# 作用：从一个数据框中提取与另一个数据框中相同的行
# 参数：x是原始的需要提取的数据框；y是用来找出相同行的另一个数据框
# ；on是指定要来比对的变量，默认为比较两个数据框中所有的变量

count(baseball,"id")  #统计累计出现次数
longterm=subset(count(baseball,"id"),freq>25)
head(longterm)
bb_longterm=match_df(baseball,longterm,on="id")
bb_longterm

#----------------------------------------------------------------#

join(x, y, by=NULL, type="left", match="all")
# 作用：联合两个数据框
# 参数：x，y是两个数据框；by是指定要联合的变量
# ，默认值为所有的变量；type是指定联合的方式

x1=c(1,2,3,4)
x2=c(5,6,7,8)
x=data.frame(x1,x2)

y1=x1*10
y=data.frame(y1,x2)
y[,2]=c(1,2,6,7)
join(x,y,by="x2")#默认左匹配

join(x,y,"x2",type="inner")#只返回真值
join(x,y,"x2",type="right")#按y匹配
join(x,y,"x2",type="full") #全匹配
y[,2]=c(6,6,6,6)
join(x,y,"x2",type="inner",match="all")
join(x,y,"x2",type="inner",match="first")


#join()函数运行速度比较快
first <-ddply(baseball, "id", summarise, first = min(year))

#system.time还可以这么用！！！！！！！！
system.time(b2 <-merge(baseball, first, by = "id", all.x= TRUE))
system.time(b3 <-join(baseball, first, by = "id"))



#===============#
#====第三周=====#
#===============#

# Baseball数据集包含了全美国职业棒球球手15年或以上的击球记录
#  id：棒球选手
#  year：记录的年份
#  rbi：runs batted in，该球员在某一赛季的击球的跑垒得分
#  ab：number of times at bat，击球次数

head(baseball,6)
str(baseball)
baberuth <- subset(baseball, id == "ruthba01")
#baberuth1<-baseball[baseball$id=="ruthba01",]#我的习惯写法

system.time(baberuth <- subset(baseball, id == "ruthba01"))
system.time(baberuth1<-baseball[baseball$id=="ruthba01",])
#在baberuth数据集中添加变量
baberuth <- transform(baberuth, cyear = year - min(year) + 1)


#先分类，然后添加变量
baseball <- ddply(baseball, .(id), transform,cyear = year - min(year) + 1)

#生成所有人的rbi/ab的时间序列图并保存到pdf中
baseball <- subset(baseball, ab >= 25)
xlim <- range(baseball$cyear, na.rm=TRUE)
ylim <- range(baseball$rbi / baseball$ab, na.rm=TRUE)
plotpattern <- function(df) {
  qplot(cyear, rbi / ab, 
        data = df, geom = "line", 
        xlim =xlim, ylim = ylim)
  }
pdf("paths.pdf", width = 8, height = 4)
d_ply(baseball, .(reorder(id, rbi / ab)), failwith(NA, plotpattern),.print=TRUE)
dev.off()


#第二个视频
#----------------------------------------------------------------#
Ozone
#  数据集介绍：
#  Ozone数据集是一个三维数组，记录了24×24个空间网格内，从1995年1月到2000年
# 12月，共72个时间点上，中美洲每月的平均臭氧水平。
#  前两维分别表示纬度和经度，第三维表示时间。
head(ozone)

























































