#自定义函数计算数值预测模型的评估指标
#trueValue:真实值
#predValue:预测值
#return-->list(...)
library(MASS)
setwd("D:\\大学\\大三下\\数据挖掘\\期中论文\\playground-series-s3e14")
a=read.csv("0.7and0.3.csv",header=T,sep=",")

ppe(trueValue = a$X0.7true,a$X0.7train_yuce)
ppe(trueValue = na.omit(a$X0.3true),na.omit(a$X0.3test_yuce))


trueValue=a$T;

GA.predValue=a$GA;
ppe(trueValue,GA.predValue)

v3bp.predValue=a$X3v;
lasbp.predValue=a$lasso;
paf.predValue=a$paf;

a1<-a[1:200,]
#lasso的bp图
plott(df = a1,true_v = a1$TRUE.,pred_v = a1$lasso)
plottt(df = a1,true_v = a1$TRUE.,pred_v = a1$lasso)

ppe<-function(trueValue,predValue)
{
  #1.计算绝对误差（Absolute Error，简记为E）
  e.E<-trueValue-predValue
  #2.计算相对误差（Relative Error，简记为e）
  e.e<-e.E*sign(trueValue)/(abs(trueValue)+exp(-10))
  #3.计算平均绝对误差（Mean Absolute Error，简记为MAE）
  e.MAE<-mean(abs(e.E))
  #4.计算均方误差（Mean Squared Error，简记为MSE）
  e.MSE<-mean(e.E^2)
  #5.计算归一化均方误差（Normalized Mean Squared Error，简记为NMSE）
  e.NMSE<-sum(e.E^2)/sum((trueValue-mean(trueValue))^2)
  #6.计算均方根误差（Root Mean Squared Error，简记为RMSE）
  e.RMSE<-sqrt(e.MSE)
  #7.计算平均绝对百分误差（Mean Absolute Percentage Error，简记为MAPE）
  e.MAPE<-mean(abs(e.e))
  #8.计算希尔不等系数（Theil inequality coefficient，简记为TIC）
  e.TIC<-e.RMSE/(sqrt(mean(trueValue^2))+sqrt(mean(predValue^2)))
  #9.计算判定系数（Coefficient Of Determination，一般记为R^2）
  e.R2<-1-sum(e.E^2)/sum((trueValue-mean(trueValue))^2)
  return(list(e.MAE=e.MAE,e.MSE=e.MSE,e.NMSE=e.NMSE,
              e.RMSE=e.RMSE,e.MAPE=e.MAPE,e.TIC=e.TIC,e.R2=e.R2))
}


#e.E=e.E,e.e=e.e,
#pac,lasbp,v3bp,rfbp
pac<-ppe(trueValue = trueValue,predValue = paf.predValue)
lasbp<-ppe(trueValue = trueValue,predValue = lasbp.predValue)
v3bp<-ppe(trueValue = trueValue,predValue = v3bp.predValue)
rfbp<-ppe(trueValue = trueValue,predValue = rfbp.predValue)
result<-data.frame(pac,lasbp,v3bp,rfbp)
#write.csv(file = "checkresult2.csv",result)














#else----
lm.fit<-lm(Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,data=iris)
y<-iris$Petal.Width
yhat<-fitted(lm.fit)
#获得真实值的标准差
sd0<-sd(y)
#n倍标准差
n<-1
#等分为100份
k<-100
nV<-(1:(n*k))/k
vals<-NULL
for(i in nV){
  vals<-c(vals,NROW(yhat[yhat>=(y-i*sd0) & yhat<=(y+i*sd0)])/NROW(y))
}
vals<-100*vals
plot(vals,col='white',xlab=paste(k,"分",n,"倍标准差",sep=""),ylab="%累计收益率",
     main="一倍标准差累计收益图")
lines(vals,col='blue')
abline(h=100,col='gray',lty=2)



#画图----
bp<-read.csv("a.bp.csv",header=T,sep=",")
mlr<-read.csv("a.mlr.csv",header=T,sep=",")
lgbm<-read.csv("aa.lgbm.csv",header=T,sep=",")
svr<-read.csv("a.svr.csv",header=T,sep=",")

f<-lgbm[1:200,]

plottt(df = f,true_v = f[,3],pred_v = f[,2])



setwd("D:\\大学\\大三下\\数据挖掘\\期中论文\\playground-series-s3e14")
duibi=read.csv("yichuan and xiaobo.csv",header=T,sep=",")
duibi<-duibi[1:200,]
trueValue=duibi$t;
bp.predValue=duibi$bp;
xiaobo.predValue=duibi$xiaobo;

plottt(df = duibi,true_v = trueValue,pred_v = bp.predValue)

plottt(df = duibi,true_v = trueValue,pred_v = xiaobo.predValue)

#画三个对比图----
plot(duibi$index,trueValue,type='o',col='#4136A4',ylim=c(2000,9000),ylab='yield',cex=1.5,lwd=2)
par(new=TRUE)
plot(duibi$index,bp.predValue,type='l',col='#00C2A0',ylim=c(2000,9000),ylab='yield',lwd=2)
par(new=TRUE)
plot(duibi$index,bp.predValue,pch=4,col='#00C2A0',ylim=c(2000,9000),ylab='yield',cex=1.5,lwd=2)
par(new=TRUE)
plot(duibi$index,xiaobo.predValue,type='l',col='#007CD2',ylim=c(2000,9000),ylab='yield',lwd=2)
par(new=TRUE)
plot(duibi$index,xiaobo.predValue,pch=6,col='#007CD2',ylim=c(2000,9000),ylab='yield',cex=1.5,lwd=2)


legend("topright",
       legend=c("真实值","bp神经网络预测值",'小波神经网络'),
       lty = c(1,1,1),pch=c(1,4,6),
       col=c('#4136A4','#00C2A0',"#007CD2"),cex=0.8) 
