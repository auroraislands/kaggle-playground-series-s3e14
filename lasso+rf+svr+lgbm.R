#清理内存----
# rm(list=ls())
# gc()

library(corrplot)
library(ggcorrplot)
library(tidyverse)
library(tidymodels) # meta package of all tidymodels packages
library(skimr) # Useful for "skimming" through a dataset
library(DataExplorer) # Alternative/additional to skimr
library(GGally) # enables some nice plots e.g. of correlations of predictors
#library(gt)
library(repr)
library(randomForest)
library(corrgram)
library(caret)
library(car)
#lasso
library(glmnet)
library(foreign)
#广义可加
library(mgcv)
library(splines)

#library(h2o)


setwd("D:\\大学\\大三下\\数据挖掘\\期中论文\\playground-series-s3e14")
train<-read.csv('train.csv')
train1<-train
test<-read.csv('test.csv')
test1<-test
#test<-test[,2:17]


#标准化or归一化----
#标准化
train=scale(train, center = TRUE, scale = TRUE)
test=scale(test, center = TRUE, scale = TRUE)

#相关矩阵和plot----

#pairs(train)
corrgram(train[,-1],order=FALSE,lower.panel = panel.shade,
         upper.panel = panel.cor)
#corrgram(test[,-1],order=TRUE,lower.panel = panel.shade,
#         upper.panel =panel.cor,diag.panel = panel.density)
#ggcorrplot(cor(test), hc.order = TRUE, type = "lower", lab = TRUE)
#corrplot(cor(test),method = "ellipse", type = "upper")

#EDA----
options(repr.plot.width=24, repr.plot.height = 10)

colnames(train)[!(colnames(train) %in% colnames(test))]

# skim(train) # Commented out, becomes Kaggle notebooks struggle with this

introduce(train)
plot_intro(train, ggtheme=theme_bw(base_size=24))

plot_histogram(train, ggtheme=theme_bw(base_size=24))

options(repr.plot.width=28, repr.plot.height = 28)
plot_correlation(na.omit(train %>% mutate(pollinators=honeybee + bumbles + andrena + osmia)) %>% relocate(clonesize, pollinators) %>% dplyr::select(-id), 
                 ggtheme=theme_bw(base_size=32))
options(repr.plot.width=24, repr.plot.height = 10)

# Select the variables you want to analyze
selected_vars <- c('clonesize', 'honeybee', 'bumbles', 'andrena', 'osmia', 'MaxOfUpperTRange', 'MinOfUpperTRange',
                   'AverageOfUpperTRange', 'MaxOfLowerTRange', 'MinOfLowerTRange', 'AverageOfLowerTRange', 
                   'RainingDays', 'AverageRainingDays', 'fruitset', 'fruitmass', 'seeds', 'pollinators')

# Create a scatterplot matrix
scatterplot_matrix <- ggpairs(
  train %>% mutate(pollinators=honeybee + bumbles + andrena + osmia),
  columns = selected_vars,
  diag = list(continuous = wrap("barDiag", bins = 25)), # setting bins explicity avoids a warning message
  lower = list(continuous = "smooth"), 
  upper = list(continuous = "cor"),
  title = "Scatterplot Matrix with Histograms and Correlations"
)

options(repr.plot.width=28, repr.plot.height = 28)
print(scatterplot_matrix)
options(repr.plot.width=24, repr.plot.height = 10)


#随机森林特征选择----
#将数据集分为训练集和测试集,比例为7:3
set.seed(123)

train_sub = sample(nrow(train),7/10*nrow(train))
train_data = train[train_sub,]
test_data = train[-train_sub,]
train_id=train_data[,1]
train_data = train_data[,-1]
test_id=test_data[,1]
test_data = test_data[,-1]

train1_sub = sample(nrow(train1),7/10*nrow(train1))
train1_data = train1[train1_sub,]
test1_data = train1[-train1_sub,]
colnames(test1_data)


train_data<-data.frame(train_data)
yy = as.factor(train_data$yield)

b_randomforest <- randomForest(yy~clonesize+honeybee+bumbles+andrena+
                                 AverageOfUpperTRange+AverageRainingDays+
                                 RainingDays+fruitset+fruitmass+seeds,
                               data = train_data,
                               ntree =1000,
                               mtry=3,
                               importance=TRUE ,
                               proximity=TRUE)
#查看变量的重要性
b_randomforest$importance
varImpPlot(b_randomforest, main = "variable importance")
# #全部变量的随机森林
# a_randomforest <- randomForest(yy~clonesize+honeybee+bumbles+andrena+osmia+
#                                  MaxOfUpperTRange+MinOfUpperTRange+AverageOfUpperTRange+
#                                  MaxOfLowerTRange+MinOfLowerTRange+AverageOfLowerTRange+
#                                  RainingDays+AverageRainingDays+fruitset+fruitmass+seeds,
#                                   data = train_data,
#                                   ntree =1000,
#                                   mtry=3,
#                                   importance=TRUE ,
#                                   proximity=TRUE)
# #查看变量的重要性
# a_randomforest$importance
# varImpPlot(a_randomforest, main = "variable importance")

#PCA降维----
# 由于量纲差别较大，我们选取从相关矩阵出发求解主成分
# 相关矩阵
#x<-train[,15:17]#三个变量
x<-test[,15:17]
sigm <-cov(x)
xdata<-eigen(sigm)
xdata
# 特征值
lam<-xdata$values;lam
p<-length(lam)
p
# 特征向量
gam<-xdata$vectors
colnames(gam)<-paste("vec",sep="",1:p)
print(gam[,1:2])
# 碎石图
plot(lam, type="o", xlab="主成分序号", ylab="特征值")
# 主成分
PC=princomp(x,cor=F)#从相关矩阵出发
summary(PC)
#由于主成分2的累计贡献率96.14%>85%
PC$loadings[,1:2]
PC$scores[,1:2]
#write.csv(file = "PCA_TRAIN.csv",PC$scores[,1:2])
#write.csv(file = "PCA_TEST.csv",PC$scores[,1:2])


#多元线性回归----
#多元线性回归因变量~.  就能表示其余的都作为自变量了
train_data<-data.frame(train_data)
lm=lm(yield~.,data = train_data)
summary(lm)
#膨胀因子看多重共线性存在，几个变量的vif>>10
vif(lm)

#利用lasso回归选择变量

#输入矩阵 x 和因向量 y
x = as.matrix(train[,2:17]) 
y = as.matrix(train[,18])
#拟合模型 glmnet
f1 <- glmnet(x, y, family="mgaussian", nlambda=500,alpha=1) 
# 这里alpha=1为LASSO回归，如果等于0就是岭回归
# 参数 family 规定了回归模型的类型：
# family="gaussian" 适用于一维连续因变量（univariate）
# family="mgaussian" 适用于多维连续因变量（multivariate）
# family="poisson" 适用于非负次数因变量（count）
# family="binomial" 适用于二元离散因变量（binary）
# family="multinomial" 适用于多元离散因变量（category）

#把f1结果输出
print(f1)
# 从左到右显示了非零系数的数量（Df），解释的（零）偏差百分比（%dev）和λ（Lambda）的值
#f：自由度，表示使用的变量数量，
#%Dev：代表了由模型解释的残差的比例。对于线性模型来说就是模型拟合的R^2(R-squred)。它在0%和100%之间，越接近100%说明模型包括了越多样本的信息，表现越好，如果是0%，说明模型的预测结果还不如直接把因变量的均值作为预测值来的有效。解释的残差百分比越高越好，但是构建模型使用的基因的数量也不能太多，需要取一个折中值。
#Lambda：表示使用的正则化系数，是构建模型的重要参数。数量和nlambda的值一致。

#plot(f1)
#plot(f1,xvar = 'lambda',label = F)#画不出来

# Lasso筛选变量动态过程图
# la.eq <- glmnet(x, y, family="mgaussian", 
#                 intercept = F, alpha=1, nlambda=100) 
# lambda <- 0.01
# # plot
# plot(la.eq,xvar = "lambda", label = F)
# # 也可以用下面的方法绘制
matplot(log(f1$lambda),t(f1$beta),type="l", main="Lasso", lwd=2)
#legend("topright", legend=colnames(x),cex=0.5,bty = 'n')  
# #交叉验证
# predict(f1, newx=x[2:17,], type = "response")
cvfit = cv.glmnet(x, y, type.measure = "mae", nfolds = 9)
# 这里的type.measure是用来指定交叉验证选取模型时希望最小化的目标参量，对于Logistic回归有以下几种选择:
# type.measure=deviance 使用deviance，即-2倍的Log-likelihood
# type.measure=mse 使用拟合因变量与实际应变量的mean squred error
# type.measure=mae 使用mean absolute error
# type.measure=class 使用模型分类的错误率(missclassification error)
# type.measure=auc 使用area under the ROC curve，是现在最流行的综合考量模型性能的一种参数

#cvfit=cv.glmnet(x,y)
plot(cvfit)
smin<-cvfit$lambda.min#求出最小值
sm<-cvfit$lambda.1se#求出最小值一个标准误的λ值
#带入
l.coef2<-coef(cvfit$glmnet.fit,s=smin,exact = F)
l.coef1<-coef(cvfit$glmnet.fit,s=sm,exact = F)
l.coef1
l.coef2
# #另一个lasso函数
# library(lars)
# lar1 <-lars(x,y,type = "lasso")
# plot(lar1)
# summary(lar1)

#lasso选择变量后的lm----
mod1<-lm(yield~clonesize+honeybee+bumbles+
           AverageOfUpperTRange+RainingDays+
           fruitset+fruitmass+seeds,data = train_data)
#mod11<-lm(as.numeric(yield)~fruitset+fruitmass+seeds,data = data.frame(train_data))
#summary(mod11)
summary(mod1)
mod1s<-stats::step(mod1,direction='both')#逐步回归
summary(mod1s)
drop1(mod1s)
# 
# mod1s<-lm(yield~clonesize+bumbles+
#            AverageOfUpperTRange+RainingDays+
#            fruitset+fruitmass+seeds,data = train_data)

#训练集预测效果
# 1. train的三分测试集 
new_test<-data.frame(test_data[,c('clonesize','bumbles','AverageOfUpperTRange','RainingDays','fruitset','fruitmass','seeds')])
pred.int <- predict(mod1s, newdata=new_test)
pred.test.guiyi<-(pred.int * sd(train1[,18])) + mean(train1[,18]) 
test_data_guiyi<-(test_data[,c('yield')]* sd(train1[,18])) + mean(train1[,18]) 
write.csv(file = "a.mlr.csv",data.frame(pred.test.guiyi,test_data_guiyi))
ppe(trueValue = test_data_guiyi,predValue =pred.test.guiyi )

t1<-data.frame(test[,c('clonesize','bumbles','AverageOfUpperTRange','RainingDays','fruitset','fruitmass','seeds')])
pred.test<-predict(mod1_s,newdata=t1)
pred.test.guiyi<-(pred.test * sd(train1[,18])) + mean(train1[,18]) 
write.csv(file = "mlr.csv",pred.test.guiyi)

#训练集
new_test<-data.frame(train_data[,c('clonesize','bumbles','AverageOfUpperTRange','RainingDays','fruitset','fruitmass','seeds')])
pred.int <- predict(mod1s, newdata=new_test)
pred.test.guiyi<-(pred.int * sd(train1[,18])) + mean(train1[,18]) 
train_data_guiyi<-(train_data[,c('yield')]* sd(train1[,18])) + mean(train1[,18]) 
ppe(trueValue = train_data_guiyi,predValue =pred.test.guiyi )


#随机森林选择变量后的lm----
mod2<-lm(yield~fruitset+fruitmass+seeds,data = data.frame(train_data))
summary(mod2)
pred.t2<-predict(mod2,newdata=t1)
pred.t2.guiyi<-(pred.t2 * sd(train1[,18])) + mean(train1[,18]) 
#write.csv(file = "mls_3v.csv",pred.t2.guiyi)


#广义可加模型----


plot(train[,18],train[,6])
model.gam <- gam(yield~clonesize+bumbles+
                   AverageOfUpperTRange+RainingDays+
                   fruitset+fruitmass+seeds,data = data.frame(train_data))
summary(model.gam)#查看模型概况
plot(model.gam,residuals = T,se=T,pch='.')

pred.t<-predict(model.gam,newdata=t1)
pred.t.guiyi<-(pred.t * sd(train1[,18])) + mean(train1[,18]) 
#write.csv(file = "gam.csv",pred.t.guiyi)


#LSSVM----
trd.x<-train_data[,c('clonesize','bumbles','AverageOfUpperTRange','RainingDays','fruitset','fruitmass','seeds')]
ted.x<-test_data[,c('clonesize','bumbles','AverageOfUpperTRange','RainingDays','fruitset','fruitmass','seeds')]
train.Y.Valid<-test_data[,'yield']
#训练集的行数和列数
n=nrow(trd.x)
nc=ncol(trd.x)
#将训练集的输入和输出分别转成矩阵x和y
x=as.matrix(trd.x)
y=as.matrix(train_data$yield)
y=rbind(0,y)
#长度为n的单位列向量
I=t(t(rep(1,n)))
#将测试集的输入转成矩阵xvt
xvt=as.matrix(ted.x)
#测试集的行数
nvt=nrow(ted.x)

#根据参数向量x，计算在测试集上的均方误差，该值越小，预测效果越好
getError<-function(argx)
{
  sigma=argx[1]
  gama=argx[2]
  omiga=matrix(rep(0,n*n),ncol=n)
  for(i in 1:n)
  {
    xi=x[i,]
    deltaX=(x-matrix(rep(xi,n),byrow=T,ncol=nc))^2
    omiga[i,]=exp(-rowSums(deltaX)/(sigma^2))
  }
  #构建矩阵A
  A=(omiga+(1/gama)*diag(n))
  A=cbind(I,A)
  A=rbind(c(0,t(I)),A)
  #求b和alpha参数
  b_alpha=solve(A)%*%y
  b=b_alpha[1,]
  alpha=b_alpha[-1,]
  
  #基于train.X.Valid进行预测
  ypred=NULL
  for(i in 1:nvt)
  {
    xvti=xvt[i,]
    deltaX=(x-matrix(rep(xvti,n),byrow=T,ncol=nc))^2
    ypred=rbind(ypred,exp(-rowSums(deltaX)/(sigma^2))%*%t(t(alpha))+b)
  }
  error=mean((train.Y.Valid-ypred)^2)
  print(paste("sigma:",sigma," gama:",gama," error:",error))
  return(error)
}

library(genalg)
#定义监控函数
monitor<-function(rbga0)
{
  #打印种群中第一个个体的值population[1,]
  print(rbga0$population[1,])
}
rbgaObj<-rbga(stringMin = rep(-5,7), stringMax = rep(5,7), popSize = 100, 
              iters = 5, mutationChance = 0.01, monitorFunc = monitor, 
              evalFunc = getError, verbose = TRUE)

argvc=rbgaObj$population[which.min(rbgaObj$evaluations),]
argvc

getError(argvc)

rbgaObj$iters


#参数
sigma=argvc[1]
gama=argvc[2]

sigma=-0.109196
gama=4.955011

#总体样本集
n=nrow(train[,2:17])
nc=ncol(train[,2:17])
x=as.matrix(train[,2:17])
y=as.matrix(train[,18])
y=rbind(0,y)
I=t(t(rep(1,n)))
omiga=matrix(rep(0,n*n),ncol=n)
for(i in 1:n)
{
  xi=x[i,]
  deltaX=(x-matrix(rep(xi,n),byrow=T,ncol=nc))^2
  omiga[i,]=exp(-rowSums(deltaX)/(sigma^2))
}
#构建矩阵A
A=(omiga+(1/gama)*diag(n))
A=cbind(I,A)
A=rbind(c(0,t(I)),A)
#求b和alpha参数
b_alpha=solve(A)%*%y
b=b_alpha[1,]
alpha=b_alpha[-1,]


#基于train.Y.Vliad进行预测
validN=nrow(test_data)
validData.Y=test_data
validData.X=(test_data-
               t(matrix(rep(scale.data.center,validN),ncol=validN)))/
  t(matrix(rep(scale.data.scale,validN),ncol=validN))
xvt=as.matrix(validData.X)
nvt=nrow(validData.X)
ypred=NULL
for(i in 1:nvt)
{
  xvti=xvt[i,]
  deltaX=(x-matrix(rep(xvti,n),byrow=T,ncol=nc))^2
  ypred=rbind(ypred,exp(-rowSums(deltaX)/(sigma^2))%*%t(t(alpha))+b)
}
pred.obj=ypred
pred.obj=t(apply(pred.obj,1,splinex))
dim(pred.obj)

pred.obj[1:10,1:5]



#通过绘图，观察预测的情况
diffsum=0
allsum=0
for(i in 1:validN)
{
  tiff(filename=paste("C:\\Users\\haolin\\Desktop\\pics\\",i,".tiff"))
  rag=range(c(pred.obj[i,],as.matrix(validData.Y[i,])[1,]))
  plot(as.matrix(validData.Y[i,])[1,],col='darkgreen',lwd=3,type='l')
  lines(pred.obj[i,],ylim=rag,col='red',lty=3,lwd=3)
  dev.off()
  diffsum=diffsum+sum(abs(pred.obj[i,]-as.matrix(validData.Y[i,])[1,]))
  allsum=allsum+sum(as.matrix(validData.Y[i,])[1,])
}
diffsum/allsum
## [1] 0.04515431


rates=NULL
for(i in 1:validN)
{
  rates=c(rates,abs(pred.obj[i,]-as.matrix(validData.Y[i,])[1,])/
            as.matrix(validData.Y[i,])[1,])
}
#绘制直方图
par(mfrow=c(2,1))
hist(rates,breaks=100)
hist(rates[rates<0.2],breaks=100)
par(mfrow=c(1,1))
#平均绝对百分误差
mean(rates)
## [1] 0.04504269


# SVR----
library(e1071)
#type参数选择“eps-regression”，即上面介绍的SVR回归分析，kernel参数指定核函数，“radial”即为高斯核。
model_1 <- svm(yield~clonesize+bumbles+
        AverageOfUpperTRange+RainingDays+
        fruitset+fruitmass+seeds,data = train_data,
        type="eps-regression",kernel = "radial")
summary(model_1)

?svm

# ggplot()+
#   geom_line(aes(train_data$yield, predict(model_1, train_data)), color = "red")

#test_data预测
t1<-data.frame(test_data[,c('clonesize','bumbles','AverageOfUpperTRange','RainingDays','fruitset','fruitmass','seeds')])
pred_svm <- predict(model_1,t1)
pred_svm
pred.svm.guiyi<-(pred_svm * sd(train1[,18])) + mean(train1[,18]) 
write.csv(file = "a.svr.csv",data.frame(pred.svm.guiyi,test_data_guiyi))
ppe(trueValue = test_data_guiyi,predValue = pred.svm.guiyi)

#train预测
t1<-data.frame(train_data[,c('clonesize','bumbles','AverageOfUpperTRange','RainingDays','fruitset','fruitmass','seeds')])
pred_svm <- predict(model_1,t1)
#pred_svm
pred.svm.guiyi<-(pred_svm * sd(train1[,18])) + mean(train1[,18]) 

ppe(trueValue = train_data_guiyi,predValue = pred.svm.guiyi)

#test
t1<-data.frame(test[,c('clonesize','bumbles','AverageOfUpperTRange','RainingDays','fruitset','fruitmass','seeds')])
pred_svm<-predict(model_1,newdata=t1)
pred.svm.guiyi<-(pred_svm*sd(train1[,18])) + mean(train1[,18]) 
write.csv(file = "svr.csv",pred.svm.guiyi)

# 
# sum_1 <- 
#   data.frame(CO = train$一氧化碳, 
#              CO_f = (model_1$fitted*sd(train$一氧化碳)) + mean(train$一氧化碳), 
#              group = "训练集") %>%
#   rbind(data.frame(CO = test$一氧化碳, 
#                    CO_f = (predict(model_1, test)*sd(train$一氧化碳)) + mean(train$一氧化碳), 
#                    group = "测试集")) %>%
#   as.data.frame() %>%
#   mutate(CO_f = ifelse(CO_f <0, 0, CO_f)) %>%
#   mutate(MAE = abs(CO_f - CO), 
#          MAPE = MAE/abs(CO), 
#          RMSE = MAE^2,
#          R2 = (CO - mean(CO))^2, 
#          R = (abs(CO_f - CO))^2)
# 
# p1 <- 
#   ggplot(sum_1, aes(x=CO, y=CO_f, colour=group)) + 
#   geom_point() +
#   geom_abline(intercept=0,slope=1) +
#   xlim(0, 0.014) +
#   ylim(0, 0.014) +
#   labs(x = "真实值", y = "预测值", title = "SVR")

#tune调参优化----
# A<-tune(model_1,ranges = list(gamma = 2^(-2:2), cost = 2^(1:4)),
#             tunecontrol = tune.control(sampling = "fix"))

#遗传算法优化----
library(GA)
# Setup the data for cross-validation 
K = 5 # 5-fold cross-validation 
fold_inds <- sample(1:K, nrow(train[,-1]), replace = TRUE) 
lst_CV_data <- lapply(1:K, function(i) list(
  train_data1 = train[fold_inds != i, , drop = FALSE], 
  test_data1 = train[fold_inds == i, , drop = FALSE]))

# Given the values of parameters 'cost', 'gamma' and 'epsilon', return the rmse of the model over the test data 
evalParams <- function(train_data1, test_data1, cost, gamma) { 
  # Train 
  model <- svm(yield~clonesize+bumbles+
                 AverageOfUpperTRange+RainingDays+
                 fruitset+fruitmass+seeds, data = train_data1, 
               cost = cost, gamma = gamma, 
               type = "eps-regression", kernel = "radial") 
  # Test 
  rmse <- mean((predict(model, newdata = test_data1)-test_data1[,18])^2) 
  return (rmse) 
} 


# Fitness function (to be maximized) 
# Parameter vector x is: (cost, gamma, epsilon) 
fitnessFunc <- function(x, Lst_CV_Data) { 
  # Retrieve the SVM parameters 
  cost_val <- x[1] 
  gamma_val <- x[2]
  # Use cross-validation to estimate the RMSE for each split of the dataset 
  rmse_vals <- sapply(Lst_CV_Data, function(in_data) with(in_data, 
                                                          evalParams(train_data1, test_data1, cost_val, gamma_val))) 
  
  # As fitness measure, return minus the average rmse (over the cross-validation folds), 
  # so that by maximizing fitness we are minimizing the rmse 
  return (-mean(rmse_vals)) 
} 

# Range of the parameter values to be tested 
# Parameters are: (cost, gamma, epsilon) 
theta_min <- c(cost = 1e-4, gamma = 1e-3) 
theta_max <- c(cost = 100, gamma = 50) 

# Run the genetic algorithm 
results <- ga(type = "real-valued", fitness = fitnessFunc, lst_CV_data, 
              names = names(theta_min), 
              min = theta_min, max = theta_max, 
              popSize = 50, maxiter = 100) 

summary(results) 

#lightbgm----
library(lightgbm)



#*归一化所有变量做lgbm----
#创建数据
dtrain1 <- lgb.Dataset(data=as.matrix(train_data[,1:16]), 
                       label =train_data[,17])

#设定参数
params<- list( boosting_type = 'gbdt',         
               objective = 'regression_l1',        
               metric = 'mae',
               learning_rate = 0.04,         
               max_depth =5,        
               num_leaves = 40,        
               sub_feature = 0.1,         
               sub_row = 0.1,         
               bagging_freq = 1 )
#模型训练
lgb_model <- lgb.train( params = params,
                        data = dtrain1,
                        nrounds = 5000,
                        verbose = -1)

#*调参----
library(tidymodels)
library(treesnip)
library(bonsai)

dta = train_data
# dta$clonesize<-as.factor(dta$clonesize)
# dta$honeybee<-as.factor(dta$honeybee)
# dta$bumbles<-as.factor(dta$bumbles)
# dta$andrena<-as.factor(dta$andrena)
# dta$osmia<-as.factor(dta$osmia)
# dta$MaxOfUpperTRange<-as.factor(dta$MaxOfUpperTRange)
# dta$MinOfUpperTRange<-as.factor(dta$MinOfUpperTRange)
# dta$AverageOfUpperTRange<-as.factor(dta$AverageOfUpperTRange)
# dta$MaxOfLowerTRange<-as.factor(dta$MaxOfLowerTRange)
# dta$MinOfLowerTRange<-as.factor(dta$MinOfLowerTRange)
# dta$AverageOfLowerTRange<-as.factor(dta$AverageOfLowerTRange)
# dta$RainingDays<-as.factor(dta$RainingDays)
# dta$AverageRainingDays<-as.factor(dta$AverageRainingDays)
# dta$fruitset<-as.factor(dta$fruitset)
# dta$fruitmass<-as.factor(dta$fruitmass)
# dta$seeds<-as.factor(dta$seeds)


#设置recipe
rec <- recipe(yield~.,dta)
#设置交叉验证方法
folds <- vfold_cv(dta,v=5)
#设置模型
lgb_mod <- boost_tree(
  trees = tune(), 
  learn_rate = tune(), 
  min_n = tune(), 
  tree_depth = tune(),
  loss_reduction = tune()
) %>% 
  set_engine("lightgbm") %>% 
  set_mode("regression")

#贝叶斯优化调参
#设置优化条件
ctr <- control_bayes(
  verbose = FALSE,
  no_improve = 25L,
  uncertain = Inf,
  seed = 1234,
  save_pred = TRUE,
  allow_par = TRUE
)
#贝叶斯优化
lgb_tune <- tune_bayes(object=lgb_mod,
                       preprocessor=rec,
                       resamples=folds,
                       iter=100,
                       metrics = metric_set(mae),
                       objective = exp_improve(),
                       initial =7,
                       control = ctr)

#最终模型
best <- select_best(lgb_tune,metric = "rmse")
final_mod <- finalize_model(lgb_mod,best) %>% 
  fit(yield~.,dta)


#testdata预测----


lgb_pred_t <- predict(lgb_model,as.matrix(test_data[,1:16]))
lgb_pred_t<-(lgb_pred_t * sd(train1[,18])) + mean(train1[,18]) 
write.csv(file = "aa.lgbm.csv",data.frame(lgb_pred_t,test_data_guiyi))
ppe(trueValue =test_data_guiyi,predValue = lgb_pred_t)

#traindata预测----
lgb_pred_t <- predict(lgb_model,as.matrix(train_data[,1:16]))
lgb_pred_t<-(lgb_pred_t * sd(train1[,18])) + mean(train1[,18]) 
train_data_guiyi<-(train_data[,c('yield')]* sd(train1[,18])) + mean(train1[,18]) 

ppe(trueValue =train_data_guiyi,predValue = lgb_pred_t)


#*所有变量做lgbm----
#创建数据
dtrain1 <- lgb.Dataset(data=as.matrix(train1[,2:17]), 
                      label =train1[,18])

#设定参数
params<- list( boosting_type = 'gbdt',         
               objective = 'regression_l1',        
               metric = 'mae',
               learning_rate = 0.04,         
               max_depth =5,        
               num_leaves = 40,        
               sub_feature = 0.1,         
               sub_row = 0.1,         
               bagging_freq = 1 )
#模型训练
lgb_model <- lgb.train( params = params,
                        data = dtrain1,
                        nrounds = 5000,
                        verbose = -1)
#模型预测

lgb_pred <- predict(lgb_model,as.matrix(test1[,-1]))
write.csv(file = 'lightgbm.csv',lgb_pred)

#testdata预测
lgb_pred_t <- predict(lgb_model,as.matrix(test1_data[,2:17]))
write.csv(file = "aa.lgbm.csv",data.frame(lgb_pred_t,test1_data[,18]))
ppe(trueValue = test1_data[,18],predValue = lgb_pred_t)


#*lasso选择变量做lgbm----
#创建数据
dtrain <- lgb.Dataset(data=as.matrix(train1[,c('clonesize','bumbles','AverageOfUpperTRange','RainingDays','fruitset','fruitmass','seeds')]), 
                      label =train1[,18])

#设定参数
params<- list( boosting_type = 'gbdt',         
               objective = 'regression_l1',        
               metric = 'mae',
               learning_rate = 0.04,         
               max_depth =5,        
               num_leaves = 40,        
               sub_feature = 0.1,         
               sub_row = 0.1,         
               bagging_freq = 1 )
#模型训练
lgb_model <- lgb.train( params = params,
                        data = dtrain,
                        nrounds = 5000,
                        verbose = -1)
#模型预测
lgb_pred <- predict(lgb_model,as.matrix(test1[,c('clonesize','bumbles','AverageOfUpperTRange','RainingDays','fruitset','fruitmass','seeds')]))
write.csv(file = 'lasso-lgbm.csv',lgb_pred)


#*3v变量做lgbm----
#创建数据
dtrain3 <- lgb.Dataset(data=as.matrix(train1[,c('fruitset','fruitmass','seeds')]), 
                      label =train1[,18])

#设定参数
params<- list( boosting_type = 'gbdt',         
               objective = 'regression_l1',        
               metric = 'mae',
               learning_rate = 0.04,         
               max_depth =5,        
               num_leaves = 40,        
               sub_feature = 0.1,         
               sub_row = 0.1,         
               bagging_freq = 1)
#模型训练
lgb_model <- lgb.train( params = params,
                        data = dtrain3,
                        nrounds = 5000,
                        verbose = -1)
#模型预测
lgb_pred <- predict(lgb_model,as.matrix(test1[,c('fruitset','fruitmass','seeds')]))
write.csv(file = '3v-lgbm.csv',lgb_pred)


