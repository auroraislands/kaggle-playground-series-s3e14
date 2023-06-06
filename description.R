library(tidyverse)
setwd("D:\\大三下\\数据挖掘\\期中论文\\playground-series-s3e14")
train<-read.csv("train.csv")

###描述性统计
summary(train)


####箱型图
boxplot(train)
boxplot(train[,2])
title("clonesize")

boxplot(train[,3])
title("honeybee")

boxplot(train[,4])
title("bumbles")

boxplot(train[,5])
title("andrena")

boxplot(train[,6])
title("osmia")

boxplot(train[,7])
title("MaxOfUpperTRange")

boxplot(train[,8])
title("MinOfUpperTRange")

boxplot(train[,9])
title("AverageOfUpperTRange")

boxplot(train[,10])
title("MaxOfLowerTRange")

boxplot(train[,11])
title("MinOfLowerTRange")

boxplot(train[,12])
title("AverageOfLowerTRange")

boxplot(train[,13])
title("RainingDays")

boxplot(train[,14])
title("AverageRainingDays")

boxplot(train[,15])
title("fruitset")

boxplot(train[,16])
title("fruitmass")

boxplot(train[,17])
title("seeds")



########直方图
# configs
options(repr.plot.width = 10, repr.plot.height = 5)

default_col_1 <- '#000080' # blue
  default_col_2 <- '#008000' # green
    default_col_3 <- '#800000' # red
      col_transparency <- '40' # suffix for transparent version of color (alpha)

cat('Size training data:', n_train <- nrow(train), '\n')

# features
features <- setdiff(colnames(train),c('id','yield'))

target <- 'yield'
# distributions
for (f in features) {
  hist(train[,f], n=50, main=f,
       xlab=f,
       col=default_col_1)
  grid()
}


























