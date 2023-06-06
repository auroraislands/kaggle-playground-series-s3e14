library(ggplot2)
plott<-function(df,true_v,pred_v){#(df为整个数据)
  plot1 <- ggplot(data = df, aes(x = index)) + 
    geom_line(aes(y = true_v, linetype = "实际值", colour = "实际值"), linewidth = 0.9)+
    geom_point(aes(y = true_v,colour = "实际值"))### 画实际值得曲线
  plot2 <- plot1 +
    geom_line(aes(y = pred_v, linetype = "预测值", colour = "预测值"), linewidth = 0.9)
  
  plot2 + 
    scale_linetype_manual(values = c("实际值" = "solid", "预测值" = "4")) +
    scale_colour_manual(values = c("实际值" = "red", "预测值" = "blue"))  ### 设置图例
  plot2

}

plottt<-function(df,true_v,pred_v){#(df为整个数据)
  plot(df$index,true_v,type='o',col='darkgreen',ylim=c(2000,9000),ylab='yield',cex=1.5,lwd=3)
  par(new=TRUE)
  plot(df$index,pred_v,type='l',col='darkblue',ylim=c(2000,9000),ylab='yield',lwd=3)
  par(new=TRUE)
  plot(df$index,pred_v,pch=4,col='darkblue',ylim=c(2000,9000),ylab='yield',cex=1.5,lwd=3)
  legend("topright",
                legend=c("真实值","预测值"),lty = c(1,1),pch=c(1,4),
                col=c('darkgreen','darkblue'),cex=0.8) 
}
