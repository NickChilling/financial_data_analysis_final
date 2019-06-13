setwd("c:/Users/zanzh/OneDrive/研一下/金融数据分析/LSTM_VS_ARIMA/data/")
library(tseries)
library(forecast)
library(ggplot2)

#read_data
da1 = read.csv("上证50.csv", header = T)
head(da1)
logSZ50Pri = log((da1[, 3]))
head(logSZ50Pri)

# plot 
tdx = c(1: 2446)/250 + 2010.4
plot(tdx, logSZ50Pri, xlab = 'day', ylab = 'ln(price)', type='l')  
title(main = '上证50指数变化')

# compute log return
dlogSZ50Pri_full = diff(logSZ50Pri) #做一阶差分
dlogSZ50Pri = dlogSZ50Pri_full[1:2295] # 划分数据集 2296-2445为测试集 共150哥样本

adf.test(dlogSZ50Pri) # 在5%的显著水平下拒绝原假设，一阶差分后数据平稳  # TODO adf检验的结果保存为文本放到graph/下面

par(mfcol = c(1, 2))
acf(dlogSZ50Pri, lag = 20) 
pacf(dlogSZ50Pri, lag = 20)

 
# 观察数据ar使用3阶，ma采用两阶，建立arima(3, 0, 2)模型
SZ50_model = arima(dlogSZ50Pri, order = c(3, 0, 2), include.mean = F)
SZ50_model
Box.test(SZ50_model$residuals) # p值显著大于0.05，残差为白噪声序列，选取的模型合适，不需要使用garch模型  #TODO 检验结果保存
#计算原本数据的对数收益率


back_test <- function(time_series,start, period_length,order){
  pred = rep(0,period_length)
  upper = rep(0,period_length)
  lower = rep(0,period_length)
  actu = time_series[start:(start+period_length-1)]
  for (i in start:(start+period_length-1)){
    model = arima(time_series[1:i],order= order,include.mean = F)
    temp = forecast(model,h=1)
    pred[i-start+1] =temp[[4]]
    upper[i-start+1] = temp$upper[1,2]
    lower[i-start+1] = temp$lower[1,2]
  }
  return(list(y=actu,y_hat=pred,upper=upper,lower=lower))
}


compute_loss <- function(result,period_length){
  result_pred = result$y_hat[1:period_length]
  result_actu = result$y[1:period_length]
  mape = mean(abs(result_actu-result_pred )/abs(result_actu))
  return(mape)
}

result = back_test(dlogSZ50Pri_full,2296,150,c(3,0,2))
paste0('short_term',compute_loss(result,5))
paste0('mid_term loss',compute_loss(result,60))
paste0('long_term loss',compute_loss(result,150))


par(mfcol = c(1,1))
plot(result$y,type= 'l',main = "comparison", xlab = 'time series', ylab='log return') # 实际值
lines(result$y_hat, col= 'red') # 预测值
lines(result$upper, col= 'grey')
lines(result$lower,col ='grey')
