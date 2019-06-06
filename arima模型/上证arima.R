setwd("c:/Users/AresL/Desktop/financial_data_analysis_final/data")
library(tseries)
library(forecast)
da1 = read.csv("上证50.csv", header = T)
head(da1)
logSZ50Pri = log((da1[c(168: 2167), 3]))
head(logSZ50Pri)
tdx = c(1: 2000)/250 + 2010
plot(tdx, logSZ50Pri, xlab = 'day', ylab = 'ln(price)', type='l')
title(main = '上证50指数变化')
dlogSZ50Pri = diff(logSZ50Pri) #做一阶差分
adf.test(dlogSZ50Pri) # 在5%的显著水平下拒绝原假设，一阶差分后数据平稳
par(mfcol = c(1, 2))
acf(dlogSZ50Pri, lag = 20)
pacf(dlogSZ50Pri, lag = 20)
# 观察数据ar使用3阶，ma采用两阶，建立arima(3, 0, 2)模型
SZ50_model = arima(dlogSZ50Pri, order = c(3, 0, 2), include.mean = F)
SZ50_model
Box.test(SZ50_model$residuals) # p值显著大于0.05，残差为白噪声序列，选取的模型合适，不需要使用garch模型
#计算原本数据的对数收益率
logr = diff(log(da1[c(2168:2468), 3]))
# 短期预测
SZ50_arima_forecast = forecast(SZ50_model, h = 250)
head(logr)    
short_di_sum = 0
for(i in 1: 5) {
  short_di_sum = short_di_sum + abs(SZ50_arima_forecast[[4]][i] - logr[i])
}
short_di_sum
# 中期预测
middle_di_sum = 0
for(i in 1: 60) {
  middle_di_sum = middle_di_sum + abs(SZ50_arima_forecast[[4]][i] - logr[i])
}
middle_di_sum

# 长期预测
long_di_sum = 0
for(i in 1: 250) {
  long_di_sum = long_di_sum + abs(SZ50_arima_forecast[[4]][i] - logr[i])
}
long_di_sum
