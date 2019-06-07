setwd("c:/Users/AresL/Desktop/financial_data_analysis_final/data")
library(tseries)
library(forecast)
da4 = read.csv("深证.csv", header = T)
head(da4)
logSZPri = log((da4[c(168: 2167), 3]))
head(logSZPri)
tdx = c(1: 2000)/250 + 2010
plot(tdx, logSZPri, xlab = 'day', ylab = 'ln(price)', type='l')
title(main = '深证指数变化')
dlogSZPri = diff(logSZPri) #做一阶差分
adf.test(dlogSZPri) # 在5%的显著水平下拒绝原假设，一阶差分后数据平稳
par(mfcol = c(1, 2))
acf(dlogSZPri, lag = 20)
pacf(dlogSZPri, lag = 20)
# 观察数据ar使用1阶，ma采用1阶，建立arima(1, 0, 1)模型
SZ_model = arima(dlogSZPri, order = c(1, 0, 1), include.mean = F)
SZ_model
Box.test(SZ_model$residuals) # p值显著大于0.05，残差为白噪声序列，选取的模型合适，不需要用garch模型

#计算原本数据的对数收益率
logr = diff(log(da4[c(2168:2468), 3]))
# 短期预测
SZ_arima_forecast = forecast(SZ_model, h = 250)
head(logr)    
short_di_sum = 0
for(i in 1: 5) {
  short_di_sum = short_di_sum + abs(SZ_arima_forecast[[4]][i] - logr[i])
}
short_di_sum
# 中期预测
middle_di_sum = 0
for(i in 1: 60) {
  middle_di_sum = middle_di_sum + abs(SZ_arima_forecast[[4]][i] - logr[i])
}
middle_di_sum
# 长期预测
long_di_sum = 0
for(i in 1: 250) {
  long_di_sum = long_di_sum + abs(SZ_arima_forecast[[4]][i] - logr[i])
}
long_di_sum
