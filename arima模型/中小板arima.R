setwd("c:/Users/AresL/Desktop/financial_data_analysis_final/data")
library(tseries)
library(forecast)
da2 = read.csv("中小板.csv", header = T)
head(da2)
logZXBPri = log((da2[c(168: 2167), 3]))
head(logZXBPri)
tdx = c(1: 2000)/250 + 2010
plot(tdx, logZXBPri, xlab = 'day', ylab = 'ln(price)', type='l')
title(main = '中小板指数变化')
dlogZXBPri = diff(logZXBPri) #做一阶差分
adf.test(dlogZXBPri) # 在5%的显著水平下拒绝原假设，一阶差分后数据平稳
par(mfcol = c(1, 2))
acf(dlogZXBPri, lag = 20)
pacf(dlogZXBPri, lag = 20)
# 观察数据ar使用2阶，ma采用1阶，建立arima(2, 0, 1)模型
ZXB_model = arima(dlogZXBPri, order = c(2, 0, 1), include.mean = F)
ZXB_model
Box.test(ZXB_model$residuals) # p值显著大于0.05，残差为白噪声序列，选取的模型合适，不需要用garch模型
#计算原本数据的对数收益率
logr = diff(log(da2[c(2168:2468), 3]))
# 短期预测
ZXB_arima_forecast = forecast(ZXB_model, h = 250)
head(logr)    
short_di_sum = 0
for(i in 1: 5) {
  short_di_sum = short_di_sum + abs(ZXB_arima_forecast[[4]][i] - logr[i])
}
short_di_sum
# 中期预测
middle_di_sum = 0
for(i in 1: 60) {
  middle_di_sum = middle_di_sum + abs(ZXB_arima_forecast[[4]][i] - logr[i])
}
middle_di_sum
# 长期预测
long_di_sum = 0
for(i in 1: 250) {
  long_di_sum = long_di_sum + abs(ZXB_arima_forecast[[4]][i] - logr[i])
}
long_di_sum
