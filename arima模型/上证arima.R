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
Box.test(SZ50_model$residuals) # p值显著大于0.05，残差为白噪声序列，选取的模型合适
#计算原本数据的对数收益率
da_forcast = da1[, 3]
for(i in 2168: length(da_forcast)) {
  da_forcast[i - 1] = da_forcast[i]/da_forcast[i - 1]
  da_forcast[i - 1] = log(da_forcast[i - 1])
}
real_rate = da_forcast[c(2167: length(da_forcast))]
head(real_rate)
# 计算短期百分比误差的绝对值之和
SZ50_arima_forecast = forecast(SZ50_model, h = 5)
# sum(abs(real_rate[c(1:5)] - forecast(SZ50_model, h = 5)[, 1]))
forecast_rate = SZ50_arima_forecast[4]
sum(abs(real_rate[c(1: 5)] - forecast_rate))
    