setwd("c:/Users/AresL/Desktop/financial_data_analysis_final/data")
library(tseries)
library(forecast)
da3 = read.csv("沪深300.csv", header = T)
head(da3)
logHSPri = log((da3[c(168: 2167), 3]))
head(logHSPri)
tdx = c(1: 2000)/250 + 2010
plot(tdx, logHSPri, xlab = 'day', ylab = 'ln(price)', type='l')
title(main = '沪深300指数变化')
dlogHSPri = diff(logHSPri) #做一阶差分
adf.test(dlogHSPri) # 在5%的显著水平下拒绝原假设，一阶差分后数据平稳
par(mfcol = c(1, 2))
acf(dlogHSPri, lag = 20)
pacf(dlogHSPri, lag = 20)
# 观察数据ar使用2阶，ma采用3阶，建立arima(2, 0, 3)模型
HS_model = arima(dlogHSPri, order = c(2, 0, 3), include.mean = F)
HS_model
Box.test(HS_model$residuals) # p值显著大于0.05，残差为白噪声序列，选取的模型合适
#计算原本数据的对数收益率
da_forcast = da3[, 3]
for(i in 2168: length(da_forcast)) {
  da_forcast[i - 1] = da_forcast[i]/da_forcast[i - 1]
  da_forcast[i - 1] = log(da_forcast[i - 1])
}
real_rate = da_forcast[c(2167: length(da_forcast))]
head(real_rate)