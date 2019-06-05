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
Box.test(ZXB_model$residuals) # p值显著大于0.05，残差为白噪声序列，选取的模型合适
#计算原本数据的对数收益率
da_forcast = da2[, 3]
for(i in 2168: length(da_forcast)) {
  da_forcast[i - 1] = da_forcast[i]/da_forcast[i - 1]
  da_forcast[i - 1] = log(da_forcast[i - 1])
}
real_rate = da_forcast[c(2167: length(da_forcast))]
head(real_rate)