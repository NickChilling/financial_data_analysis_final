library(readr)
library(keras)
setwd("c:/users/zanzh/OneDrive/研一下/金融数据分析/LSTM_VS_ARIMA/")
hs300 <- read_csv('data/沪深300.csv',locale=locale(encoding='UTF-8'))
sz50 <- read_csv('data/上证50.csv')
szcz <- read_csv('data/深证.csv')
zxb <- read_csv('data/中小板.csv')


window_length = 6
raw_data <- hs300

