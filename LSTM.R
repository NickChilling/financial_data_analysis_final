library(keras)
setwd("c:/users/zanzh/OneDrive/研一下/金融数据分析/LSTM_VS_ARIMA/")
hs300 <- read.csv("data/沪深300.csv")
sz50 <- read.csv("data/上证50.csv", encoding = "UTF-8")
szcz <- read.csv("data/深证.csv",encoding = "UTF-8")
zxb <- read.csv('data/中小板.csv',encoding = "UTF-8")


# arima backtest 是怎么跑的

# define global variable
raw_data <- hs300
time_step <-10
train_ratio <-0.7
train_means <- 0
train_stds <- 0


# data exploration
ts = seq(1,dim(hs300)[1])/244+2009.4
plot(ts,hs300[,2],type = 'l',xlab='time series',ylab = 'index',main= '沪深300线')

ts_diff = seq(1,dim(hs300)[1]-1)
lg_r = diff(log(hs300[,2]))
plot(ts_diff,lg_r,type='l',xlab= 'time series', ylab = 'log return',main='沪深300 对数收益率')


# split_datset
split.dataset <-function(dataset,split_ratio){
  samples = dim(dataset)[1]
  split_index = as.integer(samples*split_ratio)
  return list(train = dataset[1:split_index,], test=dataset[split_index:samples,]
}

# data normalization
norm.data <- function(dataset){
  numbers = dataset[,2:7]
  if (train_means==0)  {# if means and stds are not passed in (it's train dataset)    
    train_means <- apply(numbers,2,mean)
    train_stds <-apply(numbers,2,max)-apply(numbers,2,min)
    dataset[,8] = numbers[,2]
    for (i in 1:6){
      dataset[,i+1] =  (dataset[,i+1]- train_means[i])/train_stds[i]
    }
}
  else{
    for (i in 1:6){
      dataset[,i+1] =  (dataset[,i+1]- train_means[i])/train_stds[i]
    }
  }
  return (dataset)
}
make.dataset <- function(dataset,time_step){ # input data with dates 
  numbers = as.matrix(dataset[,2:7]) # 7->8
  num_sample = dim(numbers)[1]-time_step-1
  num_feature = dim(numbers)[2]
  X = array(0,dim = c(num_sample,time_step,num_feature))
  Y = array(0,dim = c(num_sample,1)) # close
  log_return  = diff(log(dataset[,8]))
  for (i in 1:num_sample){
    X[i,,] = numbers[i:(i+time_step-1),]
    Y[i,] = log_return[(i+time_step)] # 2->7
  }
  return (list(X=X,Y=Y))
}

norm_dataset <- norm.data(hs300)
dataset <- make.dataset(norm_dataset,time_step)
x<-dataset$X
y<-dataset$Y

model_stateful <- keras_model_sequential()
batch_size <- 5
model_stateful %>%
  layer_lstm(units= 10,input_shape = c(time_step,dim(x)[3]),
             batch_size = batch_size,stateful = TRUE) %>%
  layer_dense(units=1)

summary(model_stateful)
 
model_stateful %>% 
  compile(
    loss = 'mse',
    optimizer = optimizer_adam(0.001),
    metrics = list('mean_absolute_percentage_error') ) 
for (i in 1:50){
  model_stateful %>% 
    fit(x,y,epoch = 1, batch_size =batch_size,
              verbose =2,shuffle = FALSE)
  model_stateful %>% 
    reset_states()
}
# layer_batch_normalization(input_shape = c(time_step,dim(x)[3]),axis=2)

train_predict = model_stateful %>% predict(x,batch_size=batch_size)
# mape = 2600 abandon

model_2 <- keras_model_sequential()  # 2 layer model
model_2 %>%
  layer_lstm(units= 5, input_shape =c(time_step,dim(x)[3])) %>%
  layer_dense(units=1)%>%
  summary()
model_2 %>%
  compile(loss='mse',optimizer = optimizer_sgd(0.001),
          metrics= list("mean_absolute_percentage_error")) %>%
  fit(x,y,epoch=10, batch_size=batch_size,verbose=2,validation_split=0.2)
train_predict2 <- model_2 %>% predict(x)

# model referred from the papaer
make.dataset.withoutbn <- function(dataset,time_step){ # input data with dates 
  numbers = as.matrix(dataset[,2:7]) # 7->8
  num_sample = dim(numbers)[1]-time_step-1
  num_feature = dim(numbers)[2]
  X = array(0,dim = c(num_sample,time_step,num_feature))
  Y = array(0,dim = c(num_sample,1)) # close
  log_return  = diff(log(dataset[,2]))
  for (i in 1:num_sample){
    X[i,,] = numbers[i:(i+time_step-1),]
    Y[i,] = log_return[(i+time_step)] # 2->7
  }
  return (list(X=X,Y=Y))
}
trainset_bn <- make.dataset.withoutbn(sz50,time_step)
x_bn <- trainset_bn$X
y_bn <- trainset_bn$Y
batch_size_3 <- 100
model_3 <- keras_model_sequential()
model_3 %>%
  layer_batch_normalization(input_shape=c(time_step,dim(x)[3]),axis = 2) %>%
  layer_lstm(units=200, return_sequences=TRUE) %>%
  layer_dropout(0.6) %>%
  layer_lstm(units=200) %>%
  layer_dropout(0.6) %>%
  layer_dense(units=1) %>%
  summary()

model_3 %>%
  compile(loss= 'mse',optimizer = optimizer_adam(0.001),
          metrics = list("mean_absolute_percentage_error")) %>%
  fit(x_bn,y_bn,epoch = 100,batch_size = batch_size_3,verbose=2,validation_split=0.2)
train_predict3 <-model_3%>% predict(x_bn) # 292

# simple lstm 
model_4 <- keras_model_sequential()
model_4 %>%
  layer_lstm(units = 5,input_shape =c(time_step,dim(x)[3])) %>%
  layer_dense(units=5,activation='relu')%>%
  layer_dense(units=1) %>%
  summary()
model_4 %>%
  compile(loss=mean_absolute_percentage_error,optimizer = optimizer_sgd(0.001),
          metrics = list('mean_absolute_percentage_error')) %>%
  fit(x_bn,y_bn,epoch = 100,batch_size = batch_size_3,verbose =2,validation_split=0.2)
train_predict4 <- model_4%>% predict(x_bn)

acf(train_predict2) #lstm产生的序列前后相关性太大
# 数据可视化, 模型对比
plot(y,type='l', main='各模型预测值对比',xlab='time series',ylab = 'log return')
lines(train_predict,lty=2,col ='green')
lines(train_predict2,lty=2,col = 'blue')
lines(train_predict3,lty=2,col = 'yellow')
lines(train_predict4,lty=2,col = 'red')
