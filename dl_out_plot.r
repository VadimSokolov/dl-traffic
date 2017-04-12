library(lubridate)
library(h2o)
source('~/Dropbox/utils.R')
setwd('~/Dropbox/papers/dl-traffic')
source('src/data_parameters.r')
source('src/data_processing.r')


# Functions ---------------------------------------------------------------


# get_days = function(file_name)
# {
#   return(as.integer(strsplit(file_name,"[.]")[[1]][1]))
# }
# test_days =  unlist(lapply(list.files('tmp_fig/test/'), get_days))
# train_days = unlist(lapply(list.files('tmp_fig/train/'), get_days))

load_model = function(name)
{
  m = h2o.loadModel(paste(normalizePath('~'),"/Dropbox/papers/dl-traffic/models/", name, sep=''))
  #m = h2o.loadModel(path = "~/Dropbox/papers/dl-traffic/models/", name = name_)
}


# Prepare Data ------------------------------------------------------------

localH2O = h2o.init(max_mem_size = "2g", nthreads=-1)
# combo = 'i'
# d = readRDS(gzcon(url('https://app.box.com/shared/static/xvbdzd7sg1sfzfy25pe0a936eidgxrqd.rds')));
# m = load_model('DeepLearning_model_R_1450113775656_1980')
combo = 'm_8_lasso'

# d = readRDS(gzcon(url('https://app.box.com/shared/static/bt69dviydld3o3k87max5od5zhnihbf6.rds')));
# m = load_model('DeepLearning_model_R_1449675910414_4090')
m = load_model('DeepLearning_model_R_1449673978399_9545') #with lasso

source('src/data_load.r')
# d = readRDS('data/2013/gcm21_i_tf_20.rds'); 
# m = load_model('DeepLearning_model_R_1449510681559_260')
# m = load_model('DeepLearning_model_R_1449641475140_995') #with lasso

# l1 =0.000966;  
# d = readRDS('data/2013/gcm21_i_tfs_15.rds');
# m = load_model('DeepLearning_model_R_1449675875413_7960')
# m = load_model('DeepLearning_model_R_1449675008782_605') #with lasso
# data/2013/gcm21_i.rds = https://app.box.com/shared/static/xvbdzd7sg1sfzfy25pe0a936eidgxrqd.rds

ld.hex = converth2o(d = ld, conn = localH2O) #  as.h2o(ld)
d.test  = ld.hex[test_rows,] 
d.train  = ld.hex[train_rows,] 


# Choose Model ------------------------------------------------------------


# models = c('DeepLearning_model_R_1450988591754_555',
#            'DeepLearning_model_R_1450988589597_1055',
#            'DeepLearning_model_R_1450988591826_115',
#            'DeepLearning_model_R_1450988591812_850',
#            'DeepLearning_model_R_1450988591795_590',
#            'DeepLearning_model_R_1450988588982_1165',
#            'DeepLearning_model_R_1450988588915_270',
#            'DeepLearning_model_R_1450988591812_765',
#            'DeepLearning_model_R_1450988591832_925',
#            'DeepLearning_model_R_1450988591836_550',
#            'DeepLearning_model_R_1450988591813_1130',
#            'DeepLearning_model_R_1450988589595_85',
#            'DeepLearning_model_R_1450988591823_930',
#            'DeepLearning_model_R_1450988591855_1175',
#            'DeepLearning_model_R_1450988588948_1170',
#            'DeepLearning_model_R_1450989100335_385'
#            )
# 
# # choose best model
# best_err = .Machine$double.xmax
# for (i in 1:length(models)) {
#   m = load_model(models[i])
#   err <- m@model$validation_metrics@metrics$mean_residual_deviance
#   print(m@model_id)
#   print(err)
#   print( m@model$validation_metrics@metrics$r2)
#   print(m@parameters$hidden)
#   print(m@allparameters$activation)
#   if (err < best_err) {
#     best_err <- err
#     best_model <- m
#     # print(m@model_id)
#   }
# }
# m = best_model
# sensor_col = m@parameters$y

# Analyse forecasts -------------------------------------------------------
pr <- h2o.predict(m, newdata = d.test)
y_dl = as.vector(as.data.frame(pr)$predict)
saveRDS(y_dl, "fit_data/y_dl_m_8_lasso.rds")

y_dl = readRDS("fit_data/y_dl_m_8_lasso.rds")

pr <- h2o.predict(m, newdata = d.train)
y_dl_t = as.vector(as.data.frame(pr)$predict)
saveRDS(y_dl_t, "fit_data/y_dl_t_m_8_lasso.rds")

y_dl_t = readRDS("fit_data/y_dl_t_m_8_lasso.rds")


metrics(as.vector(d.test[,sensor_col]), y_dl)
metrics(as.vector(d.train[,sensor_col]), y_dl_t)

# days = yday(ld.test$dt)


# Try smoothing -----------------------------------------------------------


# residual corrected forecast
# res = ld.test[, sensor_col] - y_dl
# y_dl_corr = y_dl
# sz = dim(ld.test)
# n.test = length(y_dl)
# y_dl_corr = y_dl
# # library(forecast)
# ets_horizon = 20
# l1t_horizon = 30
# corr = rep(0, n.test-horizon-1)
# # for (i in ets_horizon:(n.test-horizon-1))


# for (i in l1t_horizon:(n.test-horizon-1))
# {
#     if (i %% 1000 == 0)
#       print(i)
#   res.trend = l1tf(res[(i-l1t_horizon + 1):i],0.01) #estimate trend in residuals
#   # forecast using the trend
#   a = sum(rep(0.25,4)*res.trend[(l1t_horizon-4+1):l1t_horizon])# - res.trend[l1t_horizon-1]
#   corr[i] = a
#   y_dl_corr[i+2+1] = y_dl_corr[i+2+1] + a
# }

# for (i in ets_horizon:(n.test-horizon-1))
# {
#   if (i %% 1000 == 0)
#     print(i)
#   res.ets = holt(res[(i-ets_horizon + 1):i],  initial = 'simple', damped = T, h = horizon+1)
#   # y_dl_corr[i+horizon+1] = res.ets$mean[horizon+1] + y_dl_corr[i+horizon+1]
#   y_dl_corr[i+2+1] = res.ets$mean[2+1] + y_dl_corr[i+2+1]
# }
# mean(abs(ld.test[, sensor_col] - y_dl_corr)/ld.test[, sensor_col])
# mean(abs(ld.test[, sensor_col] - y_dl)/ld.test[, sensor_col])
# sqrt(sum((ld.test[, sensor_col] - y_dl_corr)^2))
# sqrt(sum((ld.test[, sensor_col] - y_dl)^2))
# n = length(ld.test[, sensor_col])
# y_naive = ld.test[1:(n-horizon), sensor_col]
# mean(abs(ld.test[(horizon+1):n, sensor_col] - y_naive)/ld.test[(horizon+1):n, sensor_col])
# 
# plot(y_dl_corr - ld.test[, sensor_col])
# qqnorm(y_dl_corr - ld.test[, sensor_col])
# qqline(y_dl_corr - ld.test[, sensor_col])
# res_corr = ld.test[, sensor_col] - y_dl_corr
# plot(density(ld.test[, sensor_col] - y_dl))
# plot(density(ld.test[, sensor_col] - y_dl_corr))



# Plot --------------------------------------------------------------------


days = yday(ld.test$dt)
# s11 = ld[,c(1,11)]
# s11$wday = lubridate::wday(s11$dt)
# s11$yday = lubridate::yday(s11$dt)
# s11$min0 = 60*lubridate::hour(s11$dt) + lubridate::minute(s11$dt)
# as = ddply(s11[which(s11$yday < 345 & s11$yday> 200),], "min0", summarise, avg_speed = mean(N6043, na.rm = TRUE))$avg_speed
# bears is 283 (10/10/2013, Thursday)
# weather is 345 (12/11/2013, Wednesday)
# for (day in unique(days))
for (day in c(280, 283, 345))
{
  rows = which(days == day)
  n_rows = length(rows)
  forward_rows = rows[(1+horizon):n_rows]
  par(mar = c(0,0,0,0))
  x = ld.test[forward_rows, 1]      
  x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
  # par(mar = c(4.5,4,2,0))
  lwd = 4
  plot(x, 2.23694*ld.test[forward_rows, sensor_col], type='l',     lty=1, col=1,lwd=lwd, xlab='time', ylab='speed [m/s]', main=toString(day), ylim=c(5,80), xaxt='n', yaxt = 'n', ann=FALSE, bty="n") #names(d)[sendaysor_col])
  lines(x,2.23694*y_dl[forward_rows],   type='l',                  lty=2, col=2, lwd=lwd) # there is no time colun
  # lines(x,y_dl_corr[forward_rows],   type='l', lty=3, col=3, lwd=4) # there is no time column
# lines(x,as[(1+horizon):n_rows],   type='l', lty=3, col=3,lwd=lwd) # there is no time colun
  lines(x,2.23694*ld.test[rows[1:(n_rows - horizon)] ,sensor_col],  lty=4, col=4, lwd=lwd)
  # legend("bottomleft", legend = c('data', 'dl', 'dl_corr','const'), lty=c(1,2,3,4), col=c(1,2,3,4), lwd=3)
  # legend("bottomleft", legend = c('data', 'dl', 'const'), lty=c(1,2,4), col=c(1,2,4), lwd=3)
  nme = paste(sensor_col,toString(day),'dl', combo, sep='_')
  abline(55,0, col='green', lwd=3, lty=4)
  abline(v=8, col="orange", lty=3, lwd=4)
  # utils$save_pdf(path='paper/fig/', name = nme)
  
  # plot residuals over time
  res = 2.23694*ld.test[forward_rows, sensor_col] - 2.23694*y_dl[forward_rows]
  par(mar = c(2,3,0,0))
  plot(x,2.23694*ld.test[forward_rows, sensor_col], type='l', col="black", lwd=lwd, ylim=c(0,67), cex.axis=2.5)
  lines(x,abs(res), type='p', col="red", lwd=lwd, lty=4, yaxt="n", pch=16, cex=1)
  utils$save_pdf(path='paper/fig/', name = paste(nme, "res", sep="_"))
}


# plot residuals
days = yday(ld.test$dt)
# for (day in c(84, 203, 184, 24,59, 46))
for (day in c(46))
{
  rows = which(days == day)
  n_rows = length(rows)
  forward_rows = rows[(1+horizon):n_rows]
  x = ld.train[forward_rows, 1]      
  x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
  data =  2.23694*ld.train[forward_rows, sensor_col] 
  fcast = 2.23694*y_dl_t[forward_rows]
  # par(mar = c(4.5,4,2,0))
  par(mar = c(2,2,0,0), mfrow=c(1,1))
  lwd = 4
  res = data - fcast
  plot(x, 100*res/data, type='p',     lty=1, col=1,lwd=lwd, xlab='time', ylab='speed [m/s]', ann=T, bty="n", main="") #names(d)[sendaysor_col])
  qqnorm(res)
  qqline(res)
  acf(res)
  plot(x, data, type='l',     lty=1, col=1,lwd=lwd, xlab='time', ylab='speed [m/s]', main=toString(day), ylim=c(5,80), xaxt='n', yaxt = 'n', ann=FALSE, bty="n") #names(d)[sendaysor_col])
  lines(x,fcast,   type='l',                  lty=2, col=2, lwd=lwd) # there is no time colun
  # utils$save_pdf(path='paper/fig/', name = nme)
 print (adf.test(res,alternative = "stationary"))
}

