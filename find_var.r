library(lubridate)
library(lars)
library(SparseM)
# install.packages('lars')
# install.packages('SparseM')
source('~/Dropbox/utils.R')
setwd('~/Dropbox/papers/dl-traffic/')
source('src/fit.r')
source('src/data_processing.r')
source('src/data_parameters.r')
source('src/data_load.r')

combo = 'm_8_var'
# path = 'data/2013/gcm21_i_tf_20.rds'
# path = 'data/2013/gcm21_i_m_8.rds'
# 'data/2013/gcm21_i_tfs_15.rds' = "https://app.box.com/shared/static/bt69dviydld3o3k87max5od5zhnihbf6.rds"

# need to call it one!
#source('src/data_load.r')

first_predictor = 1 + (sz[2]-1)*horizon+1 #  horizon*5 min forecast 
last_predictor = dim(ld)[2]
pred_names = names(ld)[first_predictor:last_predictor]



### LARS FIT ###
library(lars)
A = matrix(0, ncol = length(first_predictor:last_predictor), nrow = length(2:dim(d)[2]))
for (i in 2:dim(d)[2])
{
  A[i-1,] = fit_lars(i,first_predictor, last_predictor, ld.train)
}
# saveRDS('data/Lasso_A.rds', object = A)
# A = readRDS('data/Lasso_A.rds') 
A = readRDS(gzcon(url('https://app.box.com/shared/static/9r4pc149wz2y6whxmdu67c0jf0pgm8it.rds'))) 
library(SparseM)
par(mar=c(2,2,.5,.5), mai = c(0.6,0.6,0.2,0.2), mfrow=c(1,1))
# image(as.matrix.csr(A), col=heat.colors(40,1), ann=F, cex.axis=1.5)
image(as.matrix.csr(A), col=c("red","white"), ann=F, cex.axis=1.5)
mtext(side = 1, text = "column", line = 2, cex=1.5)
mtext(side = 2, text = "row", line = 2, cex=1.5)
utils$save_pdf('paper/fig/', name = 'lasso_sparsity',lheight = utils$pdf_h/2)
#  percent of sparse entires
sum(A<0.000001)/(dim(A)[1]*dim(A)[2])

### Predictions ###
# n = 4884
n = dim(ld.test)[1]
y_lars = matrix(0,n,dim(d)[2]-1)
for(i in 1:n)
{
  # print(i)
  x = as.double(ld.test[i,first_predictor:last_predictor])
  # y=ld[i,2:dim(d)[2]]
  y_lars[i,] = A%*%x
}

n1 = dim(ld.train)[1]
y_lars1 = matrix(0,n1,dim(d)[2]-1)
for(i in 1:n1)
{
  # print(i)
  x = as.double(ld.train[i,first_predictor:last_predictor])
  # y=ld[i,2:dim(d)[2]]
  y_lars1[i,] = A%*%x
}


saveRDS(y_lars, "fit_data/y_lars.rds")
saveRDS(y_lars1, "fit_data/y_t_lars.rds")

y_lars1 = readRDS("fit_data/y_t_lars.rds")
y_lars = readRDS("fit_data/y_lars.rds")

metrics = function(data, model)
{
  n  = length(data)
  n1 = length(model)
  if (n1!=n)
  {
    print("Dimension mismatch")
    return()
  }
  davg = mean(data)
  sstot = sum( (data -  davg)^2  )
  ssmod = sum( (model - davg)^2  )
  ssres = sum( (model - data)^2  )
  rsq   =  1 - ssres/sstot
  mse  = ssres/n
  return(list(rsq=rsq, mse=mse))
}
m(ld.train[,11], y_lars1[,10])
metrics(ld.test[,11], y_lars[,10])


days = yday(ld.test$dt)
# bears is 283 (10/10/2013, Thursday)
# weather is 345 (12/11/2013, Wednesday)
# for (day in unique(days))
# for (day in c(280, 283, 305, 340, 331, 345))
for (day in c(280, 283, 345))
{
  rows = which(days == day)
  n_rows = length(rows)
  forward_rows = rows[(1+horizon):n_rows]
  x = ld.test[forward_rows, 1]      
  x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
  par(mar = c(0,0,0,0))
  lwd = 3
  plot(x, 2.23694*ld.test[forward_rows, sensor_col], type='l',     lty=1, col=1,lwd=lwd, xlab='time', ylab='speed [mi/h]', main=toString(day), ylim=c(5,80), xaxt='n', yaxt = 'n', ann=FALSE, bty="n") #names(d)[sendaysor_col])
  lines(x,2.23694*y_lars[forward_rows, (sensor_col-1)],   type='l',lty=2, col=2, lwd=lwd) # there is no time colun
  lines(x,2.23694*ld.test[rows[1:(n_rows - horizon)] ,sensor_col],  lty=4, col=4, lwd=lwd)
  abline(55,0, col='green', lwd=3, lty=4)
  abline(v=8, col="orange", lty=3, lwd=4)
  # legend("bottomleft", legend = c('data', 'dl', 'const'), lty=c(1,2,4), col=c(1,2,4), lwd=3)
  utils$save_pdf(path='paper/fig/', name = paste(sensor_col,toString(day),'dl', combo, sep='_'), lwidth = utils$pdf_w, lheight = utils$pdf_h)
  
  # plot residuals over time
  res = 2.23694*ld.test[forward_rows, sensor_col] - 2.23694*y_lars[forward_rows]
  par(mar = c(2,3,0,0))
  plot(x,2.23694*ld.test[forward_rows, sensor_col], type='l', col="black", lwd=lwd, ylim=c(0,67), cex.axis=2.5)
  lines(x,abs(res), type='p', col="red", lwd=lwd, lty=4, yaxt="n", pch=16, cex=1)
  utils$save_pdf(path='paper/fig/', name = paste(sensor_col,toString(day),'dl', combo, "res", sep='_'))
}


# plot residuals
days = yday(ld.train$dt)
for (day in c(84, 203, 184, 24,59, 46))
{
  rows = which(days == day)
  n_rows = length(rows)
  forward_rows = rows[(1+horizon):n_rows]
  x = ld.test[forward_rows, 1]      
  x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
  data  = 2.23694*ld.train[forward_rows, sensor_col]
  fcast = 2.23694*y_lars1[forward_rows, (sensor_col-1)]
  par(mar = c(1,0,4,0), mfrow=c(2,2))
  lwd = 4
  res = data - fcast
  plot(x, res, type='p',     lty=1, col=1,lwd=lwd, xlab='time', ylab='speed [m/s]', main=toString(day), xaxt='n', yaxt = 'n', ann=T, bty="n") #names(d)[sendaysor_col])
  qqnorm(res)
  qqline(res)
  acf(res)
  plot(x, data, type='l',     lty=1, col=1,lwd=lwd, xlab='time', ylab='speed [m/s]', main=toString(day), ylim=c(5,80), xaxt='n', yaxt = 'n', ann=FALSE, bty="n") #names(d)[sendaysor_col])
  lines(x,fcast,   type='l',                  lty=2, col=2, lwd=lwd)
  print (adf.test(res,alternative = "stationary"))
  # there is no time colun
  
  # utils$save_pdf(path='paper/fig/', name = paste(sensor_col,toString(day),'dl', combo, sep='_'), lwidth = utils$pdf_w, lheight = utils$pdf_h)
}




# 
# 
# # for (sensor_col in 2:sz[2])
# # {
#   for (day in unique(days))  
#   {
#     # print(day)
#     rows = which(days == day)
#     n_rows = length(rows)
#     # print(n_rows)
#     # rows = rows[]
#     forward_rows = rows[(1+horizon):n_rows]
#     par(mar = c(3.5,4,2,0))
#     x = ld[forward_rows, 1]      
#     x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
#     par(mar=c(4,4,4,0))
#     plot(x, ld[forward_rows, sensor_col], type='l',      lty=1, col=1, lwd=2, xlab='time', ylab='speed [m/s]', main=toString(day), ylim=c(2,30)) #names(d)[sendaysor_col])
#     lines(x,y_lars[forward_rows,sensor_col-1], type='l', lty=2, col=2, lwd=2) # there is no time colun
#     # lines(x,y_dl[forward_rows,sensor_col-1],   type='l', lty=3, col=3, lwd=2) # there is no time colun
#     lines(x,ld[rows[1:(n_rows - horizon)] ,sensor_col],  lty=3, col=3, lwd=2)
#     legend("bottomleft", legend = c('data', 'var', 'const'), lty=1:3, col=1:3, lwd=3)
#     # save_pdf(path='~/Dropbox/papers/bayes-traffic/fig/forecast/', name = paste(nvec[sensor_col],toString(day), sep='_'))
#   }
# # }
# 
# 
# # playground --------------------------------------------------------------
# 
# #plot train data
# # days = yday(ld.train$dt)
# # for (day in unique(days))  
# # {
# #   rows = which(days == day)
# #   n_rows = length(rows)
# #   par(mar = c(3.5,4,2,0))
# #   x = ld.train[rows, 1]      
# #   x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
# #   par(mar = c(4.5,4,2,0))
# #   plot(x, ld.train[rows, sensor_col], type='l',      lty=1, col=1, lwd=4, xlab='time', ylab='speed [m/s]', main=toString(day), ylim=c(2,40)) #names(d)[sendaysor_col])
# # }
# # #plot test data
# # days = yday(ld.test$dt)
# # for (day in unique(days))  
# # {
# #   rows = which(days == day)
# #   n_rows = length(rows)
# #   par(mar = c(3.5,4,2,0))
# #   x = ld.test[rows, 1]      
# #   x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
# #   par(mar = c(4.5,4,2,0))
# #   plot(x, ld.test[rows, sensor_col], type='l',      lty=1, col=1, lwd=4, xlab='time', ylab='speed [m/s]', main=toString(day), ylim=c(2,40)) #names(d)[sendaysor_col])
# # }
# 
# 
# 
# m = h2o.loadModel(paste(normalizePath('~'),"/Dropbox/papers/bayes-traffic/models/2009/DeepLearning_model_R_1444555786512_8743", sep=''), conn=localH2O)
# 
# 
# pr <- h2o.predict(m, newdata = d.test)
# y_dl = as.vector(as.data.frame(pr)$predict)
# days = yday(ld.test$dt)
# 
# 
# # residual corrected forecast
# res = ld.test[, sensor_col] - y_dl
# y_dl_corr = y_dl
# sz = dim(ld.test)
# library(forecast)
# n.test = length(y_dl)
# y_dl_corr = y_dl
# for (i in 20:(n.test-horizon-1))
# { if (i %% 1000 == 0)
#     print(i)
#   res.ets = holt(res[(i-19):i],  initial = 'optimal', damped = T, h = horizon+1)
#   y_dl_corr[i+horizon+1] = res.ets$mean[horizon+1] + y_dl_corr[i+horizon+1]
# }
# # saveRDS(y_dl_corr, file = '~/Dropbox/papers/bayes-traffic/data/y_dl_corr.rds')
# # y_dl_corr = readRDS('~/Dropbox/papers/bayes-traffic/data/y_dl_corr.rds')
# 100*mean(abs(ld.test[, sensor_col] - y_dl_corr)/ld.test[, sensor_col])
# 100*mean(abs(ld.test[, sensor_col] - y_dl)/ld.test[, sensor_col])
# sum((ld.test[, sensor_col] - y_dl_corr)^2)
# sum((ld.test[, sensor_col] - y_dl)^2)
# 
# # plot(y_dl_corr - ld.test[, sensor_col])
# # qqnorm(y_dl_corr - ld.test[, sensor_col])
# # qqline(y_dl_corr - ld.test[, sensor_col])
# # 
# # res_corr = ld.test[, sensor_col] - y_dl_corr
# # plot(density(ld.test[, sensor_col] - y_dl))
# # plot(density(ld.test[, sensor_col] - y_dl_corr))
# 
# 
# 
# days = yday(ld.test$dt)
# for (day in unique(days))  
# {
#   rows = which(days == day)
#   n_rows = length(rows)
#   forward_rows = rows[(1+horizon):n_rows]
#   par(mar = c(3.5,4,2,0))
#   x = ld.test[forward_rows, 1]      
#   x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
#   par(mar = c(4.5,4,2,0))
#   plot(x, ld.test[forward_rows, sensor_col], type='l',      lty=1, col=1, lwd=4, xlab='time', ylab='speed [m/s]', main=toString(day), ylim=c(2,40)) #names(d)[sendaysor_col])
#   lines(x,y_dl[forward_rows],   type='l', lty=2, col=2, lwd=4) # there is no time colun
#   lines(x,y_dl_corr[forward_rows],   type='l', lty=3, col=3, lwd=4) # there is no time colun
#   lines(x,ld.test[rows[1:(n_rows - horizon)] ,sensor_col],  lty=4, col=4, lwd=4)
#   # lines(x,0.3*ld.test[rows[1:(n_rows - horizon)], sensor_col]+0.7*y_dl[forward_rows],  lty=4, col=4, lwd=2)
#   legend("bottomleft", legend = c('data', 'dl', 'dl+ets', 'const'), lty=1:4, col=1:4, lwd=3)
#   utils$save_pdf(path='paper/fig/forecast/', name = paste(sensor_col,toString(day),'dl', combo, sep='_'))
# }
# 
# 
# 
# # Demo for the paper ------------------------------------------------------
# 
# 
# demo_day = 77
# rows = which(days == demo_day)
# n_rows = length(rows)
# demo = data.frame(dt = ld.test[rows,1], data = ld.test[rows,sensor_col], fc = y_dl_corr[rows])
# demo$min0 =  lubridate::minute(demo$dt) + 60*lubridate::hour(demo$dt)
# demo$hour0 = seq(0,24, length.out = n_rows)
# demo$rows = rows
# plot(demo$hour0, demo$data, type='l')
# lines(demo$hour0, demo$fc)
# 
# plot(density(demo$data - demo$fc))
# 
# 
# vikings_rows = which(lubridate::month(d$dt) == 12 & lubridate::day(d$dt) == 28)
# packers_rows = which(lubridate::month(d$dt) == 12 & lubridate::day(d$dt) == 13)
# ice_rows =    which(lubridate::month(d$dt) == 1 & lubridate::day(d$dt) == 16)
# ice_rows1 =    which(lubridate::month(d$dt) == 1 & lubridate::day(d$dt) == 9)
# 
# yday(d[ice_rows[10],1])
# 
# plot(d[ice_rows, 11])
# plot(d[ice_rows1, 11])
# show(m)
# pr <- h2o.predict(m, newdata = d.train)
# in_sample  = as.vector(as.data.frame(pr)$predict)
# in_sample_res = in_sample - ld.train[,sensor_col]
# 
# library(forecast)
# res.m = auto.arima(in_sample_res)
# 
# train_rows = 600:700
# test_rows = 701:710
# plot(train_rows, in_sample_res[train_rows], xlim = c(min(train_rows),max(test_rows)), ylim = c(-4,3.5))
# fc.ses = ses(in_sample_res[train_rows], h = 10,alpha = 0.2, initial = 'simple')
# fc.holt = holt(in_sample_res[train_rows], h = 10, alpha = 0.5, beta = 0.5, initial = 'simple', damped = T)
# print(in_sample_res[test_rows])
# lines(test_rows,in_sample_res[test_rows], col=2, lwd=5)
# lines(test_rows, fc.ses$mean, col = 3, lwd = 5)
# lines(test_rows, fc.holt$mean, col = 4, lwd = 5)
# 
# 
# 
# 
# 
# res = d.test[forward_rows, sensor_col] - y_dl[forward_rows]
# plot(x,res, ylab='Residual', xlab='Time')
# save_pdf('~/Dropbox/papers/bayes-traffic/fig/', 'dl_error')
# acf(res)
# qqnorm(res)
# qqline(res)
# fit = auto.arima(res)
# plot(residuals(fit))
# qqnorm(residuals(fit))
# test_rec_error <- as.data.frame(h2o.anomaly(d.test, m))



