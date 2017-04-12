fcast_data_plot = function(day, days_v, fcast, data, model_name)
{
  rows = which(days_v == day)
  n_rows = length(rows)
  forward_rows = rows[(1+horizon):n_rows]
  par(mar = c(0,0,0,0))
  x = data[forward_rows, 1]      
  x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
  # par(mar = c(4.5,4,2,0))
  lwd = 4
  plot(x, 2.23694*data[forward_rows, sensor_col], type='l',     lty=1, col=1,lwd=lwd, xlab='time', ylab='speed [m/s]', main=toString(day), ylim=c(5,80), xaxt='n', yaxt = 'n', ann=FALSE, bty="n") #names(d)[sendaysor_col])
  lines(x,2.23694*fcast[forward_rows],   type='l',                  lty=2, col=2, lwd=lwd) # there is no time colun
  lines(x,2.23694*data[rows[1:(n_rows - horizon)] ,sensor_col],  lty=4, col=4, lwd=lwd)
  legend("bottomleft", legend = c('data', model_name, 'const'), lty=c(1,2,4), col=c(1,2,4), lwd=3)
  nme = paste(sensor_col,toString(day),'dl', combo, sep='_')
  abline(55,0, col='green', lwd=3, lty=4)
  abline(v=8, col="orange", lty=3, lwd=4)
}
qqplot.data <- function (vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)+ theme_bw(base_size =  36)
  
}

res_get = function(day, days_v, fcast, data)
{
  rows = which(days == day)
  n_rows = length(rows)
  forward_rows = rows[(1+horizon):n_rows]
  x = data[forward_rows, 1]      
  x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
  data_ =  2.23694*data[forward_rows, sensor_col] 
  fcast_ = 2.23694*fcast[forward_rows]
  res = data_ - fcast_
  return(res)
}

res_plot = function(day, days_v, fcast, data, model_name)
{
  rows = which(days == day)
  n_rows = length(rows)
  forward_rows = rows[(1+horizon):n_rows]
  x = data[forward_rows, 1]      
  x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
  data_ =  2.23694*data[forward_rows, sensor_col] 
  fcast_ = 2.23694*fcast[forward_rows]
  par(mar = c(2,2,4,0), mfrow=c(2,2))
  lwd = 4
  res = data_ - fcast_
  plot(x, res, type='p',lty=1, pch=16, col=1,lwd=lwd, xlab='time', ylab='speed [m/s]', main=toString(day), ann=T, bty="n") #names(d)[sendaysor_col])
  qqnorm(res)
  qqline(res)
  acf(res)
  plot(x, data_, type='p',  pch=16,  cex=0.4, lty=1, col=1,lwd=lwd, xlab='time', ylab='speed [m/s]', main=toString(day), ylim=c(5,80), ann=FALSE, bty="n") #names(d)[sendaysor_col])
  lines(x,fcast_,   type='p',  pch=16,  cex=0.4, col=2, lwd=lwd) # there is no time colun
}
