source('~/Dropbox/utils.R')
# source('c:/Users/vsokolov/Dropbox/utils.R')
library(ggplot2)
setwd('~/Dropbox/papers/dl-traffic/')
# setwd('C:/Users/vsokolov/Google Drive/papers/dl-traffic/')
source('src/data_processing.r')
source('src/data_parameters.r')
# Functions ---------------------------------------------------------------

plot_by_day = function(data, col)
{
  days = lubridate::yday(data$dt)
  for (day in unique(days))
  {
    rows = which(days == day)
    print(day)
    day_data = data[rows, col]
    plot(day_data, type='l', main=day)
    utils$save_png(path = 'tmp_fig', toString(day))
  }
}

day_data = function(data, day, col)
{
  days = lubridate::yday(data$dt)
  rows = which(days == day)
  day_data = data[rows, col]
}

plot_day = function(data, day, col)
{
  days = lubridate::yday(data$dt)
  rows = which(days == day)
  day_data = data[rows, col]
  x = seq(0,24,length.out = length(day_data))
  plot_speed(x, day_data)
}
plot_speed = function(x, day_data, scale = 1)
{
  qplot(x,day_data,geom='point', ylab = 'Speed [m/s]', xlab='Time [hour]', size=I(scale*2), colour = I("red")) +
  scale_x_continuous(limits=c(min(x),max(x)), expand = c(0, 0)) +
  theme_bw(base_size =  scale*16)
}

print_days = function(data)
{
  print(unique(cbind(lubridate::year(data$dt), lubridate::month(data$dt), lubridate::day(data$dt), lubridate::wday(data$dt), lubridate::yday(data$dt))))
}

plot_model = function()
{
	days = yday(ld.test$dt)
	for (day in unique(days))  
	{
	  rows = which(days == day)
	  n_rows = length(rows)
	  forward_rows = rows[(1+horizon):n_rows]
	  par(mar = c(3.5,4,2,0))
	  x = ld.test[forward_rows, 1]      
	  x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
	  par(mar = c(4.5,4,2,0))
	  plot(x, ld.test[forward_rows, sensor_col], type='l',      lty=1, col=1, lwd=4, xlab='time', ylab='speed [m/s]', main=toString(day), ylim=c(2,40)) #names(d)[sendaysor_col])
	  lines(x,y_dl[forward_rows],   type='l', lty=2, col=2, lwd=4) # there is no time colun
	  lines(x,y_dl_corr[forward_rows],   type='l', lty=3, col=3, lwd=4) # there is no time colun
	  lines(x,ld.test[rows[1:(n_rows - horizon)] ,sensor_col],  lty=4, col=4, lwd=4)
	  legend("bottomleft", legend = c('data', 'dl', 'dl+ets', 'const'), lty=1:4, col=1:4, lwd=3)
	  # save_pdf(path='~/Dropbox/papers/bayes-traffic/fig/forecast/', name = paste(nvec[sensor_col],toString(day),'dl', sep='_'))
	}
}

# Some plots ------------------------------
d = readRDS('data/2013/gcm21_i.rds');
ld = lagged_data( d, max.lags)

s11 = ld[,c(1,11)]
s11$wday = lubridate::wday(s11$dt)
s11$yday = lubridate::yday(s11$dt)
s11$min0 = 60*lubridate::hour(s11$dt) + lubridate::minute(s11$dt)
as = ddply(s11[which(s11$yday < 345 & s11$yday> 200),], "min0", summarise, avg_speed = mean(N6043, na.rm = TRUE))$avg_speed
plot(as)

dim(s11[which(s11$yday < 345 & s11$yday> 200),])
print_days(d)
d = readRDS('data/2013/gcm21_i.rds')
d.trend = readRDS('data/2013/gcm21_i_tf_20.rds')
plot_by_day(d,11)

plot_day(d,295,11)
utils$save_pdf(path = 'paper/fig/',name = 'day_295', lheight = utils$pdf_h/2)

d_day = day_data(d,295,11)
p = plot_day(d.trend,295,11)
show(p)
p + geom_line(aes(y = d_day)) + theme_bw(base_size =  24)
utils$save_pdf(path = 'paper/fig/',name = 'day_295_tf',lheight = utils$pdf_h*0.7)

library(genlasso)
d_day.fused.lasso = trendfilter(d_day, ord = 1)
d_day.cv = cv.trendfilter(d_day.fused.lasso)
lambda_ind = which(d_day.fused.lasso$lambda == d_day.cv$lambda.min)
day_data = d_day.fused.lasso$fit[,lambda_ind]
x = seq(0,24,length.out = length(day_data))
p = plot_speed(x, day_data)
show(p)
p + geom_line(aes(y = d_day)) + theme_bw(base_size =  24)
utils$save_pdf(path = 'paper/fig/',name = 'day_295_fl',lheight = utils$pdf_h*0.7)

get_day = function(s)
{
  unlist(strsplit(s,split = " "))[1]
}
get_time = function(s)
{
  time = unlist(strsplit(s,split = " "))[2]
  time = unlist(strsplit(time,split = ":"))
  time = 60*as.integer(time[1]) + as.integer(time[2])
}

plot(ld$N6040[1:n], ld$N6040_10[1:n], pch=20)
save_pdf('~/Dropbox/slides/pf-traffic/fig/', name = "var_relation")


# lagged data -------------------------------------------------------------

test_days  = readRDS('data/test_days.rds')
train_days = readRDS('data/train_days.rds')
test_rows  = which(days  %in% test_days)
train_rows = which(days  %in% train_days)
ld.test = ld[test_rows,]
ld.train = ld[train_rows,]
library(MASS)
n=50
f = kde2d(ld[9:n,11], ld[1:(n-8),20], n = 50)
persp(f, xlab = 'data', ylab = 'lagged data', zlab = 'density', theta=20, phi = 20, box = T, cex.lab=1.5, col="maroon")
utils$save_pdf('paper/fig/','ccf')


f = kde2d(ld[9:n,11], ld[1:(n-8),11], n = 50)
persp(f, xlab = 'data', ylab = 'lagged data', zlab = 'density', theta=20, phi = 20, box = T, cex.lab=1.5, col="maroon")
utils$save_pdf('paper/fig/','acf')
