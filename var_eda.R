library(ggplot2)
library(lubridate)
# source(url("https://www.dropbox.com/s/leckggnen53sd7a/utils.R?dl=1"))
source('~/Dropbox/utils.R')
setwd('~/Google Drive/papers/dl-traffic/')
fig_path = 'paper/fig/'
all_speed = readRDS('data/2009/gcm21_i.rds')
rs = rowSums(is.na(all_speed))
barplot(table(rs)/length(rs), col = 6, ylab="Percent of Malfunction Sensors", xlab="Number of Malfunction Sensors")
# save_pdf(fig_path, name = 'malfunction_sensor')

#all_speed = readRDS('/Users/vsokolov/Google Drive/papers/bayes-traffic/all_wed_speed_interpolted.rds')

#morning_speed = all_speed[which(hour(all_speed$dt)>4 & hour(all_speed$dt)<13),]

# find strange days:
all_speed$daymin = lubridate::minute(all_speed$dt) + 60*lubridate::hour(all_speed$dt)
all_speed$wday = lubridate::wday(all_speed$dt)
avg_speed = ddply(all_speed, "daymin", summarise, avg_speed = mean(N6043, na.rm = FALSE))
avg__wd_speed = ddply(all_speed, .(daymin, wday), summarise, avg_speed = mean(N6043, na.rm = FALSE))
plot(avg_speed)

plot(avg__wd_speed$avg_speed[avg__wd_speed$wday==3])

plot_by_day = function(data, col)
{
  days = lubridate::yday(data$dt)
  udays = unique(days)
  n = length(udays)
  for (i in 1:(n-5))
  {
    rows      = which(days == udays[i])
    rows_next = which(days == udays[i+5])
    ts      = lubridate::round_date(data[rows[1],      1], unit = 'day')
    ts_next = lubridate::round_date(data[rows_next[1], 1], unit = 'day')
    
    plot(data[rows, col], type='l', main = c(ts, ts_next))
    lines(data[rows_next, col], type='l')
  }
}


# choose data set to work with
d = all_speed
day_step = 24*12
# d = morning_speed
# day_step = (13-4-1)*12-1
sz = dim(d)




# lag = 24
# # get speed data for period 5am-noon (8 hours)

# n = dim(morning_speed)[1]
# for (i in 2:21)
# {
#   plot(all_speed[lag:n,i],  morning_speed[1:(n-lag+1),i], main = toString(i), xlab="Current", ylab="Previous")
# }
# 


speed_heat = function(y_day, data)
{
  s = data[which(lubridate::yday(data$dt) == y_day),2:sz[2]]
  ny = dim(s)[1]
  data.m = data.matrix(s)
  # Take cumsum of distances between the sensor, which I measured in QGIS
  mileposts = cumsum(c(0, 566, 883, 1364, 1168, 875, 1210, 1153, 1186, 1173, 1203, 1242, 1086, 1122, 745, 1190, 913, 1193, 1404, 988))/1609.34
  par(mar = c(5,4,1,1), mai = c(1,1,0.2,0.2))
  image(y = seq(1,24,length.out = ny), x = mileposts, z = t(data.m) , ylab = "Time [h]", xlab = "Mile post", yaxt="n", col = heat.colors(22,1), cex.lab=2, cex.axis=2)
  axis(side = 2,at = seq(0,23,by=2), labels = seq(0,23,by=2),cex.axis=2)
}

speed_heat(7,d)
speed_heat(7,d1)
speed_heat(21,d)
speed_heat(28,d)
speed_heat(35,d)
speed_heat(42,d)
speed_heat(49,d)
utils$save_pdf(fig_path,'heat_plot_interpolated',lheight =  utils$pdf_h*0.7)
speed_heat(49,d1)
save_pdf(fig_path, 'heat_plot_interpolated')
speed_heat(56,d)
speed_heat(63,d)
speed_heat(70,d)

# possible solution  to deal with missing data
# temp=na.omit(morning_speed[,20])


# create new data frame with lagged variables


slide = function(df, vname, new.colame, n, lag)
{
  # print(c(lag, n,vname, new.colame))
  df[,new.colname] = c(rep(NA, lag),df[1:(n-lag),vname])
  return(df)
}

n = dim(morning_speed)[1]
m = dim(morning_speed)[2]

# interpolate missing data
library(zoo)
for (i in 2:m)
{
  morning_speed[,i] = na.approx(morning_speed[,i], rule=2)
}
max.lags = 12
nvec = names(all_speed)
m = length(nvec)

lagged_morning_speed = morning_speed
for (i in 2:m)
{
  for (lag in 1:max.lags)  
  {
    new.colname = paste(nvec[i], toString(lag), sep = "_")
    lagged_morning_speed = slide(lagged_morning_speed, nvec[i], new.colname, n, lag)
  }
}

n = dim(d)[2]
nvec = names(d)
for (i in 2:(n-1))
{
  for (j in (i+1):n)
  {
    if (j==i)
      next
    ccf(d[,i], d[,j], lag.max = 20, main = paste(toString(i), toString(j)))
    save_png('~/Dropbox/papers/bayes-traffic/fig/ccf/', name = paste(nvec[i], nvec[j],sep = '_'))
  }
}

m = dim(d)[1]
x = seq(from=0, to=24, length.out = day_step)
for (i in seq(from=1,to = m, by = day_step))
{
  par(mfrow=c(3,1), mar=c(2,2,0,0))
  plot(x, d[i:(i+day_step-1),2], type='l')
  plot(x, d[i:(i+day_step-1),20], type='l')
  plot(x, d[i:(i+day_step-1),21], type='l')
}

sm = loess(x~d[i:(i+day_step-1),2], span = 0.8)
pl = predict(sm, newdata = d[i:(i+day_step-1),2])
plot(x,pl, type='l', ylim=c(5,35))
lines(x,d[i:(i+day_step-1),2], type='l', lty=2, col=2)


qplot(x,d[i:(i+day_step-1),2]) + geom_smooth(method = "loess",  size = 1 ) 



#Smoothing
print(unique(days))
n_days = length(unique(days))
nvec = names(d)
index = 10
for (day in unique(days))
{
  all_days = yday(all_speed$dt)
  # index = 2
  dd = all_speed[which(all_days==day),c(1,index)]
  dd$mins = 60*hour(dd$dt) + minute(dd$dt)
  dd_trend = loess(dd[,nvec[index]] ~ dd[,'mins'], span = 10)
  dd$trend =  predict(dd_trend, newdata = dd)
  plot(dd[,nvec[index]])
  lines(dd$trend)
}  


# mock-up plots for presentation ------------------------------------------


true_data = c(seq(2,30,by=2), rep(30,4),28, 26, 24, 22) +rnorm(23)
n = length(true_data)
plot(true_data, type='l', xlim=c(0,n+10), xlab="time", ylab='speed', lwd=4, col=6)
last = true_data[n]
lines(n:(n+5),rep(last,6), col=2, lwd=4)
lines(n:(n+5),seq(from=last, to=last-3,length.out = 6), col=3, lwd=4)
lines(n:(n+5),seq(from=last, to=last-6 ,length.out = 6)+ c(0,rnorm(5)), col=6, lwd=4)
abline(v=n, lty=2)
abline(v=n+5, lty=2)
text(21,10,labels = "now")
legend('topleft', legend = c("Constant","Model","Data"), lwd = c(4,4,4),col=c(2,3,6))
save_pdf('~/Dropbox/slides/pf-traffic/fig/', name = "forecast_compare")

plot(true_data, type='l', xlim=c(0,n+10), xlab="time", ylab='speed', lwd=4, col=6)
lines(n:(n+5),seq(from=last, to=last-3,length.out = 6), col=3, lwd=4)
abline(v=n, lty=2)
abline(v=n+5, lty=2)
text(21,10,labels = "now")
save_pdf('~/Dropbox/slides/pf-traffic/fig/', name = "knn_1")
plot(true_data, type='l', xlim=c(0,n+10), xlab="time", ylab='speed', lwd=4, col=6)
lines(n:(n+5),seq(from=last, to=last-4,length.out = 6), col=3, lwd=4)
abline(v=n, lty=2)
abline(v=n+5, lty=2)
text(21,10,labels = "now")
save_pdf('~/Dropbox/slides/pf-traffic/fig/', name = "knn_2")
plot(true_data, type='l', xlim=c(0,n+10), xlab="time", ylab='speed', lwd=4, col=6)
lines(n:(n+5),seq(from=last, to=last-2,length.out = 6), col=3, lwd=4)
abline(v=n, lty=2)
abline(v=n+5, lty=2)
text(21,10,labels = "now")
save_pdf('~/Dropbox/slides/pf-traffic/fig/', name = "knn_3")

x = seq(5,30, length.out = 300)
y = x + rnorm(300, sd=4)
plot(x,y, pch=20, xlab=expression("y"[t]), ylab=expression("y"[t+1]) )
abline(1,1, lwd=3, col=2)
save_pdf('~/Dropbox/slides/pf-traffic/fig/', name = "auto_regression")


library(lubridate)
library(forecast)
days = yday(all_speed$dt)
day_rows = which(days == 21)
one_day = all_speed[day_rows,]
one_sensor = one_day[,9]
par(mar = c(5.1, 5, 4.1, 2.1))
plot(seq(0, 12.5, length.out = 150),one_sensor[1:150], ylab='Speed [m/s]', xlab = 'Time [hour]', pch=20, cex.lab=2.5, cex.axis=2.5)
fit = ets(y = one_sensor, model='ANN', alpha = 0.1)
lines(seq(0, 12.5, length.out = 150),fitted.values(fit)[1:150], col=3, lwd=7)
utils$save_pdf(fig_path, name = "ets", lheight = utils$pdf_h*0.8)

library(FBN)
plot(seq(0, 12.5, length.out = 150),one_sensor[1:150], ylab='Speed [m/s]', xlab = 'Time [hour]', pch=20, cex.lab=2.5, cex.axis=2.5)
lines(seq(0, 12.5, length.out = 150),medianFilter(inputData = one_sensor, windowSize = 10)[1:150], col=3, lwd=7)
utils$save_pdf(fig_path, name = "median_filter", lheight = utils$pdf_h*0.8)

mins = 60*hour(one_day$dt) + minute(one_day$dt)
d_trend = loess(one_sensor ~ mins, span = 0.1)
plot(seq(0, 12.5, length.out = 150),one_sensor[1:150], ylab='Speed [m/s]', xlab = 'Time [hour]', pch=20, cex.lab=2.5, cex.axis=2.5)
lines(seq(0, 12.5, length.out = 150),predict(d_trend, newdata = mins)[1:150], col=3, lwd=7)
utils$save_pdf(fig_path, name = "trend_filter", lheight = utils$pdf_h*0.8)


plot(ld$N6040[1:n], ld$N6040_10[1:n], pch=20)
save_pdf('~/Dropbox/slides/pf-traffic/fig/', name = "var_relation")


