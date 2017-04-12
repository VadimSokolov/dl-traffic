setwd('~/Dropbox/papers/dl-traffic/')
library(lubridate)
source('src/data_parameters.r')
source('src/data_processing.r')
source('src/data_load.r')
source('src/plot_functions.r')
source('~/Dropbox/utils.R')


# Scatter plots
s = sample(dim(d)[1], 2000, replace = F)
qplot(d$N6038[s], d$N6039[s], size=I(.5)) + xlab("Speed at N6038 [m/s]") + ylab("Speed at N6039 [m/s]") + theme_grey(base_size =  24)
utils$save_pdf("fig", "scatter_1")
qplot(d$N6035[s], d$N6042[s], size=I(.5)) + xlab("Speed at N6038 [m/s]") + ylab("Speed at N6039 [m/s]") + theme_grey(base_size =  24)
utils$save_pdf("fig", "scatter_2")



# bears is 283 (10/10/2013, Thursday)
# weather is 345 (12/11/2013, Wednesday)
data_name =  "train" # {train, test}
model_name = "dl" # {dl, lars}

if (data_name == "train") {
	data = ld.train
	if(model_name=="dl") fcast = y_dl_t else fcast = y_lars_t[,sensor_col]
	# analysis_days = c(84, 203, 184, 24,59, 46)
	# analysis_days = sample(unique(days), size = 50, replace = F)
	analysis_days = c(210)
} else	{
	data = ld.test
	if(model_name=="dl") fcast = y_dl else fcast = y_lars[,sensor_col]
	analysis_days = c(280, 283, 345)
}
days = lubridate::yday(data$dt)


for (day in analysis_days)
{
	#fcast_data_plot(day, days, fcast, data, model_name)
	 res_plot(day, days, fcast, data, model_name)
}

# par(resetPar())
# analyse day = 120
# rows = which(days == 210)
# n_rows = length(rows)
# forward_rows = rows[(1+horizon):n_rows]
# x = data[forward_rows, 1]
# x = (60*lubridate::hour(x) + lubridate::minute(x))/60.0
# day_210_dl =  2.23694*data[forward_rows, sensor_col]
# fcast_210_dl = 2.23694*fcast[forward_rows]
# day_210_lars =  2.23694*data[forward_rows, sensor_col]
# fcast_210_lars = 2.23694*fcast[forward_rows]


res_lars = res_get(210, days, fcast, data)


res_dl = res_get(210, days, fcast, data)
shapiro.test(res_dl)
shapiro.test(res_lars)
library(nortest)
ad.test(res_dl)
ad.test(res_lars)

m = mean(res_dl)
s = sd(res_dl)
ks.test(res_dl, "pnorm", mean = m, sd = s)

m = mean(res_lars)
s = sd(res_lars)
ks.test(res_lars, "pnorm", mean = m, sd = s)

qqplot.data(res_dl)
qqplot.data(res_lars)

library(ggplot2)
x = seq(0,24,length.out = 281)
qplot(x,res_dl)
qplot(x,res_lars)

qpacf = function(d)
{
  bacf <- acf(d, plot = FALSE)[1:20]
  bacfdf <- with(bacf, data.frame(lag, acf))
  ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) + xlab("ACF")  + theme_bw(base_size =  36)
  
}
qplot(res_lars) + xlab("Residual") + theme_bw(base_size =  36)
utils$save_pdf(path = 'paper/fig/', name = "res_lars")
qpacf(res_lars)
utils$save_pdf(path = 'paper/fig/', name = "res_lars_pacf")
qplot(x, res_lars) + theme_bw(base_size =  36) + xlab("Time [h]") + ylab("Residual [mi/h]")
utils$save_pdf(path = 'paper/fig/', name = "res_lars_1")
mean(res_lars)

qplot(res_dl) + xlab("Residual") + theme_bw(base_size =  36)
utils$save_pdf(path = 'paper/fig/', name = "res_dl")
qpacf(res_dl)
utils$save_pdf(path = 'paper/fig/', name = "res_dl_pacf")
qplot(x, res_dl) + theme_bw(base_size =  36) + xlab("Time [h]") + ylab("Residual [mi/h]")
utils$save_pdf(path = 'paper/fig/', name = "res_dl_1")
Box.test(res_dl, lag = 10, fitdf = 0)
library(tseries)
adf.test(res_dl, k=12 )
mean(res_dl)
library(fNonlinear)

n= length(res_lars)
bgtest(res_lars[1:(n-1)]~res_lars[2:n], type="F", order = 3)
Box.test(res_lars, lag = 10, fitdf = 0)
bptest(res_lars[1:(n-3)]~res_lars[2:(n-2)] + res_lars[3:(n-1)])
wnnTest(res_lars)
bdsTest(res_lars)
runsTest(res_lars)

adf.test(res_lars, k=12 )


n= length(res_dl)
bgtest(res_dl[1:(n-1)]~res_dl[2:n],  type="F", order = 5)
Box.test(res_dl, lag = 10, fitdf = 0)
bptest(res_dl[1:(n-3)]~res_dl[2:(n-2)] + res_dl[3:(n-1)])
wnnTest(res_dl)
bdsTest(res_dl)
runsTest(res_dl)
adf.test(res_dl, k=12 )


par(mfrow=c(1,1))
# for (day in c(283, 345))
for (day in c(283))
{
  rows = which(days == day)
  n_rows = length(rows)
  forward_rows = rows[(1+horizon):n_rows]
  data_day   =  data[forward_rows,]
  fcast_day = fcast[forward_rows]
  
  fcast_dl = fcast[forward_rows]
  plot(data_day[,sensor_col], type='p', main=toString(day))
  lines(fcast_day)
  # utils$save_pdf('paper/fig/', name = "heat_dl_bears")
  speed_heat(day, data_day)
  utils$save_pdf('paper/fig/', name = "heat_data_bears")
  data_day[,sensor_col] = fcast_dl
  speed_heat(day, data_day)
  utils$save_pdf('paper/fig/', name = "heat_fcast_dl_bears")
  
}



# lars findings. Any relations to trffic model?
betas = A[sensor_col,]
m = matrix(betas, nrow=20, ncol = 6)
print(m)
print(rowSums(m))
plot(rowSums(m))
names(betas) = names(ld.train)[first_predictor:last_predictor]
mileposts = cumsum(c(0, 566, 883, 1364, 1168, 875, 1210, 1153, 1186, 1173, 1203, 1242, 1086, 1122, 745, 1190, 913, 1193, 1404, 988))/1609.34

graphics::image(y = seq(7,12), x = mileposts, z = m , ylab = "Time [h]", xlab = "Mile post", yaxt="n", col = heat.colors(22,1), cex.lab=2, cex.axis=2)

par(mar = c(6,6,1,2))
library(fields)
image.plot(y = seq(7*5,12*5, by = 5), x = 1:20, z = m , add=F, ylab = "Time Lag [min]", xlab = "Mile post", col = topo.colors(50))
utils$save_pdf('./paper/fig/', 'betas', lheight = utils$pdf_h/2)


library(vars)
varm = VAR(d[-1,-1], p=6, type="both")
irfd = irf(varm, response = "N6043", boot = F, n.ahead = 12, runs = 4)
# irfd = irf(varm, response = "N6053", boot = F, n.ahead = 10, runs = 4)
# irfd = irf(varm, response = "N6034", boot = F, n.ahead = 10, runs = 4)
par(mar=c(4,4,1,1))

first_sensor = 1
last_sensor = 10
s = first_sensor
plot(seq(35,60,5),irfd$irf[s][[1]][7:12], type="o", xlab = "Lag [min]", ylab = "Response", col=s, lty=s, ylim=c(-0.05,.76), lwd=2, pch=s)
nm = c(names(irfd$irf[s])[1])
for (s in (first_sensor+1):last_sensor)
{
  d = irfd$irf[s]
  nm = c(nm,names(d)[1])
  lines(seq(35,60,5),d[[1]][7:12], type="o", col=s, lty=s, lwd=2, pch=s)
}
legend(45,0.8,legend = nm, col=first_sensor:last_sensor,lty=first_sensor:last_sensor, bty="n", pch=first_sensor:last_sensor)

View(irfd$irf)
(as.double(unlist(irfd$irf)))
irfm = matrix(unlist(irfd$irf), nrow = 20, byrow = T)

rownames(irfm) = names(d[,-1])
mileposts = cumsum(c(0, 566, 883, 1364, 1168, 875, 1210, 1153, 1186, 1173, 1203, 1242, 1086, 1122, 745, 1190, 913, 1193, 1404, 988))/1609.34
library(fields)
image.plot(y = seq(7*5,12*5, by = 5), x = 1:20, z = irfm[,7:12] , add=F, ylab = "Time Lag [min]", xlab = "Mile post", col = topo.colors(50))




first_sensor = 2
last_sensor = 10
yd = yday(d$dt)
which_day = 31
s = first_sensor
plot(d[yd==which_day,first_sensor], type='o', ylim=c(5,30), lty=first_sensor, col=first_sensor, pch=first_sensor, cex=0.3)
nm = c(names(d)[s])
for (s in (first_sensor+1):last_sensor)
{
  plot(d[yd==which_day,first_sensor], type='o', ylim=c(5,30), lty=first_sensor, col=first_sensor, pch=first_sensor, cex=0.3)
  lines(d[yd==which_day,s], type='o', lty=s, col=s, pch=s, cex=0.3)
  nm = c(nm,names(d)[s])
}

legend(0,20,legend = nm, col=first_sensor:last_sensor,lty=first_sensor:last_sensor, bty="n", pch=first_sensor:last_sensor)


a43 = matrix(A[10,], ncol = 6, byrow = F)

rownames(a43) = names(d[,-1])
colnames(a43) = seq(35,60,5)
