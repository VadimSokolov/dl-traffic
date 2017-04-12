# install.packages("RSQLite")
# more about RSQLite: http://blog.rstudio.org/2014/10/25/rsqlite-1-0-0/
library(RSQLite)
library(lubridate)
library(plyr)
source('~/Dropbox/utils.R')
setwd('~/Dropbox/papers/dl-traffic/')
source('src/data_processing.r')

year = 2013
smooth = 'm'
# will pretend that everythign is in UTC time zone
start = paste(year,'01','01', sep='/')
end = paste(year,'12','31', sep='/')
all_times = seq(ymd(start), ymd(end),  by = "5 min")
all_speed = data.frame(dt = all_times)

query = get_query(year)

# Main Loop ---------------------------------------------------------------
path = paste('data', year, 'gcm21.rds', sep='/')
if (!file.exists(path))
{
  for (i in 6034:6053)
  {
    db = paste("/Users/vsokolov/data/gcm/neighbour/IL-TESTTSC-STEVENSON-N-", toString(i), ".sqlite", sep="")
    column_name = paste("N", toString(i), sep="")
    all_speed = append_speed_column(db, column_name, all_speed)
  }
  saveRDS(all_speed, path)
}


# interpolate missing data ------------------------------------------------
all_speed = readRDS(path)
path = paste('data', year, 'gcm21_i.rds', sep='/')
if (!file.exists(path))
{
  print("Do interpolation")
  library(zoo)
  sz = dim(all_speed)
  # interpolation in time (by columns)
  # all_speed[,2:sz[2]] = na.approx(all_speed[,2:sz[2]], rule=2)
  
  # interpolation in space (by rows)
  # find first non-empty row
  rs = rowSums(is.na(all_speed))
  for (i in 1:sz[1])
  {
    if (rs[i] < (sz[2]-1) )
    {
      first_non_na_row = i
      break
    }
  }
  # remove all-NA rows
  all_speed = all_speed[-(1:first_non_na_row),]
  # update sz
  sz = dim(all_speed)
  # update rs
  rs = rowSums(is.na(all_speed))
  last_working = 1
  for (i in 2:sz[1])
  {
    if (i %% 1000)
      print(i)
    #   print(last_working)
    if (rs[i]==0)
      next
    if (rs[i] == (sz[2]-1)) # all NAs
      all_speed[i,2:sz[2]] = na.approx(as.double(all_speed[last_working,2:sz[2]]), rule=2)
    else 
    {
      last_working = i
      all_speed[i,2:sz[2]] = na.approx(as.double(all_speed[i,2:sz[2]] ), rule=2)
    }
  }
  saveRDS(all_speed, path)
}


# Smoothing ---------------------------------------------------------------


all_speed = readRDS(path)
# days = lubridate::yday(all_speed$dt)
# d = all_speed[which(days == 282),11]
# plot(d)
# lines(l1tf(d, lambda = 50), col = 1, type='l')
# lines(l1tf(d, lambda = 10), col = 2, type='l')
# lines(l1tf(d, lambda = 1), col = 3, type='l')
install.packages("devtools")
library(devtools)
install_github("hadley/l1tf")
library(l1tf)

# parameters of the smoother
smooth_param = 8
if (smooth_param > 0 )
{
  path = paste('data', year, sprintf("gcm21_i_%s_%d.rds",smooth, smooth_param), sep='/')
} else
{
  path = paste('data', year, sprintf("gcm21_i_%s.rds",smooth), sep='/')
}

if (!file.exists(path))
{
  print("Do Smoothing")
  sz = dim(all_speed)
  days = lubridate::yday(all_speed$dt)
  nvec = names(all_speed)
  for (index in 2:sz[2])
  {
    print(index)
    for (day in unique(days))
    {
      # print(day)
      rows = which(days == day)
      dd = all_speed[rows,c(1,index)]
      if (length(dd[,2]) < window + 1)
      {
        print (length(dd))
        print (c('Skipping day ', toString(day)))
        next
      }
  #     dd$trend = as.double(filter(dd[,2], c(log(10:1))/15.10441, side=1))
  #     dd.ets   = ets(dd[,2], model = 'ZZZ', damped = T)
  #     dd$trend = fitted(dd.ets)
      if (smooth == 'm')
        dd$trend = median_filter(dd[,2], smooth_param, sides=1)
      if (smooth == 'tf')
      {
        dd$trend = l1tf(dd[,2], lambda = smooth_param)
      }
      
      if (smooth == 'tfs')
      {
        lhor = 24
        dd$trend = l1tf(dd[,2], lambda = smooth_param)
        nmeas = length(dd$trend)
        for (i in lhor:nmeas)
        {
          dd$trend[i] = l1tf(dd[(i-lhor+1):i,2], lambda = smooth_param*2)[lhor]
        }
      }
      
      if (smooth == 'l')
      {
            dd$mins = 60*lubridate::hour(dd$dt) + lubridate::minute(dd$dt)
            dd_trend = loess(dd[,nvec[index]] ~ dd[,'mins'], span = 0.2)
            dd$trend =  predict(dd_trend, newdata = dd)
      }
      # dd$trend = mean_filter(dd[,2],6)
      if (runif(1) < 0.05)
      {
        plot(dd[,2], type='l')
        lines(dd$trend, lty = 2, col = 2)
        # utils$save_png('~/Dropbox/papers/bayes-traffic/fig/2013/', paste(nvec[index], toString(day), sep='_'))
      }
      # replace data with the smoothed data
      all_speed[which(days==day),index] = dd$trend
      # plot(dted$trend)
  #     lines(dd$trend)
    }  
  }  
  saveRDS(all_speed, path)
}

# normalize the data
all_speed = readRDS(path)
path_name = unlist(strsplit(path,"\\."))[1]
path = paste(path_name,"_n.rds",sep='')
if (!file.exists(path))
{
  n = dim(all_speed)[[2]]
  for (i in 2:n)
  {
    all_speed[,i] = (all_speed[,i] - mean(all_speed[,i], na.rm=T))/sd(all_speed[,i], na.rm=T)
  }
  saveRDS(all_speed, path)
}

print("Done!")

