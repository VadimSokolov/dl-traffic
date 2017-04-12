library(plyr)
# creates lagged column
append_speed_column = function(db, column_name, all_speed)
{
  print(paste("Adding: ", column_name, sep = ''))
  con = dbConnect(RSQLite::SQLite(), dbname=db)
  d =  dbGetQuery(con, query)
  #convert dt column to POSIX time 
  d$dt = parse_date_time(d$dt,"ymd hms")
  d <- rename(d, c("Speed" = column_name))
  nd = join(all_speed, d, by='dt', type = "left", match='first')
  print(dim(nd))
  return(nd)
}

get_query = function(year)
{

	query = paste("select Speed, datetime(LastUpdateTime - LastUpdateTime%300, 'unixepoch','localtime') as dt",
	              " from sensordata",
	              " where", 
	              " strftime('%Y', LastUpdateTime, 'unixepoch','localtime') = '",
	              toString(year),"'",
	              " order by dt;", sep = "")
	return(query)
}

slide = function(df, vname, new.colamn, n, lag)
{
  df[,new.colamn] = c(rep(NA, lag),df[1:(n-lag),vname])
  return(df)
}

lagged_data = function(d, max.lags)
{
	sz = dim(d)
	nvec = names(d)
	ld = d #lagged data
	for (lag in 1:max.lags)  
	{		for (i in 2:sz[2])
		{
			new.colname = paste(nvec[i], toString(lag), sep = "_")
			ld = slide(ld, nvec[i], new.colname, sz[1], lag)
			# print(dim(ld))
		}
	}
	#remove first max.lags rows with NAs
	ld =ld[(max.lags+1):sz[1],]
	return(ld)
}
# removes rows for certain dys/years
remove_days = function(d, years_keep, days_keep = c(2,3,4,5,6))
{
	days =  lubridate::wday(all_speed$dt)
	years = lubridate::year(all_speed$dt)
	return(d[which(days %in% days_keep & years %in% years_keep)])
}
# load from rds file
load_data = function(data_dir, year, smoothed = 'm')
{
	return(readRDS(c(data_dir, '/gcm_21_', year, '_', smoothed, '.rds')))
}
# save to rds file
save_data = function(data_dir, d, year, smoothed)
{
	saveRDS(c(data_dir, '/gcm_21_', year, '_', smoothed, '.rds'))
} 
# get set of parameters to be used for data preparations
get_data_prep_parameters = function(set_id = 1)
{
	gcm.data.params = list()
	year = 2009
	horizon = 7
	max.lags = 12
	if (set_id == 1)
	{
		gcm.data.params$year = years
		gcm.data.params$horizon = horizon
		gcm.data.params$max.lags = max.lags
	}
	return(gcm.data.params)
}

converth2o = function(d,conn)
{
	min0 = 1440*lubridate::yday(ld$dt) + 60*lubridate::hour(ld$dt) + lubridate::minute(ld$dt)
	temp = rename(ld, c('dt' = 'min0'))
	temp$min0 = min0
	return(as.h2o(temp))
}

median_filter = function(x, window = 3, sides = 1)
{
  n = length(x)
  y = x
  if (sides ==1)
  {
    for (i in window:n)
    {
      
        y[i] =  as.double(quantile(x[(i-window+1):i], probs = 0.5, na.rm = TRUE))
    }
  }
  if (sides == 2)
  {
    for (i in window:(n - window))
    {
      
      y[i] =  as.double(quantile(x[(i-window+1):(i + window - 1)], probs = 0.5, na.rm = TRUE))
    }
  }
  return(y)
}

mean_filter = function(x, window = 3)
{
  n = length(x)
  y = x
  for (i in window:n)
  {
    y[i] =  mean(x[(i-window+1):i])
  }
  return(y)
}

# library(h2o)
# library(plyr)
