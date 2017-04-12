# gcm21_i_m_8.rds = https://app.box.com/shared/static/bt69dviydld3o3k87max5od5zhnihbf6.rds
combo = 'm_8_lasso'
d = readRDS(gzcon(url('https://app.box.com/shared/static/bt69dviydld3o3k87max5od5zhnihbf6.rds')));
sz = dim(d)
ld = lagged_data( d, max.lags)
days = yday(ld$dt)
udays = unique(days)
train_days = readRDS(gzcon(url('https://app.box.com/shared/static/rvtrajgihbznvglc3qgq36lmdqkqf2o3.rds')))
test_days =  readRDS(gzcon(url('https://app.box.com/shared/static/88n6wwvlseuz0iwr0gz7752pnmr6mfjg.rds')))
test_rows  = which(days  %in% test_days)
train_rows = which(days  %in% train_days)
ld.test = ld[test_rows,]
ld.train = ld[train_rows,]

# load predicted data
y_dl =   readRDS("fit_data/y_dl_m_8_lasso.rds")
y_dl_t = readRDS("fit_data/y_dl_t_m_8_lasso.rds")

y_lars = readRDS("fit_data/y_lars.rds")
y_lars_t = readRDS("fit_data/y_t_lars.rds")
