max.lags = 12
horizon = 7
train_days = readRDS(gzcon(url('https://app.box.com/shared/static/rvtrajgihbznvglc3qgq36lmdqkqf2o3.rds')))
test_days =  readRDS(gzcon(url('https://app.box.com/shared/static/88n6wwvlseuz0iwr0gz7752pnmr6mfjg.rds')))
sensor_col = 11
