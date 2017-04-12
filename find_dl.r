library(lubridate)
library(h2o)
source('~/Dropbox/utils.R')
setwd('~/Dropbox/papers/dl-traffic/')
source('src/fit.r')
source('src/data_processing.r')
source('src/data_parameters.r')


localH2O = h2o.init(max_mem_size = "8g", nthreads=-1)
# path = paste('../../data', year, sprintf("gcm21_i_%s_%d.rds",'m', 6), sep='/')
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2)
{ 
  path = 'data/2013/gcm21_i_m_8_n.rds'
  ensemble_size = 1
  sensor_col = 11
  notes = "default parameters"
} else {
  path = args[1]
  ensemble_size = args[2]
  sensor_col = as.integer(args[3])
  notes = args[4]
}

d = readRDS(path)
sz = dim(d)
ld = lagged_data( d, max.lags)
ld.hex =converth2o(d = ld, conn = localH2O) #  as.h2o(ld)
udays = unique(yday(ld$dt))
days = yday(ld$dt)
train_rows =    which(days %in% train_days)
test_rows =     which(days  %in% test_days)
first_predictor = 1 + (sz[2]-1)*horizon+1 #  horizon*5 min forecast 
last_predictor = dim(ld)[2]
names(ld)[first_predictor:last_predictor]
pred_cols = c(1,first_predictor:last_predictor)
lasso_a = readRDS(gzcon(url('https://app.box.com/shared/static/9r4pc149wz2y6whxmdu67c0jf0pgm8it.rds')))
to_keep = which(lasso_a[sensor_col,] > 0.001) + 1
pred_cols = pred_cols[to_keep]

d.train  =    ld.hex[train_rows,]
ld.train = ld[train_rows,]
d.test  = ld.hex[test_rows,] 
ld.test = ld[test_rows,]


m = best_dl(d.train, d.test,sensor_col, pred_cols, 50)
m = best_dl(d.train, d.test,sensor_col, pred_cols, ensemble_size)
h2o.saveModel(m, path = paste(normalizePath('~'),"/Dropbox/papers/dl-traffic/models/", sep=''), force = T)
line = paste(path, ensemble_size, m@model_id, sensor_col, notes, sep='\t')
write(line,'src/models', append=TRUE)


for (i in 1:10)
{
dlmodel <- h2o.deeplearning(x=pred_cols, 
                            y = sensor_col, 
                            training_frame = d.train,
                            validation_frame = d.test,
                            hidden = c(200,200), 
                            activation="Tanh", 
                            adaptive_rate = F,
                            l1 = 0.000966,
                            nesterov_accelerated_gradient = T,
                            epochs = 0.2
                            )

print(dlmodel)
}




# hyper_params <- list(
#   hidden=list(c(32,32,32),c(64,64)),
#   input_dropout_ratio=c(0,0.05),
#   rate=c(0.01,0.02),
#   rate_annealing=c(1e-8,1e-7,1e-6),
#   activation=c("Tanh", "Rectifier")
# )
# hyper_params
# dl.grid <- h2o.grid(
#   "deeplearning",
#   model_id="dl_grid", 
#   training_frame=d.train_n,
#   x=pred_cols, 
#   y=sensor_col,
#   epochs=0.1,
#   score_validation_samples=10000, ## downsample validation set for faster scoring
#   score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
#   adaptive_rate=F,                ## manually tuned learning rate
#   momentum_start=0.5,             ## manually tuned momentum
#   momentum_stable=0.9, 
#   momentum_ramp=1e7, 
#   l1=1e-5,
#   l2=1e-5,
#   max_w2=10,                      ## can help improve stability for Rectifier
#   hyper_params=hyper_params
# )
# dl.grid
