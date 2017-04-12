
fit_lars = function(i, first_predictor, last_predictor, ld.train)
{
  m2 = lars(x=as.matrix(ld.train[,first_predictor:last_predictor]), y=ld.train[,i], type=c("lasso"))
  a = summary(m2)
  c = coef(m2, s=which.min(a$Cp), mode="step") 
  return(c)
}

fit_arima = function(i, training_set_sz, predictors_indeces)
{
  print(i)
  m = auto.arima(ld[1:training_set_sz,i], xreg=as.matrix(ld[1:training_set_sz,predictors_indeces]))
  return(m)
}
fit_dl = function(ld.hex,i, first_predictor, last_predictor)
{
  ld.dl = h2o.deeplearning(x=first_predictor:last_predictor, y = i, training_frame = ld.hex)
}

best_dl = function(d.train, d.test,sensor_col, predictors, ensemble_size)
{
  best_err = .Machine$double.xmax
  # models = vector("list", ensemble_size) 
  for(i in 1:ensemble_size)
  {
    # for (i in 1:ensemble_size) {
    if (i %% 10 == 0)
      print(i)
    # rand_activation <- c("Tanh", "Rectifier", "TanhWithDropout", "Maxout")[sample(1:4,1)]
    rand_activation <- c("Tanh", "Rectifier")[sample(1:2,1)]
    rand_numlayers <- sample(1:10,1)
    rand_hidden <- c(sample(1:200,rand_numlayers,T))
    rand_l1 <- runif(1, 1e-6, 1e-4)
    res = tryCatch(
      {
        dlmodel <- h2o.deeplearning(x=predictors, 
                                    y = sensor_col, training_frame = d.train,
                                    validation_frame = d.test,
                                    hidden=rand_hidden, 
                                    activation=rand_activation, 
                                    l1=rand_l1, 
                                    max_w2=10,
                                    epochs = 0.1)
      err <- dlmodel@model$validation_metrics@metrics$mean_residual_deviance
      }, warning = function(war) {
        err = .Machine$double.xmax
        return(err)
      }, error = function(err) {
        err = .Machine$double.xmax
        return(err)
      }
    )
    # models <- c(models, dlmodel)
    # models[[i]] <- dlmodel
    if (err < best_err) {
      best_err <- err
      m <- dlmodel
    }    
  }
  # seelct best model baswd on the meas residual deviance for the test data set
  # best_err = .Machine$double.xmax
  # for (i in 1:length(models)) {
  #   err <- models[[i]]@model$validation_metrics@metrics$mean_residual_deviance
  #   if (err < best_err) {
  #     best_err <- err
  #     best_model <- models[[i]]
  #   }
  # }
  # m = best_model
  return(m)
}
