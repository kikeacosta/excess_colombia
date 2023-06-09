library(tidyverse)
library(readxl)
library(lubridate)
library(mgcv)
library(ISOweek)
library(viridis)

options(scipen=999)

interpop <- function(db)
{
  xs <- db %>% drop_na() %>% pull(t)
  ys <- db %>% drop_na() %>% pull(pop)
  # smoothing using cubic splines
  ts <- db %>% pull(t)
  db2 <- 
    db %>% 
    mutate(pop2 = spline(xs, ys, xout = ts)$y)
  return(db2)
}


# fitting the model
# ~~~~~~~~~~~~~~~~~
est_baseline <- 
  function(db, knots = NA){
    
    if(!is.na(knots)){
      gam_model <- 
        gam(dts ~ t + 
              s(week, bs = 'cp', k = knots) +
              offset(log(exposure)), 
            weights = w,
            data = db, 
            family = "quasipoisson")
    }else{
      gam_model <- 
        gam(dts ~ t + 
              s(week, bs = 'cp') +
              offset(log(exposure)), 
            weights = w,
            data = db, 
            family = "quasipoisson")
    }
    
    resp <- predict(gam_model, newdata = db, type = "response")
    
    db %>% 
      mutate(bsn = resp,
             p_score = dts / bsn) %>% 
      left_join(simul_intvals(gam_model, db, 100),
                by = "date")
  }

est_week_trend <- 
  function(db){
    
    wt_m <- 
      lm(dts ~ t, 
          weights = w,
          data = db)
    
    resp <- predict(wt_m, newdata = db, type = "response")
    
    db %>% 
      mutate(bsn = resp)
  }



est_baseline2 <- 
  function(db){
    
    try(
      model <- 
        gam(dts ~ t + 
              s(week, bs = 'cp') +
              offset(log(exposure)), 
            weights = w,
            data = db, 
            family = "quasipoisson")
    )
    
    if(exists("model")){
      test <- 
        try(
          resp <- predict(model, newdata = db, type = "response")
        )
    }
    
    if(exists("test") & class(test) != "try-error" & 
       model$outer.info$conv == "full convergence" & 
       exists("model") & 
       exists("resp")){
      
      db %>% 
        mutate(bsn = resp) %>% 
        left_join(simul_intvals(model, db, 100),
                  by = "date")
      
    }else{
      db %>% 
        mutate(bsn = NA,
               ll = NA,
               ul = NA)
    }
  }

# bootstrapping using Jonas' method 
simul_intvals <- function(model, db, nsim){
  # matrix model
  X_prd <- predict(model, newdata = db, type = 'lpmatrix')
  # estimated coefficients
  beta <- coef(model)
  # offsets
  offset_prd <- matrix(log(db$exposure))
  
  # applying Huber-White adjustment for robust estimators 
  # beta_sim <- MASS::mvrnorm(nsim, beta, sandwich::vcovHAC(model))
  beta_sim <- MASS::mvrnorm(nsim, coef(model), vcov(model))
  Ey_sim <- apply(beta_sim, 1, FUN = function (b) exp(X_prd%*%b + offset_prd))
  
  y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
    y <- mu <- Ey
    # NA's can't be passed to the simulation functions, so keep them out
    idx_na <- is.na(mu) 
    mu_ <- mu[!idx_na] 
    N <- length(mu_)
    phi <- summary(model)$dispersion
    # in case of under-dispersion, sample from Poisson
    if (phi < 1) { phi = 1 }
    y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
    return(y)
  })
  
  ints_simul <- 
    db %>% 
    select(date)
  
  colnames_y_sim <- paste0('deaths_sim', 1:nsim)
  
  ints_simul[,colnames_y_sim] <- y_sim
  
  ints_simul <-
    ints_simul %>%
    pivot_longer(cols = starts_with('deaths_sim'),
                 names_to = 'sim_id', values_to = 'deaths_sim') %>%
    group_by(date) %>%
    summarise(
      ll = quantile(deaths_sim, 0.05, na.rm = TRUE),
      ul = quantile(deaths_sim, 0.95, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(ints_simul)
}
