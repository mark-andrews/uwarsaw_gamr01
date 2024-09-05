AIC_C <- function(model){
  K <- length(coef(model)) + 1
  N <- nrow(model$model)
  AIC(model) + (2*K*(K+1))/(N-K-1)
}

loo_lm_poly <- function(degree, data_df){
  
  f <- 'mean_fix ~ poly(Time, degree = %d)'
  n <- nrow(data_df)
  
  loocv <- function(i){
    
    train_df <- slice(data_df, -i)
    test_df <- slice(data_df, i)
    
    M <- lm(formula(sprintf(f, degree)), data = train_df)
    predictions <- add_predictions(data = test_df, model = M, var = 'y_hat')
    
    mutate(predictions,
           lpd = dnorm(mean_fix, mean = y_hat, sd = sigma(M), log = T)
    ) %>% pull(lpd) %>% unlist()
    
  }
  
  # elp
  pointwise_elpd <- map_dbl(seq(n), loocv)
  c(elpd = -2*sum(pointwise_elpd), se = -2 * sqrt(n * var(pointwise_elpd)))
}
