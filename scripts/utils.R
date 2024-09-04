AIC_C <- function(model){
  K <- length(coef(model)) + 1
  N <- nrow(model$model)
  AIC(model) + (2*K*(K+1))/(N-K-1)
}
