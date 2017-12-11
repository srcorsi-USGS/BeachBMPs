get.smear <- function(residual.vals,transformation="log10"){
  # Compute smearing coefficient for bias correction in retransformations of
  # regressions with transformed response variable.
  
  if(transformation=="log10"){
    smear.coef <- sum(10^(residual.vals))/length(residual.vals)
  }
  if(transformation=="log"){
    smear.coef <- sum(exp(residual.vals))/length(residual.vals)
  }
  return(smear.coef)
}