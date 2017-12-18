rSqr <- function(observed, resid, p){
  if(length(resid)==0) resid <- predicted-observed
  n <- length(observed)
  yMean <- mean(observed)
  SStot <- sum((observed-yMean)^2)
  
  SSerr <- sum((resid)^2)
 
  rSquare <- 1-(SSerr/SStot)
  
  VARerr <- SSerr/(n-1-p)
  VARtot <- SStot/(n-1)
  adjRsquare <- 1-(VARerr/VARtot)
  
  df <- list(Rsq=rSquare,adjRsq=adjRsquare)
  return(df)
}