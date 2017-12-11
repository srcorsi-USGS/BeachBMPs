# Compute RMSE and smearing coefficient (bias from log transformation) from
# JSON object for beach model and data that was used to develop model

# Requires you to be in this directory:
  setwd("M:/QW Monitoring Team/GLRI beaches/Modeling/Models for 2014 season/")
# Set these specifics:
 fileType="MaxRowsTurb"
 data <- "TH2013T.MaxRowsTurb.csv"
 JSON <- "TH_LCV_140602.csv"
 response="log_beach_EColi"
 
#Set up the function:
getBeachBias <- function(JSON,data,fileType="MaxRowsTurb",response="log_beach_EColi"){
  source("./R/fxn_get.smear.R")
  df <- read.csv(paste("./Datasets 2013/",fileType,"/",data,sep=""),)
  dfJSON <- read.csv(paste("./JSON excel files/",JSON,sep=""),as.is=TRUE)
  
  dfJSON <- dfJSON[,c("colName","Coefficient")]
  dfJSON <- dfJSON[nchar(dfJSON$colName)>0,]
  
  intRow <- which(dfJSON$colName=="intercept")
  intercept <- dfJSON[intRow,"Coefficient"]
  coefficients <- dfJSON$Coefficient
  names(coefficients) <- dfJSON$colName
  intercept <- 1
  df <- cbind(intercept,df)
  dfModel <- as.matrix(df[,names(coefficients)])
  
  predictions <- dfModel %*% coefficients
#   form <- formula(paste(response,"~",paste(dfJSON$colName,collapse="+")))
#   m <- lm(form,data=df)
  residuals <- predictions - df[,response]
  BIAS <- get.smear(residuals,transformation="log10")
  
  RMSE <- sqrt(mean(residuals^2))
  values <- c(BIAS,RMSE)
  names(values) <- c("bias","RMSE")
  return(values)
}
# get result
getBeachBias(JSON,data)
