# Develop model and test for differences in water quality for beaches
# 63rd Street beach

library(glmnet)
library(tidyverse)

#model_Racine <- function(df){

dfModel <- df.orig
#######(((((((########
#reduce to data set for modeling and define pre, during, and post periods
## Reduce to 2006 and after for pre-post analysis
response <- "Ecoli"
df <- dfModel[which(!is.na(dfModel[,response])),] #remove rows without E coli

preDates <- as.POSIXct(c("2006-01-02","2011-01-02"))
postDates <- as.POSIXct(c("2011-01-02","2017-01-02"))
df <- subset(df,pdate>preDates[1])
df$period <- ifelse(df$pdate < preDates[2],"pre","post")
df$period <- ifelse(df$pdate >postDates[1] & df$pdate < postDates[2],"during",df$period)
df$Wind.A.Component..BO....338.55ø. <- as.numeric(df$Wind.A.Component..BO....338.55ø.)
df$Wind.O.Component..BO....338.55ø. <- as.numeric(df$Wind.O.Component..BO....338.55ø.)

df <- na.omit(df)
#######)))))))########


## LASSO MODELING  ###


response <- "Ecoli"
beginIV <- which(names(df)=="Ecoli")+1
endIV <- dim(df)[2]-1

bdate <- as.POSIXct("2006-01-01")
edate <- as.POSIXct("2010-01-01")

modelRows <- which(df$pdate >= bdate & df$pdate <= edate)
model_beach_wq_change <- function(df,response,beginIV,endIV){
  
  IVs <- names(df)[beginIV:endIV]
  matIVs <- as.matrix(df[modelRows,IVs])
  colnames(matIVs) <- IVs
  y<-log10(df[modelRows,response])
  
  g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200)
  g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200,standardize=FALSE)
  
}

c1<-coef(g1, s='lambda.min')
c1.1se <- coef(g1,s='lambda.1se')

beta<-which(abs(c1)>0)[-1]-1
beta.1se <- which(abs(c1.1se)>0)[-1]-1
testvars.orig <- colnames(matIVs)[beta]
testvars.orig.1se <- colnames(matIVs)[beta.1se]

plot(g1)


plot(y,predict(g1,newx=matIVs))
abline(0,1)
abline(h=log10(100),v=log10(235))

# ls()
# save(list=ls(),file="workspace.RData")

## try stepwise
# df$response <- log10(df$Ecoli)
# 
# 
# m <- lm(response ~ 1,data = df)
# summary (m)
# 
# form <- formula(paste("response ~",paste(IVs,collapse = " + ")))
# 
# mstep <- step(m,scope = form,k=log(dim(df)[1]))
# 
# summary(mstep)
# 
# plot(df$response~predict(mstep))
# df$resids <- residuals(mstep)
# 
# transition <- as.POSIXct(c("2011-01-02"))
# df$prepost <- ifelse(df$pdate < transition,"pre","post")
# 
# boxplot(resids~prepost,data = df)
# 

# Test different pre post periods

DataTestPeriod <- list(as.POSIXct(c("2010-01-01","2012-12-31","2014-12-31","2020-01-01")),
                       as.POSIXct(c("2006-01-01","2012-12-31","2014-12-31","2020-01-01")))
TestResult_list <- list()
wilcox_results <- numeric()
for(i in 1:2){
  testPeriod <- DataTestPeriod[[i]]
  subdf <- subset(df,pdate > testPeriod[1] & pdate < testPeriod[4])
  m <- lm(response ~ 1,data = subdf)
  summary (m)
  
  form <- formula(paste("response ~",paste(IVs,collapse = " + ")))
  
  mstep <- step(m,scope = form,k=log(dim(subdf)[1]))
  
  summary(mstep)
  
  plot(subdf$response~predict(mstep))
  subdf$resids <- residuals(mstep)
  
  transition <- as.POSIXct(c("2011-01-02"))
  
  subdf <- subdf %>%
    mutate(period = case_when(pdate > testPeriod[1] & pdate < testPeriod[2] ~ "Pre",
                              pdate > testPeriod[2] & pdate < testPeriod[3] ~ "Transition",
                              pdate > testPeriod[3] & pdate < testPeriod[4] ~ "Post"))
  subdf$period <- factor(subdf$period,levels = c("Pre","Transition","Post"))
  
  par(mar = c(5,5,3,1))
  boxplot(resids~period,data = subdf, ylab = "Residuals (Log E. coli cfu/100 mL)",
          main = paste0("Pre = ",testPeriod[1]," - ",testPeriod[2],"; Post = ",testPeriod[3]," - ", testPeriod[4]))
  pre <- subdf[subdf$period == "Pre","resids"]
  post <- subdf[subdf$period == "Post","resids"]
  wilcox_results <- c(wilcox_results,wilcox.test(pre,post,paired = FALSE)$p.value)
  
  TestResult_list[[i]] <- subdf %>%
    group_by(period) %>%
    summarize(median = median(Ecoli),
              median_resid = median(resids),
              n = length(Ecoli))
  TestResult_list[[i]]$testperiod <- paste(testPeriod[1],"-",testPeriod[4])
  
}

TestResult <- rbind(TestResult_list[[1]],TestResult_list[[2]])
wilcox_results

df$year <- as.POSIXlt(df$pdate)$year + 1900

boxplot(Ecoli~year,data = df,log = "y", ylab = "E. coli (cfu/100mL)",
        main = "North Beach E. coli concentrations by Year")
