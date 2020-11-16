# Develop model and test for differences in water quality for beaches
# 63rd Street beach

library(glmnet)
library(tidyverse)
library(tools)

source("10_load_data/src/get_Racine_data.R")
source("20_process_data/src/fxn_cart2polar_polar2cart.R")
source("20_process_data/src/fxn_cart2polar_polar2cart_df.R")
source("20_process_data/src/process_Racine.R")

dfNB <- read.csv("10_load_data/raw/Racine/North Beach Historical Database 1995-2019 COMBINED EDIT.csv",
                 stringsAsFactors = FALSE, 
                 na.strings = c('NR','#N/A', 'NC', 'No Data','N/A','NA'))

dfNBIVs <- read.csv("10_load_data/raw/Racine/IVs.csv",stringsAsFactors = FALSE)

dfRacine <- get_Racine(df=dfNB,dfIVs=dfNBIVs)

dfModel <- process_Racine(dfRacine)


#model_Racine <- function(df){

#dfModel <- df.orig
#######(((((((########
#reduce to data set for modeling and define pre, during, and post periods
## Reduce to 2006 and after for pre-post analysis
response <- "Ecoli"
df <- dfModel[which(!is.na(dfModel[,response])),] #remove rows without E coli

#preDates <- as.POSIXct(c("2006-01-02","2001-01-02"))
#postDates <- as.POSIXct(c("2011-01-02","2017-01-02"))
#df <- subset(df,pdate>preDates[1])
#df$period <- ifelse(df$pdate < preDates[2],"pre","post")
#df$period <- ifelse(df$pdate >postDates[1] & df$pdate < postDates[2],"during",df$period)
df$Wind.A.Component..BO....338.55ø. <- as.numeric(df$Wind.A.Component..BO....338.55ø.)
df$Wind.O.Component..BO....338.55ø. <- as.numeric(df$Wind.O.Component..BO....338.55ø.)

df <- na.omit(df)
#######)))))))########


## LASSO MODELING  ###


response <- "Ecoli"
beginIV <- which(names(df)=="Ecoli")+1
endIV <- dim(df)[2]-1

bdate <- as.POSIXct("1997-01-01")
edate <- as.POSIXct("2010-12-01")

modelRows <- which(df$pdate >= bdate & df$pdate <= edate)
#model_beach_wq_change <- function(df,response,beginIV,endIV){
  
  IVs <- names(df)[beginIV:endIV]
  matIVs <- as.matrix(df[modelRows,IVs])
  colnames(matIVs) <- IVs
  y<-log10(df[modelRows,response])
  
  g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200)
  g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200,standardize=FALSE)
  
#}

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

DataTestPeriod <- list(as.POSIXct(c("1997-01-01","2000-09-30","2004-12-31","2010-01-01")),
                       as.POSIXct(c("2010-01-01","2013-12-31","2014-01-01","2017-01-01")),
                       as.POSIXct(c("2005-01-01","2007-12-31","2016-12-31","2019-12-31")),
                       as.POSIXct(c("1997-01-01","2000-12-31","2015-12-31","2019-12-31")))
TestResult_list <- list()
wilcox_results <- numeric()
t.test_results <- numeric()
df$response <- log10(df$Ecoli)
r2_values <- numeric()
models <- list()
subModeldf <- list()
for(i in 1:length(DataTestPeriod)){
  testPeriod <- DataTestPeriod[[i]]
  subdf <- subset(df,pdate > testPeriod[1] & pdate < testPeriod[4])
  
    m <- lm(response ~ 1,data = subdf)
  summary (m)
  
  form <- formula(paste("response ~",paste(IVs,collapse = " + ")))
  
  mstep <- step(m,scope = form,k=log(dim(subdf)[1]))
  
  models[[i]] <- mstep
  
  summary(mstep)
  r2_values <- c(r2_values,summary(mstep)$r.squared)
  
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
  t.test_results <- c(t.test_results,t.test(pre,post)$p.value)
  
  TestResult_list[[i]] <- subdf %>%
    group_by(period) %>%
    summarize(medianEC = median(Ecoli),
              median_resid = median(resids),
              meanEC = mean(Ecoli),
              mean_resid = mean(resids),
              n = length(Ecoli))
  TestResult_list[[i]]$testperiod <- paste(testPeriod[1],"-",testPeriod[4])
  subModeldf[[i]] <- subdf
  
}

TestResult <- rbind(TestResult_list[[1]],TestResult_list[[2]]) %>%
  rbind(TestResult_list[[3]]) %>%
  rbind(TestResult_list[[4]])
wilcox_results
t.test_results
r2_values

period1 <- filter(df,pdate < DataTestPeriod[[2]][1])
period2 <- filter(df,pdate > DataTestPeriod[[2]][2],pdate < DataTestPeriod[[2]][3])
period3 <- filter(df,pdate > DataTestPeriod[[2]][3])

mean(period1$Ecoli)
mean(period2$Ecoli)
mean(period3$Ecoli)

df$year <- as.POSIXlt(df$pdate)$year + 1900

boxplot(Ecoli~year,data = df,log = "y", ylab = "E. coli (cfu/100mL)",
        main = "North Beach E. coli concentrations by Year")

############Final models###############

## 1 ##
# 1998-2010 analysis
# examining the model variables, two of them do not make logical sense given the sign of the coefficients. Removing 
# those results in the following model:
i <- 1

testPeriod <- DataTestPeriod[[i]]

summary(models[[1]]) # remove 1-flow and 1-rain variable with negative coefficients
model_vars <- names(coef(models[[i]]))[-c(1,7,8)]

form <- formula(paste("response ~",paste(model_vars,collapse = " + ")))
subdf <- subModeldf[[i]]
m <- lm(response ~ 1,data = subdf)
m4 <- step(m,scope = form)
summary(m4)

plot(subdf$response~predict(m4))
subdf$resids <- residuals(m4)

subdf <- subdf %>%
  mutate(period = case_when(pdate > testPeriod[1] & pdate < testPeriod[2] ~ "Pre",
                            pdate > testPeriod[2] & pdate < testPeriod[3] ~ "Transition",
                            pdate > testPeriod[3] & pdate < testPeriod[4] ~ "Post"))
subdf$period <- factor(subdf$period,levels = c("Pre","Transition","Post"))
subModeldf[[i]] <- subdf

par(mar = c(5,5,3,1))
boxplot(resids~period,data = subdf, ylab = "Residuals (Log E. coli cfu/100 mL)",
        main = paste0("Pre = ",testPeriod[1]," - ",testPeriod[2],"; Post = ",testPeriod[3]," - ", testPeriod[4]))
pre <- subdf[subdf$period == "Pre","resids"]
post <- subdf[subdf$period == "Post","resids"]
wilcox.test(pre,post,paired = FALSE)
t.test(pre,post)

model_1_results <- subdf %>%
  group_by(period) %>%
  summarize(medianEC = median(Ecoli),
            median_resid = median(resids),
            meanEC = mean(Ecoli),
            mean_resid = mean(resids),
            n = length(Ecoli))
model_1_results$testperiod <- paste(testPeriod[1],"-",testPeriod[4])


TestResult <- rbind(model_1_results,TestResult_list[[2]]) %>%
  rbind(TestResult_list[[3]]) %>%
  rbind(TestResult_list[[4]])



#

dfResid <- subModeldf[[1]][,c("resids","period")]
dfResid$Time_period <- "Period 1"
for(i in 2:4) {
 temp <- subModeldf[[i]][,c("resids","period")]
 temp$Time_period <- paste("Period",i)
 dfResid <- rbind(dfResid,temp)
}

library(ggplot2)

ggplot(dfResid,aes(x=period,y=resids)) +
  geom_boxplot() +
  facet_wrap(vars(Time_period),nrow = 2) +
  ggtitle("Residuals for Pre, Transition, and Post Periods for Management Effectiveness Evaluation")

boxplot(resids~period,data = subdf, ylab = "Residuals (Log E. coli cfu/100 mL)",
        main = paste0("Pre = ",testPeriod[1]," - ",testPeriod[2],"; Post = ",testPeriod[3]," - ", testPeriod[4]))
