# Develop model and test for differences in water quality for beaches
# Jeorse Street beach

library(glmnet)
library(tidyverse)
library(tools)

# Jeorse Data files from workflow_Jeorse.R

# Two sampling points at Jeorse will be modeled separately

JP2 <- JP2

JP2 <- JP2 %>%
  rename(Ecoli = EC)

# Read Calumet and East Hammond data
source(file.path("10_load_data","src","get_Calumet_data.R"))
source(file.path("10_load_data","src","get_EastHammond_data.R"))
names(cal)
cal <- cal %>%
  rename(Date = Date, Calumet = RESULT_VALUE) %>%
  select(Date, Calumet) %>%
  group_by(Date) %>%
  summarize(Calumet = mean(Calumet))

eh <- eh %>%
  rename(Date = Date, EastHammond = RESULT_VALUE) %>%
  select(Date, EastHammond) %>%
  group_by(Date) %>%
  summarize(EastHammond = mean(EastHammond))

# Combine the two beach data sets
dfModel <- left_join(JP2,cal) %>%
  left_join(eh)



#######(((((((########
#reduce to data set for modeling and define pre, during, and post periods
## Reduce to 2006 and after for pre-post analysis
response <- "Ecoli"
df <- dfModel[which(!is.na(dfModel[,response])),] #remove rows without E coli

DataTestPeriod <- list(as.POSIXct(c("2012-05-01","2014-10-01","2016-05-01","2017-10-01")))
#                       as.POSIXct(c("2008-01-02","2010-01-02","2014-01-02","2016-01-02")))

df <- subset(df,pdate < as.POSIXct("2013-01-01") | pdate > as.POSIXct("2014-01-01"))
df <- subset(df,pdate > DataTestPeriod[[1]][1] & pdate < DataTestPeriod[[1]][4])
df$Ecoli <- ifelse(df$Ecoli > 4000,4000,df$Ecoli)


df <- na.omit(df)
saveRDS(df,file = file.path("40_modeling","out","Jeorse2_model_df.rds"))
write.csv(df,file = file.path("40_modeling","out","Jeorse2_model_df.csv"),row.names = FALSE)

#######)))))))########


## LASSO MODELING  ###


response <- "Ecoli"
beginIV <- which(names(df)=="Ecoli")+1
endIV <- dim(df)[2]


IVs <- names(df)[beginIV:endIV]
IVs <- IVs[-which(IVs %in% c("period","year"))]
# IVs <- c("Calumet","EastHammond") # for paired beach without environmental variables
matIVs <- as.matrix(df[,IVs])
colnames(matIVs) <- IVs
y<-log10(df[,response])

g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200)
#g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200,standardize=FALSE)

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


TestResult_list <- list()
wilcox_results <- numeric()
t.test_results <- numeric()
df$response <- log10(df$Ecoli)
r2_values <- numeric()
models <- list()
subModeldf <- list()

filenm <- "Jeorse2_residual_boxplots.pdf"
beach <- "Jeorse2"

pdf(file = filenm)
for(i in 1:length(DataTestPeriod)){
  testPeriod <- DataTestPeriod[[i]]
  subdf <- subset(df,pdate > testPeriod[1] & pdate < testPeriod[4])
  subdf <- subdf %>%
    mutate(period = case_when(pdate > testPeriod[1] & pdate < testPeriod[2] ~ "Pre",
                              pdate > testPeriod[2] & pdate < testPeriod[3] ~ "Transition",
                              pdate > testPeriod[3] & pdate < testPeriod[4] ~ "Post"))
  subdf$period <- factor(subdf$period,levels = c("Pre","Transition","Post"))
  subdf$plotColors <- as.numeric(subdf$period) + 1
  m <- lm(response ~ 1,data = subdf)
  
  form <- formula(paste("response ~",paste(IVs,collapse = " + ")))
  
  mstep <- step(m,scope = form,k=log(dim(subdf)[1]))
  
  models[[i]] <- mstep
  
  summary(mstep)
  r2_values <- c(r2_values,summary(mstep)$adj.r.squared)
  
  plot(subdf$response,predict(mstep),xlab = "Observed",ylab = "Fitted",col = subdf$plotColors,pch=20,cex=0.8)
  R2text <- bquote("adj R"^"2"~"="~.(round(r2_values[i],2)))
  mtext(side=3,line=-1.5,R2text,col="orange") 
  legend("topleft",legend = c("Pre","Transition","Post"),col = c(2,3,4),pch=20,cex=0.8,bty = "n")
  
  subdf$resids <- residuals(mstep)
  
  transition <- as.POSIXct(c("2011-01-02"))
  
  
  pre <- subdf[subdf$period == "Pre","resids"]
  post <- subdf[subdf$period == "Post","resids"]
  wilcox_results <- signif(c(wilcox_results,wilcox.test(pre,post,paired = FALSE)$p.value),2)
  t.test_results <- c(t.test_results,t.test(pre,post)$p.value)
  
  par(mar = c(5,5,3,1))
  bp <- boxplot(resids~period,data = subdf, ylab = "Residuals (Log E. coli cfu/100 mL)")
  mtext(paste0("Pre = ",testPeriod[1]," - ",testPeriod[2],"; Post = ",testPeriod[3]," - ", testPeriod[4]),line =1)
  mtext(paste(beach, "Model residuals for pre and post periods: Test Period ",i),line = 2)
  mtext(paste("p_wilcox = ",wilcox_results[i]),line = 0.)
  mtext("n = ", line = -1, side = 3,adj=0, cex=0.8)
  mtext(paste(bp$n, sep = ""), at = seq_along(bp$n), line = -1, side = 3,cex=0.8)
  
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
dev.off()
shell.exec(filenm)







#TestResult <- TestResult_list[[1]]
TestResult <- rbind(TestResult_list[[1]],TestResult_list[[2]])
#   rbind(TestResult_list[[3]]) %>%
#   rbind(TestResult_list[[4]])
wilcox_results
t.test_results
r2_values

period1 <- filter(df,pdate < DataTestPeriod[[1]][2])
period2 <- filter(df,pdate > DataTestPeriod[[1]][2],pdate < DataTestPeriod[[1]][3])
period3 <- filter(df,pdate > DataTestPeriod[[1]][3])

mean(period1$Ecoli)
mean(period2$Ecoli)
mean(period3$Ecoli)

df$year <- as.POSIXlt(df$pdate)$year + 1900

boxplot(Ecoli~year,data = df,log = "y", ylab = "E. coli (cfu/100mL)",
        main = "Jeorse E. coli concentrations by Year")

summary(mstep)





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
