# Develop model and test for differences in water quality for beaches
# 63rd Street beach

library(glmnet)

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

#######)))))))########


## LASSO MODELING  ###


response <- "Ecoli"
beginIV <- which(names(df)=="Ecoli")+1
endIV <- dim(df)[2]-1

bdate <- as.POSIXct("2006-01-01")
edate <- as.POSIXct("2018-01-01")

modelRows <- which(df$pdate >= bdate & df$pdate <= edate)
model_beach_wq_change <- function(df,response,beginIV,endIV){

  IVs <- names(df)[beginIV:endIV]
  matIVs <- as.matrix(df[modelRows,IVs])
  colnames(matIVs) <- IVs
  y<-log10(df[modelRows,response])

  g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200)

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

