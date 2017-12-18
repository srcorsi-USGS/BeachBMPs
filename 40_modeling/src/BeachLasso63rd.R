#Adaptive LASSO routine for beach model development

#This routine uses adaptive LASSO with several  options for variable selection.
#For all options LASSO is the first option for the initial selection. The  unique models 
#and variables are compiled from each cross validation model run for use in stepwise 
#regression. Stepwise regression is done using standard ordinary least squares and 
#using maximum likelhood regression with options for censored values. 


#Adaptive LASSO is implemented by weighting the variables by the correlation coefficient
#with the respnse variable in a scaled matrix (all variables scaled by mean and standard deviation)

#########################################################
#setwd("C:/Users/Jenn/Documents/Oshkosh/MantyOptical/Data")
library(glmnet)
library(censReg)
library(parallel)

#source(paste(Rlocal,"/Scripts/fxn_multiGrep.R",sep=""))
source("D:/SRCLData/Git/BeachBMPs/40_modeling/src/fxn_Rsquared.R")

###########################################
# Define functions for parallel processing#
compute <- function(l){
  model <- list()
  set.seed(seed[l])
  # Use lasso cross validation for initial variable selection
  g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200,standardize=FALSE)
  return(g1)
}

extract <- function(CV.models,seed){
  for (i in 1:length(seed)){
    g1 <- CV.models[[i]]
    c1<-coef(g1, s='lambda.min')
    c1.1se <- coef(g1,s='lambda.1se')
    beta<-which(abs(c1)>0)[-1]-1
    beta.1se <- which(abs(c1.1se)>0)[-1]-1
    testvars.orig <- c(testvars.orig,colnames(matIVs)[beta])
    testvars.orig.1se <- c(testvars.orig.1se,colnames(matIVs)[beta.1se])
    if(length(beta.1se)>0) {modelvars.1se[[i]] <- colnames(matIVs)[beta.1se]
    }else{modelvars.1se[[i]] <- "intercept"}
    varstring.1se[i] <- paste(modelvars.1se[[i]][order(modelvars.1se[[i]])],collapse="+")
  }
  return(list(testvars.orig,testvars.orig.1se,modelvars.1se,varstring.1se))
}

###########################################

# Load data
df.originalData <- df

###########################
# Help naming files
filetype <- c("63rd") #

################################################
# Rearrange dataframe so all IVs are at the end#

# Define first independent variable. Assume all columns after this are also IVs
beginIV <- names(df.originalData)[5] 
IVs <- names(df.originalData)[which(names(df.originalData)==beginIV):(dim(df.originalData)[2]-1)]
response <- 'Ecoli'
df.orig <- df.originalData[,c(response,IVs)]
df.orig[,response] <- round(df.orig[,response],3)
################################################


##########################################

#Set parameter for response variable and for naming files. 
parameter <- response

# Set left and right censoring values
leftCens <- log10(1) # lowest detection limit: I didn't see any below 1, so assume all samples had detections.
rightCens <- ifelse(logResponse,round(log10(2420),3), round(2420,3)) # this is the upper limit of the colilert method

EPAthresh <- log10(235)
thresh <- round(log10(235),1) #log10(235)


#Set Lasso method
method.cv <- "1se"

#Set number of cross validations per file
numXval <- 100

#Define response variable
response <- parameter  #define the response variables
logResponse <- TRUE    #should response be log transformed within the regression routine?


###########################################################################
# Initialize data frames, lists, variables,..... before cross validation 
###########################################################################

y.list <- list()
BeginTime <- proc.time()

CV.models <- list()
CV.var.summary <- list()

df <- df.orig


#  df <- df[,colSums(is.na(df)) <= 3]  #Remove columns with more than 3 NAs
df <- na.omit(df) #remove remaining rows with NAs

#remove columns that do not change 
#non.unique <- which(apply(df,2,function(x)length(unique(x))==1))
#if(length(non.unique)>0) df <- df[,-non.unique]


#Define IVs and matrix of IV values
responseCol <- which(names(df)==response)
begincol <- which(names(df)==beginIV)
IVs <- names(df)[begincol:length(df)]
matIVs.orig <- as.matrix(df[,IVs])
colnames(matIVs.orig) <- IVs

#Set seeds for cross validation
#set.seed(10)
seed <- sample(1:1000,size=numXval)

if(exists("freq.vars.all")) rm(freq.vars.all)
if(exists("model.all")) rm(model.all)
if(exists("models")) rm(models)

if(exists("model.all.step")) rm(model.all.step)
if(exists("models.step")) rm(models.step)

models <- list()
models.step <- list()
modelvars.1se <- list()
yadd <- 0

outlier <- which(df[,response]>50000000)
if (length(outlier)>0){y<-(df[-outlier,response])
matIVs <- matIVs.orig[-outlier,]
}else{y<-(df[,response])
matIVs <- matIVs.orig
}
if(logResponse){y <- log10(y)}
y<-round(y,3)
df[,response] <- y


##############################################################
# Run LASSO cross validations; use parallelization routine.  
##############################################################

matIVs <- scale(matIVs)
betas <- numeric()
r <- numeric()
for(k in 1:dim(matIVs)[2]){
  betas[k] <- coef(lm(y~matIVs[,k]))[2]
  r[k] <- cor(y,matIVs[,k])
}
for(k in 1:dim(matIVs)[2]){
  matIVs[,k] <- matIVs[,k] * r[k]
}

cores <- detectCores();use.cores <- ifelse(cores>=4,(cores-2),1)
cl <- makeCluster(getOption("cl.cores", use.cores))
clusterEvalQ(cl, library(package=glmnet)) 
clusterEvalQ(cl, library(package=censReg)) 
clusterExport(cl=cl, varlist=c("matIVs","y","seed"))
BeginTime2 <- proc.time()
CV.models <- parLapply(cl,1:length(seed),compute)
stopCluster(cl)
# Description of "CV.models" list: It is a nested list with these levels:
# 1. full results for all filetypes (length(filetype) elements long)
# 2. list of models by seed

testvars.orig <- character()
testvars.orig.1se <- character()
varstring.1se <- character()
model <- list()
modelvars.1se <- list()
CV.var.summary <- extract(CV.models,seed)
run.time2 <- proc.time() - BeginTime2#;run.time2

#######################################

#######################################
# Plot CV results for each data set
filenm <- paste(parameter,filetype,"CVgraphsAlass.pdf",sep="")
pdf(filenm)
CV.dev <- dev.cur()
for (k in 1:length(seed)){
  g1 <- CV.models[[k]]
  plot(CV.models[[k]])
  mtext(paste(response,"seed = ",seed[k]),line=2.5,font=2,cex=1.2)
}
dev.off(CV.dev)


#######################################

##########################################################################
# Sort out variables for Stepwise regressions and run stepwise regressions
##########################################################################

testvars.orig <- CV.var.summary[[1]]
testvars.orig.1se <- CV.var.summary[[2]]
modelvars.1se <- CV.var.summary[[3]]
varstring.1se <- CV.var.summary[[4]]

#Determine which model appears most frequently in LASSO cross validation
unique.1se <- unique(varstring.1se)
num.occur <- numeric()
max.occur.1se <- 0
for (k in 1:length(unique.1se)){
  num.occur[k] <- length(which(varstring.1se==unique.1se[k]))
}
model.1se <- which.max(num.occur)
vars.1se.exists <- !(is.na(unique.1se[model.1se]) | unique.1se[model.1se] == 'intercept')
if(vars.1se.exists){
  vars.1se <- unlist(strsplit(unique.1se[model.1se],fixed=T,split="+"))
  form <- formula(paste(response,"~",unique.1se[model.1se]))
  CR.1se <- censReg(form,left=leftCens,right=rightCens,data=df)
  
  #compute LASSO model with CV identified variables
  form <- formula(paste(response,"~",paste(unique(testvars.orig.1se),collapse="+")))
  X <- scale(as.matrix(df[,unique(testvars.orig.1se)]))
  m.CV <-cv.glmnet(X,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200,nfolds=trunc(nrow(X)/3))
  vars.CVcen <- coef(m.CV)[-1,]
  vars.CVcen <- vars.CVcen[which(abs(vars.CVcen)>0)]
  
  #recalibrate the lasso with censReg
  form <- formula(paste(response,"~",paste(names(vars.CVcen),collapse="+")))
  m.CVcen <- censReg(form,left=leftCens,right=rightCens,data=df)
  
}
#if(length(testvars.orig>0)){
if (method.cv=="1se") freq.vars <- as.data.frame(table(testvars.orig.1se))
if (method.cv=="min") freq.vars <- as.data.frame(table(testvars.orig))
freq.order <- order(freq.vars[,2],decreasing=T)
testvars <- as.character(freq.vars[freq.vars[,2]>(numXval/2.001),1])

if(length(testvars)<40 & length(testvars)>0){
  # Use stepwise regression for next phase of variable selection    
  #original scale est
  if (length(outlier)>0){testdf<-as.data.frame(df[-outlier,testvars])
  }else{testdf<-as.data.frame(df[,testvars])
  }
  form <- formula(paste("y~",paste(testvars,collapse="+"),sep=""))
  if(length(testvars)>1){
    lm1<-censReg(y~1,left=leftCens,right=rightCens,dat=testdf) #stepwise regression, forward and backward using BIC
    m2<-step(lm1,scope=form,k=log(nrow(testdf))) #BIC
    lm1.step<-lm(y~1,dat=testdf) #stepwise regression, forward and backward using BIC
    m2.step<-step(lm1.step,scope=form,k=log(nrow(testdf))) #BIC
  }else{
    names(testdf) <- testvars
    m2<-censReg(formula=form,left=leftCens,right=rightCens,dat=testdf) #stepwise regression, forward and backward using BIC
    m2.step<-lm(formula=form,dat=testdf) #stepwise regression, forward and backward using BIC
  }
  models <- m2
  models.step <- m2.step
  
  modelvars <- names(coef(m2))[-c(1,length(coef(m2)))]
  model.ind <- as.data.frame(coef(m2))
  modelvars.step <- names(coef(m2.step))[-1]
  model.ind.step <- as.data.frame(coef(m2.step))
  
  
  ##
}



############ Graphics ###########################
filenm <- paste(parameter,filetype,"ModelgraphsCenAndStepAlass.pdf",sep="")
pdf(filenm)
model.dev <- dev.cur()
# Graph most frequent 1se LASSO model results
dfRegressionResults <- data.frame(observations=y) # Save observations and predictions
# from all methods


# Graph most common lasso model recalibrated with CensReg
dev.set(model.dev)
if(vars.1se.exists){
  method <- "LASSO 1se Censored"
  par(mar=c(5,4,8,8))
  intercept <- coef(CR.1se)[1]
  df.model <- as.data.frame(df[,vars.1se])
  df.predict <- as.data.frame(df.model)
  predictions <- as.matrix(df.model) %*% as.numeric(coef(CR.1se)[vars.1se]) + intercept
  residuals.1se <- predictions-y
  dfRegressionResults <- cbind(dfRegressionResults,predictions)
  names(dfRegressionResults)[dim(dfRegressionResults)[2]] <- paste(method,"_",response,sep="")
  
  colors <- ifelse(y>EPAthresh & predictions>thresh,"blue","skyblue")
  colors <- ifelse(y>EPAthresh & predictions<thresh,"darkorange1",colors)
  colors <- ifelse(y<EPAthresh & predictions<thresh,"springgreen4",colors)
  colors <- ifelse(y<EPAthresh & predictions>thresh,"purple1",colors)
  sensitivity <- sum(sum(colors=="blue")/sum(y>EPAthresh))
  specificity <- sum(sum(colors=="springgreen4")/sum(y<EPAthresh))
  ylims <- round(range(c(y,predictions,na.rm=T)*2)+c(0,0.5),0)/2
  plot(predictions~y,xlab="Observations (gc/L)",ylab="Predictions (gc/L)",
       pch=20,ylim=ylims,xlim=ylims,col=colors)
  abline(h=thresh,v=EPAthresh,lty=3,col="blue")
  #        abline(0,1)
  coefSign <- ifelse(coef(CR.1se)[vars.1se]>0,"+","-")
  fsize <- 0.8
  for (j in length(vars.1se):1){
    k<-length(vars.1se)-j+1
    mtext(paste(coefSign[j],vars.1se[j],sep=""),side=3,line=fsize*k,adj=0,cex=fsize)
  }
  mtext(paste(method,": ",response,sep=""),line=0.,side=3,font=2)
  text(x=ylims[2],y=ylims[2],labels=paste("Correct pos =",sum(colors=="blue")),adj=c(0.9,0.5,1))
  text(x=ylims[1],y=ylims[2],labels=paste("False pos =",sum(colors=="purple1")),adj=c(0.1,0.5,1))
  text(x=ylims[1],y=ylims[1],labels=paste("Correct neg =",sum(colors=="springgreen4")),adj=c(0.1,0.5,1))
  text(x=ylims[2],y=ylims[1],labels=paste("False neg =",sum(colors=="darkorange1")),adj=c(0.9,0.5,1))
  mtext(paste("Sensitivity =",round(sensitivity,2)),side=4,line=2)
  mtext(paste("Specificity =",round(specificity,2)),side=4,line=1)
}else{
  plot(1:10,1:10,pch="")
  mtext(line=-5,"no model")
  if(length(testvars)>=40)mtext(line=-6,"too many variables remain")
}
dfModelCoefs <- data.frame(parameter=parameter,ModelID=method,Variable=names(coef(CR.1se)),
                           coefficient=coef(CR.1se),Method="1se",file=filetype)


#Graph Lasso model determined with most common variables and n/3-fold XV, then 
#recalibrated with CensReg
if(length(testvars.orig.1se)>0){
  method <- "LASSO CV"
  par(mar=c(5,4,8,8))
  par(mar=c(5,4,8,8))
  intercept <- coef(m.CVcen)[1]
  df.model <- as.data.frame(df[,names(vars.CVcen)])
  df.predict <- as.data.frame(df.model)
  predictions <- as.matrix(df.model) %*% coef(m.CVcen)[names(vars.CVcen)] + intercept
  dfRegressionResults <- cbind(dfRegressionResults,predictions)
  names(dfRegressionResults)[dim(dfRegressionResults)[2]] <- paste(method,"_",response,sep="")
  
  residuals <- predictions-y
  #smear <- get.smear(residuals)
  colors <- ifelse(y>EPAthresh & predictions>thresh,"blue","skyblue")
  colors <- ifelse(y>EPAthresh & predictions<thresh,"darkorange1",colors)
  colors <- ifelse(y<EPAthresh & predictions<thresh,"springgreen4",colors)
  colors <- ifelse(y<EPAthresh & predictions>thresh,"purple1",colors)
  sensitivity <- sum(sum(colors=="blue")/sum(y>EPAthresh))
  specificity <- sum(sum(colors=="springgreen4")/sum(y<EPAthresh))
  ylims <- round(range(c(y,predictions)*2)+c(0,0.5),0)/2
  plot(predictions~y,xlab="Observations (gc/L)",ylab="Predictions (gc/L)",
       pch=20,ylim=ylims,xlim=ylims,col=colors)
  abline(h=thresh,v=EPAthresh,lty=3,col="blue")
  #        abline(0,1)
  coefSign <- ifelse(vars.CVcen>0,"+","-")
  fsize <- 0.8
  for (j in length(vars.CVcen):1){
    k<-length(vars.CVcen)-j+1
    mtext(paste(coefSign[j],names(vars.CVcen[j]),sep=""),side=3,line=fsize*k,adj=0,cex=fsize)
  }
  mtext(paste(method,": ",response,sep=""),line=0.,side=3,font=2)
  text(x=ylims[2],y=ylims[2],labels=paste("Correct pos =",sum(colors=="blue")),adj=c(0.9,0.5,1))
  text(x=ylims[1],y=ylims[2],labels=paste("False pos =",sum(colors=="purple1")),adj=c(0.1,0.5,1))
  text(x=ylims[1],y=ylims[1],labels=paste("Correct neg =",sum(colors=="springgreen4")),adj=c(0.1,0.5,1))
  text(x=ylims[2],y=ylims[1],labels=paste("False neg =",sum(colors=="darkorange1")),adj=c(0.9,0.5,1))
  mtext(paste("Sensitivity =",round(sensitivity,2)),side=4,line=2)
  mtext(paste("Specificity =",round(specificity,2)),side=4,line=1)
}else{
  plot(1:10,1:10,pch="")
  mtext(line=-5,"no model")
  if(length(testvars)>=40)mtext(line=-6,"too many variables remain")
}
dfModelCoefs.tmp <- data.frame(parameter=parameter,ModelID=method,Variable=names(coef(m.CVcen)),
                               coefficient=coef(m.CVcen),Method="LCV",file=filetype)
dfModelCoefs <- rbind(dfModelCoefs,dfModelCoefs.tmp)


# Graph Censored regression results
#      thresh <- log10(235)
dev.set(model.dev)
if(length(testvars)<40 & length(testvars)>0 & length(coef(m2))>1){
  method <- "Censored"
  par(mar=c(5,4,8,8))
  intercept <- coef(m2)[1]
  df.model <- as.data.frame(testdf[,modelvars])
  df.predict <- as.data.frame(df.model)
  predictions <- as.matrix(df.model) %*% coef(m2)[modelvars] + intercept
  dfRegressionResults <- cbind(dfRegressionResults,predictions)
  names(dfRegressionResults)[dim(dfRegressionResults)[2]] <- paste(method,"_",response,sep="")
  
  residuals.Cens <- predictions-y
  colors <- ifelse(y>EPAthresh & predictions>thresh,"blue","skyblue")
  colors <- ifelse(y>EPAthresh & predictions<thresh,"darkorange1",colors)
  colors <- ifelse(y<EPAthresh & predictions<thresh,"springgreen4",colors)
  colors <- ifelse(y<EPAthresh & predictions>thresh,"purple1",colors)
  sensitivity <- sum(sum(colors=="blue")/sum(y>EPAthresh))
  specificity <- sum(sum(colors=="springgreen4")/sum(y<EPAthresh))
  
  ylims <- round(range(c(y,predictions)*2)+c(0,0.5),0)/2
  plot(predictions~y,xlab="Observations (gc/L)",ylab="Predictions (gc/L)",
       pch=20,ylim=ylims,xlim=ylims,col=colors)
  abline(h=thresh,v=EPAthresh,lty=3,col="blue")
  #        abline(0,1)
  coefSign <- ifelse(coef(m2)[modelvars]>0,"+","-")
  for (j in length(modelvars):1){
    k<-length(modelvars)-j+1
    mtext(paste(coefSign[j],modelvars[j],sep=""),side=3,line=fsize*k,adj=0,cex=fsize)
  }
  mtext(paste(method,": ",response,sep=""),line=0.,side=3,font=2)
  text(x=ylims[2],y=ylims[2],labels=paste("Correct pos =",sum(colors=="blue")),adj=c(0.9,0.5,1))
  text(x=ylims[1],y=ylims[2],labels=paste("False pos =",sum(colors=="purple1")),adj=c(0.1,0.5,1))
  text(x=ylims[1],y=ylims[1],labels=paste("Correct neg =",sum(colors=="springgreen4")),adj=c(0.1,0.5,1))
  text(x=ylims[2],y=ylims[1],labels=paste("False neg =",sum(colors=="darkorange1")),adj=c(0.9,0.5,1))
  mtext(paste("Sensitivity =",round(sensitivity,2)),side=4,line=2)
  mtext(paste("Specificity =",round(specificity,2)),side=4,line=1)
}else{
  plot(1:10,1:10,pch="")
  mtext(line=-5,"no model")
  if(length(testvars)>=40)mtext(line=-6,"too many variables remain")
}
dfModelCoefs.tmp <- data.frame(parameter=parameter,ModelID=method,Variable=names(coef(m2)),
                               coefficient=coef(m2),Method="CEN",file=filetype)
dfModelCoefs <- rbind(dfModelCoefs,dfModelCoefs.tmp)

# Graph traditional Stepwise regression results
if(length(testvars)<40 & length(testvars)>0 & length(coef(m2.step))>1){
  method <- "OLS"
  par(mar=c(5,4,8,8))
  intercept.step <- coef(m2.step)[1]
  df.model.step <- as.data.frame(testdf[,modelvars.step])
  df.predict.step <- as.data.frame(df.model.step)
  predictions.step <- as.matrix(df.model.step) %*% coef(m2.step)[modelvars.step] + intercept.step
  dfRegressionResults <- cbind(dfRegressionResults,predictions)
  names(dfRegressionResults)[dim(dfRegressionResults)[2]] <- paste(method,"_",response,sep="")
  
  residuals.ols <- predictions.step-y
  colors <- ifelse(y>EPAthresh & predictions.step>thresh,"blue","skyblue")
  colors <- ifelse(y>EPAthresh & predictions.step<thresh,"darkorange1",colors)
  colors <- ifelse(y<EPAthresh & predictions.step<thresh,"springgreen4",colors)
  colors <- ifelse(y<EPAthresh & predictions.step>thresh,"purple1",colors)
  sensitivity <- sum(sum(colors=="blue")/sum(y>EPAthresh))
  specificity <- sum(sum(colors=="springgreen4")/sum(y<EPAthresh))
  #        ylims <- round(range(c(y,predictions.step)*2)+c(0,0.5),0)/2
  plot(predictions.step~y,xlab="Observations (gc/L)",ylab="Predictions (gc/L)",
       pch=20,ylim=ylims,xlim=ylims,col=colors)
  abline(h=thresh,v=EPAthresh,lty=3,col="blue")
  #        abline(0,1)
  coefSign <- ifelse(coef(m2.step)[modelvars.step]>0,"+","-")
  for (j in length(modelvars.step):1){
    k<-length(modelvars.step)-j+1
    mtext(paste(coefSign[j],modelvars.step[j],sep=""),side=3,line=fsize*k,adj=0,cex=fsize)
  }
  mtext(paste(method,": ",response,sep=""),line=0.1,side=3,font=2)
  text(x=ylims[2],y=ylims[2],labels=paste("Correct pos =",sum(colors=="blue")),adj=c(0.9,0.5,1))
  text(x=ylims[1],y=ylims[2],labels=paste("False pos =",sum(colors=="purple1")),adj=c(0.1,0.5,1))
  text(x=ylims[1],y=ylims[1],labels=paste("Correct neg =",sum(colors=="springgreen4")),adj=c(0.1,0.5,1))
  text(x=ylims[2],y=ylims[1],labels=paste("False neg =",sum(colors=="darkorange1")),adj=c(0.9,0.5,1))
  mtext(paste("Sensitivity =",round(sensitivity,2)),side=4,line=2)
  mtext(paste("Specificity =",round(specificity,2)),side=4,line=1)
  
  dfModelCoefs.tmp <- data.frame(parameter=parameter,ModelID=method,Variable=names(coef(m2.step)),
                                 coefficient=coef(m2.step),Method="OLS",file=filetype)
  dfModelCoefs <- rbind(dfModelCoefs,dfModelCoefs.tmp)
  dfModelCoefsFinal <- dfModelCoefs
  
  
  #Aggregate model results into one frequency dataframe per method
  freq.vars$model <- response
  if(exists("freq.vars.all")) {freq.vars.all <- rbind(freq.vars.all,freq.vars)
  }else{freq.vars.all <- freq.vars}
  
  
  #Aggregate model results into one model dataframe per method
  conf.ind <- as.data.frame(confint(m2))
  conf.ind$model <- response
  model.ind <- cbind(model.ind,conf.ind)
  if(exists("model.all")) {model.all <- rbind(model.all,model.ind)
  }else{model.all <- model.ind}
  
  conf.ind.step <- as.data.frame(confint(m2.step))
  conf.ind.step$model <- response
  model.ind.step <- cbind(model.ind.step,conf.ind.step)
  if(exists("model.all.step")) {model.all.step <- rbind(model.all.step,model.ind.step)
  }else{model.all.step <- model.ind.step}
}else{
  plot(1:10,1:10,pch="")
  mtext(line=-5,"no model")
  if(length(testvars)>=40)mtext(line=-6,"too many variables remain")
}
dev.off(model.dev)

shell.exec(filenm)
#############################################
# Save final variable files and model objects
#############################################

write.csv(freq.vars.all,paste(parameter,filetype,"ModelVarFreqCenAlass.csv",sep=""))
write.csv(model.all,paste(parameter,filetype,"ModelCICenAlass.csv",sep=""))
write.csv(model.all.step,paste(parameter,filetype,"ModelCIStepAlass.csv",sep=""))
names(models) <- response
names(models.step) <- response
save(models,file=paste(parameter,filetype,"ModelObjectsAlass.Rdata",sep=""))
save(models.step,file=paste(parameter,filetype,"ModelObjectsStepAlass.Rdata",sep=""))
save(dfRegressionResults,file=paste(parameter,filetype,"ObservationsAndPredictionsAlass.Rdata",sep=""))


write.csv(dfModelCoefsFinal,paste(parameter,"ModelcoefficientsAlass",response,".csv",sep=""),row.names=F)

