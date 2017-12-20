#Adaptive LASSO routine for beach model development

#This routine uses adaptive LASSO with several  options for variable selection.
#For all options LASSO is the first option for the initial selection. The  unique models 
#and variables are compiled from each cross validation model run for use in stepwise 
#regression. Stepwise regression is done using standard ordinary least squares and 
#using maximum likelhood regression with options for censored values. 


#Adaptive LASSO is implemented by weighting the variables by the correlation coefficient
#with the respnse variable in a scaled matrix (all variables scaled by mean and standard deviation)

#########################################################

###########################################
# Define functions for parallel processing#
compute <- function(l){
  #model <- list()
  
  #for (l in 1:length(seed)){
  set.seed(seed[l])
  # Use lasso cross validation for initial variable selection
  g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200,standardize=FALSE)
  return(g1)
}


extract <- function(CV.models,seed){
  testvars.orig <- character()
  testvars.orig.1se <- character()
  varstring.1se <- character()
  modelvars.1se <- list()
  
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
setModelingSeed <- function(numXval){
  #Set seeds for cross validation
  #set.seed(10)
  seed <- sample(1:1000,size=numXval)
  return(seed)
}

getIVs <- function(df,beginIV,endIV){
  # Define first independent variable. Assume all columns after this are also IVs
  
  IVs <- names(df)[beginIV:endIV]
  return(IVs)
  
}

modelBeachBMP <- function(df,response,IVs,seed,logResponse = TRUE){
  
  ################################################
  # Rearrange dataframe so all IVs are at the end#
  
  df.orig <- df[,c(response,IVs)]
  df.orig[,response] <- round(df.orig[,response],3)
  ################################################
  
  
  ##########################################
  
  #Set Lasso method
  method.cv <- "1se"
  
  ###########################################################################
  # Initialize data frames, lists, variables,..... before cross validation 
  ###########################################################################
  
  y.list <- list()
  
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
  matIVs.orig <- as.matrix(df[,IVs])
  colnames(matIVs.orig) <- IVs
  
  y<-(df[,response])
  matIVs <- matIVs.orig
  
  
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
  
  for (l in 1:length(seed)){
    set.seed(seed[l])
    #   Use lasso cross validation for initial variable selection
    CV.models[[l]]<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200,standardize=FALSE)
  }
  
  # cores <- detectCores();use.cores <- ifelse(cores>=4,(cores-2),1)
  # cl <- makeCluster(getOption("cl.cores", use.cores))
  # clusterEvalQ(cl, library(package=glmnet)) 
  # #clusterEvalQ(cl, library(package=censReg)) 
  # clusterExport(cl=cl, varlist=c("matIVs","y","seed"))
  # CV.models <- parLapply(cl,1:length(seed),compute)
  # stopCluster(cl)
  # # Description of "CV.models" list: It is a nested list with these levels:
  # 1. full results for all filetypes (length(filetype) elements long)
  # 2. list of models by seed
  return(CV.models)
}


# testvars.orig <- character()
# testvars.orig.1se <- character()
# varstring.1se <- character()
# model <- list()
# modelvars.1se <- list()
getCVSummary <- function(CV.models,seed){
  CV.var.summary <- extract(CV.models,seed)
  return(CV.var.summary)
}
#######################################

plotCVResults <- function(CV.models,filenm,seed){
  #######################################
  # Plot CV results for each data set
  pdf(filenm)
  CV.dev <- dev.cur()
  for (k in 1:length(seed)){
    g1 <- CV.models[[k]]
    plot(CV.models[[k]])
    mtext(paste(response,"seed = ",seed[k]),line=2.5,font=2,cex=1.2)
  }
  dev.off(CV.dev)
}


#######################################

##########################################################################
# Sort out variables for Stepwise regressions and run stepwise regressions
##########################################################################

censRegAndlm_modeling <- function(df,response,logResponse,CV.var.summary){
  
  if(logResponse){y <- log10(df[,response])
  }else{
    y <- df[,response]
  }
  df[,response] <- round(y,3)
  
  lowerLimit <- 1
  upperLimit <- 4838
  
  numXval <- length(CV.var.summary[[4]])
  # Set left and right censoring values
  leftCens <- ifelse(logResponse,log10(lowerLimit),lowerLimit) # lowest detection limit: I didn't see any below 1, so assume all samples had detections.
  rightCens <- ifelse(logResponse,round(log10(upperLimit),3), round(upperLimit,3)) # this is the upper limit of the colilert method
  
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
    testdf<-as.data.frame(df[,testvars])
    
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
  return(1)
  
}

lm_modeling <- function(df,response,logResponse,CV.var.summary){
  
  if(logResponse){y <- log10(df[,response])
  }else{
    y <- df[,response]
  }
  df[,response] <- round(y,3)
  
  numXval <- length(CV.var.summary[[4]])
  
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
    m.1selm <- lm(form,data=df)
    
    step.1se <- m2<-step(m.1selm,scope=form,k=log(nrow(df))) #BIC
    
    #compute LASSO model with CV identified variables
    form <- formula(paste(response,"~",paste(unique(testvars.orig.1se),collapse="+")))
    X <- scale(as.matrix(df[,unique(testvars.orig.1se)]))
    m.CV <-cv.glmnet(X,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200,nfolds=trunc(nrow(X)/3))
    vars.CVcen <- coef(m.CV)[-1,]
    vars.CVcen <- vars.CVcen[which(abs(vars.CVcen)>0)]
    
    #recalibrate the lasso with censReg
    form <- formula(paste(response,"~",paste(names(vars.CVcen),collapse="+")))
    m.CVlm <- lm(form,data=df)
    
    step.cv <- m2<-step(m.CVlm,scope=form,k=log(nrow(df))) #BIC
    #m.CVcen
    #CV.1se
  }
  models <- list(step.1se,step.cv)
  names(models) <- c("1se","cv")
  return(list(step.1se,step.cv))
}

#   #if(length(testvars.orig>0)){
#   if (method.cv=="1se") freq.vars <- as.data.frame(table(testvars.orig.1se))
#   if (method.cv=="min") freq.vars <- as.data.frame(table(testvars.orig))
#   freq.order <- order(freq.vars[,2],decreasing=T)
#   testvars <- as.character(freq.vars[freq.vars[,2]>(numXval/2.001),1])
#   
#   if(length(testvars)<40 & length(testvars)>0){
#     # Use stepwise regression for next phase of variable selection    
#     #original scale est
#     testdf<-as.data.frame(df[,testvars])
#     
#     form <- formula(paste("y~",paste(testvars,collapse="+"),sep=""))
#     if(length(testvars)>1){
#       lm1<-censReg(y~1,left=leftCens,right=rightCens,dat=testdf) #stepwise regression, forward and backward using BIC
#       m2<-step(lm1,scope=form,k=log(nrow(testdf))) #BIC
#       lm1.step<-lm(y~1,dat=testdf) #stepwise regression, forward and backward using BIC
#       m2.step<-step(lm1.step,scope=form,k=log(nrow(testdf))) #BIC
#     }else{
#       names(testdf) <- testvars
#       m2<-censReg(formula=form,left=leftCens,right=rightCens,dat=testdf) #stepwise regression, forward and backward using BIC
#       m2.step<-lm(formula=form,dat=testdf) #stepwise regression, forward and backward using BIC
#     }
#     models <- m2
#     models.step <- m2.step
#     
#     modelvars <- names(coef(m2))[-c(1,length(coef(m2)))]
#     model.ind <- as.data.frame(coef(m2))
#     modelvars.step <- names(coef(m2.step))[-1]
#     model.ind.step <- as.data.frame(coef(m2.step))
#     
#     ##
#   }
#   return(1)
#   
# }

graphModels <- function(y,parameter,filetype){
  ############ Graphics ###########################
  filenm <- paste(parameter,filetype,"ModelgraphsCenAndStepAlass.pdf",sep="")
  pdf(filenm)
  model.dev <- dev.cur()
  
  #Define regulatory and model thresholds
  EPAthresh <- log10(235)
  thresh <- round(log10(235),1) #log10(235)
  
  
  
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
}  
plotModelPlain <- function(models,site,filenm,logResponse=TRUE){
  #Define observations and fitted values
  pdf(filenm)
  for (j in 1:length(models)){
    model <- models[[j]]
    method <- names(models)[j]
    y <- model[["model"]][,1]
    predictions <- model[["fitted.values"]]
    
    #Define axes limits
    ylims <- round(range(c(y,predictions,na.rm=T)*2)+c(0,0.5),0)/2
    
    #Define variables and coefficient signs
    variableNames <- names(coef(model))[-1]
    coefSign <- ifelse(coef(model)[variableNames]>0,"+","-")
    
    par(mar=c(5,4,8,2))
    colors <- "blue"
    plot(predictions~y,xlab="Observations (Counts/100 ml)",ylab="Predictions (Counts/100 ml)",
         pch=20,ylim=ylims,xlim=ylims,col=colors)
    abline(0,1)
    mtext(paste(site, method, "LASSO/lm regression model"),side=3,font = 2,line = 2,cex = 1.4)
    if(logResponse) mtext("In log 10 space",side=3,line=0.7,font=2)
    
    #Add variable names
    fsize <- 0.8
    for (j in length(variableNames):1){
      k<-length(variableNames)-j+1
      mtext(paste(coefSign[j],variableNames[j],sep=""),side=3,line=fsize*k,adj=0,cex=fsize)
    }
  }
  dev.off()
}

#plotModelPlain(testModels,"Junker","test.pdf")

