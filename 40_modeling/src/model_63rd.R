# Develop model and test for differences in water quality for beaches
# 63rd Street beach

library(glmnet)

df63rd <- make("df63rd")


## Add perpendicular and parallel wind components
source("./20_process_data/src/fxn_cart2polar_polar2cart_df.R")

velocities <- c("CMT.WdSpd4", "CMT.WdSpd6", "CMT.WdGst4", "CMT.WdGst6")
directions <- c("CMT.WdDir4", "CMT.WdDir6", "CMT.WdDir4", "CMT.WdDir6")

dfModel <- df63rd
for (i in 1:length(velocities)){
  
  dfModel <- polar2cart(dfModel,r = velocities[i], th = directions[i], circle = 360,
                        x= paste0(velocities[i],"Perp"),y=paste0(velocities[i],"Parl"))
}

currentE <- c("GL.E.WtrVelSur4", "GL.E.WtrVelSur6", "GL.E.WtrVelDA4", "GL.E.WtrVelDA6")
currentN <- c("GL.N.WtrVelSur4", "GL.N.WtrVelSur6", "GL.N.WtrVelDA4", "GL.N.WtrVelDA6")
currentNames <- c("Sur4","Sur6","DA4","DA6")
beachAngle <- 27

temp <- dfModel
for (i in 1:length(currentE)) {
  varNamesOrig <- names(temp)
  r <- paste0(currentNames[i],"Spd")
  th <- paste0(currentNames[i],"dir")
  PerpName <- paste0(currentNames[i],"Perp")
  ParlName <- paste0(currentNames[i],"Parl")
  #test <- rotatecoords(df = dfModel,east = currentE[i],north = currentN[i],orientation = 27,circle = 360,name = currentNames[i])
  temp <- cart2polar(df=temp,x=currentE[i],y=currentN[i],
                    r=r,th = th,circle=360)
  temp[,th] <- temp[,th] - beachAngle
  temp <- polar2cart(df=temp,r = r,th = th,circle=360,
                     x=PerpName,y=ParlName)
  temp <- temp[,c(varNamesOrig,PerpName,ParlName)]

}
dfModel <- temp




#######(((((((########
#reduce to data set for modeling and define pre, during, and post periods
## Reduce to 2006 and after for pre-post analysis
response <- "Ecoli"
df <- dfModel[which(!is.na(dfModel[,response])),] #remove rows without E coli

preDates <- as.POSIXct(c("2006-01-02","2010-01-02"))
postDates <- as.POSIXct(c("2010-01-02","2016-01-02"))
df <- subset(df,pdate>preDates[1])
df$period <- ifelse(df$pdate < preDates[2],"pre","post")
df$period <- ifelse(df$pdate >postDates[1] & df$pdate < postDates[2],"during",df$period)

# focus on critical variables for modeling.

#remove Chicago weather variables in favor of Calumet weather
df <- df[,-grep("CH",names(df))]
names(df)
IVcount <- apply(df,MARGIN = 1,function(x)sum(!is.na(x)))
Obscount <- apply(df,MARGIN = 2,function(x)sum(!is.na(x)))
dfMaxRows <- df[,colSums(is.na(df)) <= nrow(df)*0.05]  #Remove columns with more than 20% NAs

# Explore number of IVs available for each observation over the years

plot(df$pdate,IVcount,xlab="",ylab="")
mtext("Independent variable availability",side=2,line=2.5,cex=1.5,font=2)
mtext("Date",side=1,line=3,cex=1.5,font=2)
mtext("63rd St: Independent Variables Available for E. coli observations",side = 3, line = 2,font = 2, cex = 1.5)

df <- na.omit(df)

#######)))))))########

# 
# 
# 
# response <- "Ecoli"
# beginIV <- which(names(df)=="Ecoli")+1
# endIV <- dim(df)[2]-1
# 
# model_beach_wq_change <- function(df,response,beginIV,endIV){
#   
#   IVs <- names(df)[beginIV:endIV]
#   matIVs <- as.matrix(df[,IVs])
#   colnames(matIVs) <- IVs
#   y<-log10(df[,response])
#   
#   g1<-cv.glmnet(matIVs,y,alpha=1,type.measure="mse",family='gaussian',nlambda=200)
#   
# }
# 
# c1<-coef(g1, s='lambda.min')
# c1.1se <- coef(g1,s='lambda.1se')
# beta<-which(abs(c1)>0)[-1]-1
# beta.1se <- which(abs(c1.1se)>0)[-1]-1
# testvars.orig <- c(testvars.orig,colnames(matIVs)[beta])
# testvars.orig.1se <- c(testvars.orig.1se,colnames(matIVs)[beta.1se])
# 
# plot(g1)
# 
# 
# plot(y,predict(g1,newx=matIVs))
# 
# 
# ls()
# save(list=ls(),file="workspace.RData")

