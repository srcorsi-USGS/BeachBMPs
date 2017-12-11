#Script to graph beach model results with threshold of choice

################################################################
# Variables to modify ##########################################
################################################################
Bias <- 1.6
Thresh <- 235/Bias # in CFU or MPN
thresh <- log10(Thresh) #log transform of Thresh
EPAthresh <- log10(235)

#Set beach for naming files
beachDir <- "Marina"
beach  <- "BRMarina.test."
filetype <- c("MaxRowsTurb") #,"MaxRows","MaxCols","EnDDaT","MaxRowsTurb")
l <- 1
#Subdirectory to write pdf files
writeDir <- "Lasso"
################################################################

################################################################

#Change to data exploration directory
currentDir <- getwd()
setwd(paste("//igsarmewfsapa/projects/QW Monitoring Team/GLRI beaches/Modeling/Models for 2014 season/Model Exploration/",beachDir,"/",writeDir,sep=""))

load(paste(beach,filetype[l],"ObservationsAndPredictions.Rdata",sep=""))

pdf(paste(beach,filetype[l],"Modelgraphs",Thresh,".pdf",sep=""))
methods <- names(dfRegressionResults)[which(names(dfRegressionResults)!="observations")]
y <- dfRegressionResults$observations
par(mar=c(5,4,8,8))

for (i in 1:length(methods)){
  method <- methods[i]
  predictions <- dfRegressionResults[,method]
  
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
  
  mtext(paste(method,sep=""),line=0.1,side=3,font=2)
  text(x=ylims[2],y=ylims[2],labels=paste("Correct pos =",sum(colors=="blue")),adj=c(0.9,0.5,1))
  text(x=ylims[1],y=ylims[2],labels=paste("False pos =",sum(colors=="purple1")),adj=c(0.1,0.5,1))
  text(x=ylims[1],y=ylims[1],labels=paste("Correct neg =",sum(colors=="springgreen4")),adj=c(0.1,0.5,1))
  text(x=ylims[2],y=ylims[1],labels=paste("False neg =",sum(colors=="darkorange1")),adj=c(0.9,0.5,1))
  mtext(paste("Sensitivity =",round(sensitivity,2)),side=4,line=2)
  mtext(paste("Specificity =",round(specificity,2)),side=4,line=1)
  
}
dev.off()
shell.exec(paste(beach,filetype[l],"Modelgraphs",Thresh,".pdf",sep=""))
setwd(currentDir)
