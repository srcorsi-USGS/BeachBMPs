# Add perpendicular and parallel vectors components for wind and current


# df63rd <- make("df63rd")

process_63rd <- function(df63rd) {
  # ## Add perpendicular and parallel wind components
  
  response <- "Ecoli"
  beachAngle <- 27- 90
  
  
  velocities <- c("CMT.WdSpd4", "CMT.WdSpd6", "CMT.WdGst4", "CMT.WdGst6")
  directions <- c("CMT.WdDir4", "CMT.WdDir6", "CMT.WdDir4", "CMT.WdDir6")
  
  dfModel <- df63rd
  
  for (i in 1:length(velocities)){
    
    dfModel <- polar2cart(dfModel,r = velocities[i], th = directions[i], circle = 360,
                          x= paste0(velocities[i],"East"),y=paste0(velocities[i],"North"))
    dfModel <- rotatecoords(df = dfModel,east = paste0(velocities[i],"East"),north = paste0(velocities[i],"North"),orientation = beachAngle,circle = 360,name = velocities[i])
  }
  
  
  #------------------------------------------------------
  
  ## Add perpendicular and parallel currents
  
  currentE <- c("GL.E.WtrVelSur4", "GL.E.WtrVelSur6", "GL.E.WtrVelDA4", "GL.E.WtrVelDA6")
  currentN <- c("GL.N.WtrVelSur4", "GL.N.WtrVelSur6", "GL.N.WtrVelDA4", "GL.N.WtrVelDA6")
  currentNames <- c("Sur4","Sur6","DA4","DA6")
  
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
  
  ## end adding perpendicular and parallel currents ##
  
  
  ##----------------------------------##
  #reduce to data set for modeling and define pre, during, and post periods
  ## Reduce to 2006 and after for pre-post analysis
  
  df <- dfModel[which(!is.na(dfModel[,response])),] #remove rows without E coli
  
  preDates <- as.POSIXct(c("2006-01-02","2010-01-02"))
  postDates <- as.POSIXct(c("2010-01-02","2016-01-02"))
  #  df <- subset(df,pdate>preDates[1])
  df$period <- ifelse(df$pdate < preDates[2],"pre","post")
  df$period <- ifelse(df$pdate >postDates[1] & df$pdate < postDates[2],"during",df$period)
  df <- df[df$pdate>preDates[1],]
  # focus on critical variables for modeling.
  
  #remove Chicago weather variables in favor of Calumet weather
  df <- df[,-grep("CH",names(df))]
  naCols <- apply(df,MARGIN = 2, function(x) sum(is.na(x)) > 0)
  dfMaxRows <- df[,colSums(is.na(df)) <= nrow(df)*0.05]  #Remove columns with more than 20% NAs
  
  dfMaxRows <- na.omit(dfMaxRows)
  return(dfMaxRows)
  
}

# Explore number of IVs available for each observation over the years

plot_variable_availability <- function(df,pdate="pdate",site){
  pdf(paste0("20_process_data/figures/data_available_",site,".pdf"))
  IVcount <- apply(df,MARGIN = 1,function(x)sum(!is.na(x)))
  Obscount <- apply(df,MARGIN = 2,function(x)sum(!is.na(x)))
  
  plot(df$pdate,IVcount,xlab="",ylab="")
  mtext("Independent variable availability",side=2,line=2.5,cex=1.5,font=2)
  mtext("Date",side=1,line=3,cex=1.5,font=2)
  mtext(paste0(site,": Independent Variables Available for E. coli observations"),side = 3, line = 2,font = 2, cex = 1.5)
  dev.off()
}