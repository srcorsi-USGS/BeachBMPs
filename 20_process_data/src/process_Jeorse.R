# Add perpendicular and parallel vectors components for wind and current



process_Jeorse <- function(dfJeorse) {
  # ## Add perpendicular and parallel wind components

  response <- "EC"
  beachAngle <- 27- 90 #needs to be changed for Jeorse angle
  
  
  dfModel <- dfJeorse
  
  velocities <- c(grep("WdSpd",names(dfModel),value=TRUE),
                  grep("WdGst",names(dfModel),value=TRUE))
  directions <- c(grep("WdDir",names(dfModel),value=TRUE),
                  grep("WdDir",names(dfModel),value=TRUE))
  
  
  for (i in 1:length(velocities)){
    
    dfModel <- polar2cart(dfModel,r = velocities[i], th = directions[i], circle = 360,
                          x= paste0(velocities[i],"East"),y=paste0(velocities[i],"North"))
    dfModel <- rotatecoords(df = dfModel,east = paste0(velocities[i],"East"),
                            north = paste0(velocities[i],"North"),
                            orientation = beachAngle,circle = 360,
                            name = velocities[i])
  }
  

  waveheights <- grep("SigWvHgt",names(dfModel),value=TRUE)
  WvDirections <- grep("WvDir",names(dfModel),value=TRUE)
  
  for (i in 1:length(waveheights)){
    
    dfModel <- polar2cart(dfModel,r = waveheights[i], th = WvDirections[i], circle = 360,
                          x= paste0(waveheights[i],"East"),y=paste0(waveheights[i],"North"))
    dfModel <- rotatecoords(df = dfModel,east = paste0(waveheights[i],"East"),
                            north = paste0(waveheights[i],"North"),
                            orientation = beachAngle,circle = 360,
                            name = waveheights[i])
  }
  
  
  #------------------------------------------------------
  
  ## Add perpendicular and parallel currents
  currentE <- grep("E",c(grep("AirVel",names(dfModel),value=TRUE),
    grep("Vel",names(dfModel),value=TRUE)),value = TRUE)
  currentN <- grep("N",c(grep("AirVel",names(dfModel),value=TRUE),
             grep("Vel",names(dfModel),value=TRUE)),value = TRUE)
  currentNames <- sub("_","",sub("E","",currentE))
    
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
  
  #remove original wind and current variables
  removeVars <- which(names(dfModel) %in% c(velocities,directions,WvDirections,currentE,currentN))
  dfModel <- temp[,-removeVars]
  
  ## end adding perpendicular and parallel currents ##
  
  
  ##----------------------------------##
  #reduce to data set for modeling and define pre, during, and post periods
  ## Reduce to 2006 and after for pre-post analysis
  
  df <- dfModel[which(!is.na(dfModel[,response])),] #remove rows without E coli

  preDates <- as.POSIXct(c("2010-05-01","2015-10-01"))
  initialPostPeriod <- as.POSIXct(c("2015-07-06","2015-08-01"))
  postDates <- as.POSIXct(c("2016-01-02","2017-10-01"))
#  df <- subset(df,pdate>preDates[1])
  df$period <- ifelse(df$pdate < preDates[2],"pre","post")
  df$period <- ifelse(df$pdate >postDates[1] & df$pdate < postDates[2],"during",df$period)
  df$period <- ifelse(df$pdate>initialPostPeriod[1] & df$pdate < initialPostPeriod[2],"pre",df$period)
  df <- df[which(df$pdate>preDates[1]),]
  # focus on critical variables for modeling.

  naCols <- apply(df,MARGIN = 2, function(x) sum(is.na(x)) > 0)
  dfMaxRows <- df[,colSums(is.na(df)) <= nrow(df)*0.05]  #Remove columns with more than 5% NAs
  
  dfMaxRows <- na.omit(dfMaxRows)
  return(dfMaxRows)
  
}


split_Jeorse <- function(df,site){
  df <- df[which(df[,"Location"] == site),]
  return(df)
}

