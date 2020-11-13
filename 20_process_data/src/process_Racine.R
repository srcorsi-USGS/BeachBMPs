# Add perpendicular and parallel vectors components for wind and current


# df63rd <- make("df63rd")

process_Racine <- function(df) {
  # ## Add perpendicular and parallel currents and wind components
  
  response <- "Ecoli"
  beachAngle <- 338.55
  
  
  velocities <- c("Water.Velocity.at.Surface.m.s.", "Significant.Wave.Height.meter.")
  directions <- c("Water.Velocity.at.Surface.Direction.Degrees.0.toward.North.", "Wave.Direction.Degrees.0.toward.North.")
  
  dfModel <- df
  
  for (i in 1:length(velocities)){
    
    dfModel <- polar2cart(dfModel,r = velocities[i], th = directions[i], circle = 360,
                          x= paste0(velocities[i],"East"),y=paste0(velocities[i],"North"))
    dfModel <- rotatecoords(df = dfModel,east = paste0(velocities[i],"East"),north = paste0(velocities[i],"North"),orientation = beachAngle,circle = 360,name = velocities[i])
  }
  
  
  #------------------------------------------------------
  
  ## Add perpendicular and parallel currents
  
  currentE <- c("Depth.Averaged.Eastward.Water.Velocity.m.s.","Eastward.Air.Velocity.m.s.")
  currentN <- c("Depth.Averaged.Northward.Water.Velocity.m.s.","Northward.Air.Velocity.m.s.")
  currentNames <- c("DepthAve","Wind")
  
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
  
  #remove directional vectors in favor of perp and parl variables
  
  removeCols <- grep("Eastward|Northward|speed|Direction|Air.Velocity",
                     names(dfModel),ignore.case = TRUE)
  dfModel <- dfModel[,-removeCols]
  
  
  ## end adding perpendicular and parallel currents ##
  
  # Add jday
  dfModel$jday <- as.POSIXlt(dfModel$pdate)$yday+1
  
  # Add transformations
  #SQRT Rain
  Rain_variables <- grep("precip|Precip",names(dfModel))
  dfRain <- sqrt(dfModel[,Rain_variables])
  names(dfRain) <- paste0("sqrt_",names(dfRain))
  dfModel <- cbind(dfModel,dfRain)
  
  #log Q
  Q_variables <- grep("cubic",names(dfModel),ignore.case = TRUE)
  dfQ <- log10(dfModel[,Q_variables])
  names(dfQ) <- paste0("log_",names(dfQ))
  dfModel <- cbind(dfModel,dfQ)
  
  
  
  ##----------------------------------##
  #reduce to data set for modeling and define pre, during, and post periods
  ## Reduce to 2006 and after for pre-post analysis
  
  df <- dfModel[which(!is.na(dfModel[,response])),] #remove rows without E coli
  
  preDates <- as.POSIXct(c("2006-01-02","2011-01-02"))
  postDates <- as.POSIXct(c("2011-01-02","2016-10-02"))
  #  df <- subset(df,pdate>preDates[1])
  df$period <- ifelse(df$pdate < preDates[2],"pre","post")
  df$period <- ifelse(df$pdate >postDates[1] & df$pdate < postDates[2],"during",df$period)
  #df <- df[df$pdate>preDates[1],]
  # focus on critical variables for modeling.
  
  
  naCols <- apply(df,MARGIN = 2, function(x) sum(is.na(x)) > 0)
  dfMaxRows <- df[,colSums(is.na(df)) <= nrow(df)*0.05]  #Remove columns with more than 5% NAs
  dfMaxRows <- dfMaxRows[,-which(names(dfMaxRows)=="Adjusted.Gull.Count")] # Removed due to many missing values in the pre period and no apparent relation to E coli.
    dfMaxRows <- na.omit(dfMaxRows)
  
  #Convert character variables into numeric categories
  #Discharge categories
  # Q_cat_variables <- which(names(dfModel) %in% c("EOF.Discharge","IEB.Discharge" ))
  # dfMaxRows$EOF.Discharge <- toTitleCase(dfMaxRows$EOF.Discharge)
  # dfMaxRows$IEB.Discharge <- toTitleCase(dfMaxRows$IEB.Discharge)
  # 
  
  #Water Clarity
  if("Water.Clarity" %in% names(dfMaxRows)){
    WaterClarity <- c(1:5)
    names(WaterClarity) <- c("Clear","Turbid","Slightly turbid","Very Turbid","Opaque")
    dfMaxRows$Water.Clarity <- WaterClarity[dfMaxRows$Water.Clarity]
  }
  #Algae
  if("Algae.on.Beach" %in% names(dfMaxRows)){
    dfMaxRows$Algae.on.Beach <- toTitleCase(dfMaxRows$Algae.on.Beach)
    dfMaxRows$Algae.in.Nearshore.Water <- toTitleCase(dfMaxRows$Algae.in.Nearshore.Water)
    Algae <- 1:4
    names(Algae) <- c("None","Low","Moderate","High")
    dfMaxRows$Algae.on.Beach <- Algae[dfMaxRows$Algae.on.Beach]
    dfMaxRows$Algae.in.Nearshore.Water <- Algae[dfMaxRows$Algae.in.Nearshore.Water]
  }
  return(dfMaxRows)
  
}

# Explore number of IVs available for each observation over the years

plot_variable_availability <- function(df,site,pdate="pdate"){
  pdf(paste0("20_process_data/figures/data_available_",site,".pdf"))
  IVcount <- apply(df,MARGIN = 1,function(x)sum(!is.na(x)))
  Obscount <- apply(df,MARGIN = 2,function(x)sum(!is.na(x)))
  
  plot(df$pdate,IVcount,xlab="",ylab="")
  mtext("Independent variable availability",side=2,line=2.5,cex=1.5,font=2)
  mtext("Date",side=1,line=3,cex=1.5,font=2)
  mtext(paste0(site,": Independent Variables Available for E. coli observations"),side = 3, line = 2,font = 2, cex = 1.5)
  dev.off()
}
