#functions to convert polar to cartesion and vice versa


#usage:

# cart2polar(df, x,y,r,th,circle)    df=data frame with x and y components
#                           x=x component (column in df), 
#                           y=y component (column in df), 
#                           circle =360 or 2*pi
#                           depending on which you would like returned
#                           Function returns data fram with added variables
#                           r (magnitude) and th (angle)
#

# polar2cart(df, r,th,x,y,circle)      df=data frame with r (magnitude) and th (angle) 
#                           r=magnitude, 
#                           th=angle, 
#                           circle =360 or 2*pi
#                           depending on whether th is in degrees or radians
#                           Function returns data frame with added variables 
#                           x and y.
#

cart2polar <- function(df,x,y,r="r",th="th",circle) {
  df[,r] <- sqrt(df[,x]^2+df[,y]^2)
  df[,th] <- (atan(df[,x]/df[,y])/2/pi * circle + ifelse(df[,y] > 0, circle, 1.5 * 
        circle))%%circle
return(df)
}

polar2cart <- function(df,r,th,x="x",y="y",circle){
  df[,x] <- df[,r] * sin(df[,th]/circle * 2 * pi)
  df[,y] <- df[,r] * cos(df[,th]/circle * 2 * pi)
return(df)
}



cart2polarVectors <- function(x,y,circle) {
  r <- sqrt(x^2+y^2)
  th <- (atan(x/y)/2/pi * circle + ifelse(y > 0, circle, 1.5 * 
                                            circle))%%circle
  rth <- data.frame(r,th)
  return(rth)
}

polar2cartVectors <- function(r,th,circle){
  x <- r * sin(th/circle * 2 * pi)
  y <- r * cos(th/circle * 2 * pi)
  xy<-data.frame(x,y)
  return(xy)
}

######
# Rotate east/north components to perpendicular and parallel

rotatecoords  <- function(df,east,north,orientation,circle,name) {
  
  eastloc <- which(names(df) == east)
  northloc <- which(names(df) == north)
  # Convert from east/north components to polar
  rth <- cart2polarVectors(df[,eastloc],df[,northloc],circle)
  
  # Rotate to beach orientation
  rth[,2] <- (rth[,2] - orientation + circle)%%circle
  xy <- polar2cartVectors(rth[,1],rth[,2],circle)
  
  # Replace east/north velocities with perpendicular (perp) and 
  # parallel (par) components
  names(df)[eastloc] <- paste(name,"perp",sep="")
  names(df)[northloc] <- paste(name,"parl",sep="")
  df[,eastloc] <- xy[,1]
  df[,northloc] <- xy[,2]
  return(df)
}

