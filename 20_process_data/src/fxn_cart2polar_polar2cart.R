#functions to convert polar to cartesion and vice versa


#usage:

# cart2polar(x,y,circle)    x=x component, y=y component, circle =360 or 2*pi
#                           depending on which you would like returned
#                           Function returns r (magnitude) and th (angle)
#

# polar2cart(r,th,circle)  r=magnitude, th=angle, , circle =360 or 2*pi
#                           depending on whether th is in degrees or radians
#

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
rth <- cart2polar(df[,eastloc],df[,northloc],circle)

# Rotate to beach orientation
rth[,2] <- (rth[,2] - orientation + circle)%%circle
xy <- polar2cart(rth[,1],rth[,2],circle)

# Replace east/north velocities with perpendicular (perp) and 
# parallel (par) components
names(df)[eastloc] <- paste("perp",name,sep="")
names(df)[northloc] <- paste("parl",name,sep="")
df[,eastloc] <- xy[,1]
df[,northloc] <- xy[,2]
return(df)
}