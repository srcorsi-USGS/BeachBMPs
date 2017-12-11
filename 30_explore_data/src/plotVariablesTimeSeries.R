

plotVarsTS <- function(df, target_name, beach){
  filenm <- target_name
  pdf(filenm)


  if(beach %in% c("63rd","Jeorse")){
    variables <- names(df)[4:dim(df)[2]]
    dateCol <- "pdate"
  }
      

  x <- df[,dateCol]

  for(i in 1:length(variables)) {
    y <- df[,variables[i]]
    plot(x,y,ylab=variables[i],log="y",main = paste(variables[i],"at",beach))
  }
  dev.off()
}

