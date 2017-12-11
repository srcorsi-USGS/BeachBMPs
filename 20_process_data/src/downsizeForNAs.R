df63rd <- make("df63rd")

response <- "Ecoli"
df <- df63rd[which(!is.na(df63rd[,response])),]
dfMaxRows <- df[,colSums(is.na(df)) <= nrow(df)*0.3]  #Remove columns with more than 20% NAs
dfMaxRows <- na.omit(dfMaxRows)

plot(dfMaxRows$pdate,dfMaxRows$Ecoli)
plot(df$pdate,df$Ecoli)


IVcount <- apply(df,MARGIN = 1,function(x)sum(!is.na(x)))

# Explore number of IVs available for each observation over the years

plot(df$pdate,IVcount,xlab="",ylab="")
mtext("Independent variable availability",side=2,line=2.5,cex=1.5,font=2)
mtext("Date",side=1,line=3,cex=1.5,font=2)
mtext("63rd St: Independent Variables Available for E. coli observations",side = 3, line = 2,font = 2, cex = 1.5)


countNAs <- function(df){
  test <- apply(df, MARGIN=2, FUN=function(x)sum(is.na(x)))
  df <- df[,-which(test>150)]
  
}




