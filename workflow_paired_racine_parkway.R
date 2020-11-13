library(tools)

source("10_load_data/src/get_Racine_data.R")
source("20_process_data/src/fxn_cart2polar_polar2cart.R")
source("20_process_data/src/fxn_cart2polar_polar2cart_df.R")
source("20_process_data/src/process_Racine.R")

#read and process Racine NB data

dfNB <- read.csv(file = ("10_load_data/raw/Racine/North Beach Historical Database 1995-2019 COMBINED EDIT.csv"),
                 stringsAsFactors = FALSE, 
                 na.strings = c('NR','#N/A', 'NC', 'No Data','N/A','NA'))

dfNBIVs <- read.csv(file = ("10_load_data/raw/Racine/IVs.csv"),stringsAsFactors = FALSE)

dfRacine <- get_Racine(df=dfNB,dfIVs=dfNBIVs)

dfModel <- process_Racine(dfRacine)

#Read parkway Beach
parkway.orig <- read.csv("10_load_data/raw/Racine/parkway.csv",stringsAsFactors = FALSE)

parkway.orig$pdate <- as.Date(parkway.orig$Sample.date.time,format = "%m/%d/%Y")
parkway <- parkway.orig %>%
  rename(parkway = E..Coli.Value,parkway.rmk = E..Coli.Remark,parkway.units = E..Coli.Units,parkway.tmp = Water.temp...F.) %>%
  group_by(pdate) %>%
  summarize(parkway = mean(parkway),parkway.rmk = paste(unique(parkway.rmk),sep = "|"),
            parkway.units = paste(unique(parkway.units),sep = "|"), n = length(parkway))

dfRacine$pdate <- as.Date(dfRacine$pdate,format = "%m/%d/%Y")
df <- left_join(parkway,dfRacine) %>%
  filter(!is.na(Ecoli))
parkway$pdate
dfRacine$pdate

plot(df$Ecoli ~ df$pdate, log = "y")
points(df$parkway ~ df$pdate, col = "blue",pch = 20)

df$year <- as.POSIXlt(df$pdate)$year + 1900

rparkwaytoNorth <- df$parkway/df$Ecoli

par (mfrow=c(3,1), mar = c(1,5,0,1), oma = c(5,0,4,1))
boxplot(parkway ~ year,data = df,log = "y", xaxt = "n")
boxplot(Ecoli ~ year,data = df,log = "y", xaxt = "n")

boxplot(rparkwaytoNorth~df$year,log = "y")

par(mfrow = c(1,1),mar = c(5,5,3,2),oma = c(0,0,0,0))
plot(df$parkway,df$Ecoli,log = "xy",xlab = "parkway",ylab = "North Beach",
     main = "Comparison of North Beach to Parkway E. coli Concentrations")

abline(0,1)
m <- lm(Ecoli ~ parkway, data = df)
summary(m)
df$residuals <- residuals(m) 
df$predictions <- predict(m)
boxplot(residuals ~ year,data = df,log="y")
boxplot(residuals ~ year,data = df,ylim = c(-500,500))

breakYear <- 2012.5

df$prepost <- ifelse(df$year < breakYear,"pre", "post")

boxplot(residuals~prepost,data = df,ylim = c(-500,500))
table(df$prepost)

boxplot(df$alford ~ df$alford.units, log = "y", xlab = "Analytical Method",ylab = "E. coli concentration",
        main = "Alford comparison of results by analytical method")

Alford_method <- paste("Alford",df$alford.units,"Period")
boxplot(df$Ecoli ~ Alford_method, log = "y", xlab = "Alford Analytical Method period",ylab = "E. coli concentration",
        main = "North Beach comparison of results by time period of Alford analytical method")
