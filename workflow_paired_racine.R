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

#Read Alford Beach
alford.orig <- read.csv("10_load_data/raw/Racine/Alford.csv",stringsAsFactors = FALSE)

alford.orig$pdate <- as.Date(alford.orig$Sample.date.time,format = "%m/%d/%Y")
alford <- alford.orig %>%
  rename(alford = E..Coli.Value,alford.rmk = E..Coli.Remark,alford.units = E..Coli.Units,alford.tmp = Water.temp...F.) %>%
  group_by(pdate) %>%
  summarize(alford = mean(alford),alford.rmk = paste(unique(alford.rmk),sep = "|"),
            alford.units = paste(unique(alford.units),sep = "|"), n = length(alford))

dfModel$pdate <- as.Date(dfModel$pdate,format = "%m/%d/%Y")
df <- left_join(alford,dfModel) %>%
  filter(!is.na(Ecoli))
alford$pdate
dfModel$pdate

plot(df$Ecoli ~ df$pdate, log = "y")
points(df$alford ~ df$pdate, col = "blue",pch = 20)

df$year <- as.POSIXlt(df$pdate)$year + 1900

rAlfordtoNorth <- df$alford/df$Ecoli

par (mfrow=c(3,1), mar = c(1,5,0,1), oma = c(5,0,4,1))
boxplot(alford ~ year,data = df,log = "y", xaxt = "n", ylab = "Alford")
mtext ("E coli concentratons at Alford, North Beach (for matching dates), and the Ratio of the Two",line = 1)
boxplot(Ecoli ~ year,data = df,log = "y", xaxt = "n", ylab = "North Beach" )

boxplot(rAlfordtoNorth~df$year,log = "y", ylab = "Ratio Alford/North Beach")

#Define pre and post periods
breakYear <- 2011.5
df$prepost <- ifelse(df$year < breakYear,"pre", "post")


par(mfrow = c(1,1),mar = c(5,3,3,2),oma = c(0,0,0,0))
plot(log10(df$alford),log10(df$Ecoli),xlab = "Alford",ylab = "North Beach",col = )
mlog <- lm(log10(Ecoli) ~ log10(alford), data = df)
summary(mlog)
abline(1.05,0.31)
df$residuals <- residuals(m) 
boxplot(residuals~prepost,data = df,ylim = c(-500,500))


par(mfrow = c(1,1),mar = c(5,5,3,2),oma = c(0,0,0,0))
plot(df$alford,df$Ecoli,log = "xy",xlab = "Alford",ylab = "North Beach",col = )
abline(0.178,115)
abline(0,1)
abline(lm(Ecoli ~ alford, data = df))
abline(120,0.179)
m <- lm(Ecoli ~ alford, data = df)
summary(m)
summary(mlog)
df$residuals <- residuals(m) 
df$predictions <- predict(m)
boxplot(residuals ~ year,data = df,log="y")
boxplot(residuals ~ year,data = df,ylim = c(-500,500))

breakYear <- 2011.5

df$prepost <- ifelse(df$year < breakYear,"pre", "post")

boxplot(residuals~prepost,data = df,ylim = c(-500,500))
table(df$prepost)
