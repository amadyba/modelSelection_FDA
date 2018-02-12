library(dplyr)
library(tidyr)
library(funFEM)

# Data from https://data.worldbank.org/indicator/EN.ATM.CO2E.PC
co2 <- read.csv("./co2_countries.csv")

# Remove unwanted columns
co2.vars <- names(co2) %in% c("Indicator.Name", "Indicator.Code", "Country.Name", "X")
co2 <- co2[!co2.vars]

# Transpose the data (using t() changes the vars to factors)
df = co2 %>%
  gather(var, val, 2:ncol(co2)) %>%
  spread(names(co2)[1], val) %>% as.data.frame(.)
rownames(df) = df[,1]

# Remove the final rows (empty) and the first column (years)
n = 55 # Number of years for which we have data
df <- df[0:n, -c(1)] 
df <- df[,!names(df) %in% c("CHN", "USA", "MEA")]
  
# Convert to matrix, we'll need it like that
data = as.matrix(df)
plot.ts(data[,c(1:5)])

years = 0.5:n
names(years) = 1960:2014

# Clustering the well-known "Canadian temperature" data (Ramsay & Silverman)
basis <- create.bspline.basis(c(0, n), nbasis=21, norder=4)
fdobj <- smooth.basis(years, data, basis,
                      fdnames=list("Year", "Country", "CO2 (t)"))$fd

par(mfrow=c(1,1))
plot(fdobj,lwd=2,lty=1)

res = funFEM(fdobj, K = 2:20, model="all", disp=TRUE, crit="bic")

# Visualization of the partition and the group means
par(mfrow=c(1,2))
plot(fdobj,col=res$cls,lwd=2,lty=1)
fdmeans = fdobj; fdmeans$coefs = t(res$prms$my)
plot(fdmeans,col=1:max(res$cls),lwd=2)


#Normalized version
#---------------------------
data.norm = scale(data, center=TRUE, scale=colSums(data))


fdobj.norm <- smooth.basis(years, data.norm, basis,
                      fdnames=list("Year", "Country", "CO2 (t)"))$fd

par(mfrow=c(1,1))
plot(fdobj.norm,lwd=2,lty=1)

res.norm = funFEM(fdobj.norm, K = 2:20, model="all", disp=TRUE, crit="bic")

# Visualization of the partition and the group means
par(mfrow=c(1,2))
plot(fdobj.norm, col=res.norm$cls, lwd=2, lty=1)
fdmeans.norm = fdobj.norm; 
fdmeans.norm$coefs = t(res.norm$prms$my)
plot(fdmeans.norm, col=1:max(res$cls), lwd=2)


