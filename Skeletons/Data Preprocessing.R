###############################
######## PRELIMINARIES ########
###############################

# install and load packages

packs <- c("tseries", "forecast", "reshape2", "urca")

Install_And_Load <- function(packages) {
  k <- packages[!(packages %in% installed.packages()[,"Package"])];
  if(length(k))
  {install.packages(k, repos='https://cran.rstudio.com/');}
  
  for(package_name in packages)
  {library(package_name,character.only=TRUE, quietly = TRUE);}
}

Install_And_Load(packs)

# set working directory and load data

setwd("E:/KIVAS/Daten")
data_SA_90_NeuroNet_raw <- read.csv2(paste(
  
  # If the data files are in a subdirectory of the R-files, run the next line.
  getwd()
  ,"data_SA90_NeuroNet.csv"
  , sep = "/")
  , header = T
  , check.names = F
  , sep = ";"
  , dec = ",")

data_SA_90_NeuroNet_pre <- data_SA_90_NeuroNet_raw



# plot time series

# min, max, usw summary

# missings??

# correct quarterly values

# save object for later use
setwd("E:/")
saveRDS(jausa, file = "jausa.rds")
head(jausa)
jausanames <- colnames(jausa)
saveRDS(jausanames, file = "jausanames.rds")
