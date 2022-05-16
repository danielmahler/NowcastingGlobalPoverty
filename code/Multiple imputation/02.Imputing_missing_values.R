#------------------------------------
# Prepare for multiple imputation
#------------------------------------

#create smaller dataset for testing
sample.size <- 1
randrows <- sample(nrow(wdigmd), round(nrow(wdigmd)*sample.size))
wdigmd_sample <- wdigmd[randrows,]

a <- length(wdigmd_sample) - 13  # GMD variables

 
# randcols <- sample(3:1500, 20)
# wdigmd_sample <- cbind(wdigmd_sample[1:2],                     # id
#                        wdigmd_sample[randcols],                # rand columns
#                        wdigmd_sample[a:length(wdigmd_sample)]) # GMD variables 
# 
# wdigmd_sample <- wdigmd_sample %>% 
#   filter(region == "LAC")
 

## Impute missing values using Amelia Techniques

wdivars <- as.data.frame(grep("\\.", names(wdigmd_sample), 
                              value = TRUE))
names(wdivars) <- "vars"

countryvar <- c("countrycode", "countryname", "region")
countryinfo <- wdigmd_sample[countryvar]

# identify number of no-missing values in each variable. 
#n.na <- apply(wdigmd_sample, 2, function(x) length(which(!is.na(x))))
n.na2 <- wdigmd_sample %>% 
  map_dbl(~length(which(!is.na(.x)))) # No. of obs in each variable that is !na



sdofvars <- wdigmd_sample %>% 
  map_dbl(~sd(.x, na.rm = TRUE))


#get name of variables with only missing values
drops <- names(n.na2[n.na2 %>% map_lgl(~.x <= 1)]) 
drops <- cbind(drops, names(sdofvars[sdofvars %>% map_lgl(~.x == 0)]))

  

drops <- c("countryname","region", drops)
wdigmd_sample <- wdigmd_sample[ , !(names(wdigmd_sample) %in% drops)] # remove variables in drop

names(wdigmd_sample)

#----------------------------------------------------------
# multiple imputation of missing values in covariates
#----------------------------------------------------------


sysinfo <- as.data.frame(Sys.info())
if (sysinfo["nodename",] == "WBGMSBDAT001") {
  b = 0
} else if (sysinfo["nodename",] == "WBGMSBDAT002") {
  b = 5
} else {
  b = 10
}
print(b)

for (i in 1:5) {
  b <- b + i
  a.out <- amelia(wdigmd_sample, m = 1, 
                  ts = "year", polytime = 2,
                  cs = "countrycode", intercs = TRUE,
                  parallel = "snow", 
                  p2s = 2, 
                  empri = round(.02*nrow(wdigmd_sample))) # ridge prior for numeverial stability
  
  
  seq.name   <- paste("wdi_MI_amelia",b,sep = "")
  RData.name <- paste(seq.name,".RData", sep = "")
  save(a.out, file = RData.name)
  write.amelia(obj=a.out, file.stem = seq.name, format = "dta")
}


#Basic Analsys of Amelia results
compare.density(a.out, var = "VC.PKP.TOTL.UN")
overimpute(a.out, var = "VC.PKP.TOTL.UN")
missmap(a.out)
plot(a.out)

# Back up Amelia results
a.backup <- a.out

a.out <- a.backup

str(a.out)

# max year without missing data in fgt0_190
# way 1
maxGMD.year <- aggregate(year ~ countrycode ,
                         data = filter(GMD, !is.na(fgt0_190)), max)

# way 2. Tidyverse
maxGMD.year <- GMD %>% 
  filter(!is.na(fgt0_190)) %>%  # for no missing in fgt
  group_by(countrycode) %>%     # By countrycode
  summarize(maxYear = max(year))   # max year

## replace imputed values in FGT and ineq vars after last 
# data point availabe (do NOT delete)

depVars <- grep("fgt|values", a.out$orig.vars, value = TRUE) # names of variables
for (n in 1:a.out$m) {
  
  a.out$imputations[[n]] <- join(maxGMD.year, 
                                 a.out$imputations[[n]], 
                                 by = "countrycode")
  
  temp <- a.out$imputations[[n]] # temporal variable for ease of use 
  for (nm in depVars) {
      a.out$imputations[[n]][, nm] <- 
                  ifelse( !is.na(temp[,nm]) &    # no missing values 
                      temp$year > temp$maxYear,  # year > than max year with available data
                      NA, temp[,nm] )  # replace imputations by missing values
  }
}

save(a.out, file = "Missing_imputations.RData")
write.amelia(obj=a.out, file.stem = "Missing_imputations")

