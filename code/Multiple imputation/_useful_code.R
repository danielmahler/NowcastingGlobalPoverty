

###########################################################################################

## replace imputed values in FGT and ineq vars after last data point availabe (do NOT delete)

#----
# max year without missing data in fgt0_190
maxGMD.year <- aggregate(year ~ countrycode ,
                         data = filter(GMD, !is.na(fgt0_190)), max)

names(maxGMD.year)[2] <- "max.year"
mGMD <- join(maxGMD.year, GMD, by = "countrycode")

for (nm in names(mGMD)) {
  if (grepl("fgt|values", nm)) {  ## only FGT and ineq variables
    #print(names(mGMD[nm]))
    mGMD[(mGMD$year > mGMD$max.year),nm] <- NA  ## replace imputations by missing values
  }
}
#----



# LM formula
#-------  
x <- wdivars %>% summarise(vars = paste(vars, collapse = " + "))
form <- as.formula(paste("fgt0_190 ~", x))
summary(lm(form, 
           data = wdigmd_sample))
#-------


# Manipulate variables
#---------
for (n in wdivars) {
  v = get(n)
  print(mean(v, na.rm = TRUE))
}

for (n in c("x", "y", "z")) {
  v = get(n)
  v = v**2
  assign(n, v)
}
#-----------



# create functions
#--------
first_and_last <- function(name) {
  name <- gsub(" ", "", name)
  letters <- strsplit(name, split = "")[[1]]
  c(first = min(letters), last = max(letters))
}

first_and_last("New York")

name <- gsub(" ", "", "New York")
name
letters <- strsplit(name, split = "")[[1]]
letters
#--------


# Error and debugger functions
#--------
options(error = browser)
options(error = NULL)
browser() # enter debugger mode
debugonce() #which function we want to see
debug()
#-------


# Installing Rtools
#----------

install.packages("installr")
library(installr)
install.Rtools()
#------------------

update.packages(ask = FALSE)


