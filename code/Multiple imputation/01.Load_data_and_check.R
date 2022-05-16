# ==================================================
# Project:       Nowcast poverty using ML
# Author:        Andres Castaneda 
# Dependencies:  The World Bank
# -----------------------------------------------------
# Creation Date:    Aug 2018
# Modification Date:   
# Do-file version:    01
# References:          
# Output:             xlsx and dta
# ==================================================

#-------------------------------------
# SET UP
#-------------------------------------

# set current directory
getwd()
inputdir <- file.path(getwd(), "input")
setwd(inputdir) # woring directory

#-------------------------------------
# Load packages
#-------------------------------------

pkgs = c("reshape2", "Amelia", "snow", "Zelig", "boot", "caret", "glmnet", 
        "data.table", "utils", "useful", "parallel", "tidyverse") # package names

#inst = lapply(pkgs, function(x) {if (!require(x)) install.packages(x)}) # check to install
load = lapply(pkgs, library, character.only = TRUE) # load them

install.packages("MAMI", repos=c("http://R-Forge.R-project.org",
                                 "http://cran.at.r-project.org"), dependencies=TRUE)
library(MAMI)


 
# if (!require("tidyr")) install.packages("tidyr")
# if (!require("reshape2")) install.packages("reshape2")
# if (!require("plyr")) install.packages("plyr")
# if (!require("Amelia")) install.packages("Amelia")
# if (!require("snow")) install.packages("snow")
# if (!require("Zelig")) install.packages("Zelig")
# if (!require("ISLR")) install.packages("ISLR")
# if (!require("boot")) install.packages("boot")
 
# library(utils)
# library(tidyr)
# library(reshape2)
# library(plyr)
# library(dplyr)
# library(readr)
# library(Amelia)
# require(parallel)
# library(snow)
# library(tidyverse)

###################################
library(caret)
library(glmnet)
library(data.table)
require(boot)
library(useful)
library(MAMI)
citation("MAMI")
print(citation("MAMI"), bibtex=T)


#---------------------------
# Load data
#---------------------------


WDI_wide <- read_csv("WDIData_wide.csv")
str(WDI_wide)


# Convert to long shape 
WDI_long <- melt(WDI_wide, 
                     variable.name = "year",
                     value.names = "value",
                     id.vars = c("countrycode", "Ind_code"))

#convert to wide back again where indicators are variables
tempy <- colsplit(WDI_long[, 3], "", c("y", "year"))
WDI_long <- cbind(WDI_long[1:2], 
                  tempy[2],
                  WDI_long[4])
rm(tempy)
str(WDI_long)

WDI <- dcast(WDI_long, 
             formula = countrycode + year ~ Ind_code,
             value.var = "value")

WDI <- WDI[which(WDI$year >= 1995), ]


##------ RUN stata Code 

# load gmd poverty and inequality results

GMD <- read_csv("gmd_PovIne.csv")
str(GMD)

## Join GMD and WDI

wdigmd <- join(WDI, GMD, by = c("countrycode", "year"))

