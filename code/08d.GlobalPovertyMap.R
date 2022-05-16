#####################################
### INSTALLING NECESSARY PACKAGES ###
#####################################
#install.packages("haven")
#install.packages("rgdal")
#install.packages("ggplot2")
{
suppressMessages(library(rgdal))
suppressMessages(require("plyr"))
library(scales)
library(haven)
library(haven)
library(ggplot2)
}

setwd("C:/Users/WB514665/OneDrive - WBG/Research/NowcastingGlobalPoverty")
rm(list=ls())


#################
### LOAD DATA ###
#################
# Load mobility data
poverty = read_dta("04.outputdata/NowcastedPoverty.dta")
# Load shapefile
load(file = "02.inputdata/Maps/WorldShapefile.rda")
# Merge
names(poverty)[names(poverty)=="code"] <- "ISO_CODES"
mergedfile <- join(poverty,WorldShapefile,by="ISO_CODES", type="full")

############################
### CREATE COLOR SCHEMES ###
############################
LOW  = "#1A8693"
HIGH = "#751A33"
MID  = "#FF6433"

################
### PLOT MAP ###
################
  map = ggplot() +
  geom_polygon(data = mergedfile, aes(x = long, y = lat, group = group, fill=rate_dgdp_al_dnmu_grow), color = 'white',size=0.1) + 
    scale_fill_gradient2(low = LOW, mid = MID,high = HIGH, midpoint=50, oob=squish, name="Poverty rate (%)") +
    theme(panel.background = element_rect(fill='white', color='white')) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))
  plot(map)
  ggsave(file="05.figures/GlobalPovertyMap.jpg", width=12,height=6,units="in",dpi=1000)
  ggsave(file="06.text/Figures/GlobalPovertyMap.jpg", width=12,height=6,units="in",dpi=1000)
  