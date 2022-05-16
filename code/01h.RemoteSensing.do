********************
*** INTRODUCTION ***
********************
/*
This .do-file loads and prepares the remote sensing data.
*/

*****************************
*** SET WORKING DIRECTORY ***
*****************************
if (lower("`c(username)'") == "wb514665") {
	cd "C:\Users\WB514665\OneDrive - WBG\Research\NowcastingGlobalPoverty"
}
else if (lower("`c(username)'") == "wb384996") {
	cd "c:\Users\wb384996\OneDrive - WBG\Papers\global_poverty_nowcasting"
}

*****************
*** LOAD DATA ***
*****************
import delimited "02.inputdata\RemoteSensing\Country_Dataset.csv", encoding(ISO-8859-2) clear
// Drop irrelevant columns
drop v1 objectid iso_a2  incomeg lendingc region wb_adm0_na  lights_source wb_adm0_co fid un_m49
// Rename vars
rename *precipitation* *precip*
rename *temperature* *temp*
rename * RS_l_*
rename RS_l_iso3 code
rename RS_l_year year
rename *stddev* *std*
rename *stddev* *std*
rename *mean* *mea*
compress 

******************
*** LABEL DATA ***
******************
lab var code "Country code"
lab var year "Year"
foreach var of varlist RS* {
// Outcome 
if strpos("`var'", "lights") {
local start "Nightlights"
}
if strpos("`var'", "temp") {
local start "Temperature"
}
if strpos("`var'", "preci") {
local start "Precipitation"
}
if strpos("`var'", "ndwi") {
local start "Normalized Difference Water Index"
}
if strpos("`var'", "ndsi") {
local start "Normalized Difference Snow Index"
}
if strpos("`var'", "ndvi") {
local start "Normalized Difference Vegetation Index"
}
if strpos("`var'", "evi") {
local start "Enhanced Vegetation Index"
}
// Temporal
if strpos("`var'", "_mea_") {
local mid " (temporal mean"
}
if strpos("`var'", "_sum_") {
local mid " (temporal sum"
}
if strpos("`var'", "_min_") {
local mid " (temporal min"
}
if strpos("`var'", "_max_") {
local mid " (temporal max"
}
if strpos("`var'", "_std_") {
local mid " (temporal standard dev."
}
// Spatial
if substr("`var'", -3, 3)=="mea" {
local end ", spatial mean)"
}
if substr("`var'", -3, 3)=="sum" {
local end ", spatial sum)"
}
if substr("`var'", -3, 3)=="min" {
local end ", spatial min)"
}
if substr("`var'", -3, 3)=="max" {
local end ", spatial max)"
}
if substr("`var'", -3, 3)=="std" {
local end ", spatial standard dev.)"
}
// Actual labelling
lab var `var' "`start'`mid'`end'"
}
lab var RS_l_cropland "Cropland"
lab var RS_l_impervious "Impervious surface"

save "02.inputdata\RemoteSensing\RemoteSensing.dta", replace 

/*
keep code
duplicates drop
tempfile rs
save    `rs'

use "C:\Users\WB514665\OneDrive - WBG\Research\NowcastingGlobalPoverty\02.inputdata\Class\Class_processed.dta", clear
keep economy code
duplicates drop

merge 1:1 code using `rs'