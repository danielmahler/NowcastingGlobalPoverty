********************
*** INTRODUCTION ***
********************
/*
This .do-file generates status quo predictions, predictions from using other national accounts rules and 'perfect' mean predictions, i.e. predictions that are identical to the true values. The latter will be helpful to look at how well our poverty predictions are if we are able to perfectly predict the mean.
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

***********************
*** LOAD INPUT DATA *** 
***********************
use "02.inputdata\FinalInputData.dta", clear
// Drop irrelevant variables
drop WDI* RS* WEO* HISTWEO_gdp_l* *gni* *hfc* NA_l*
// Drop irrelevant rows
drop if sample_now==1

**************************************************
*** ASSIGN GROWTH RATES BASED ON DATA VINTAGES ***
**************************************************
ren *_01 *25
ren *_02 *75
// A bit complicated because of decimal years
forvalues casting=5/15 {
forvalues edition=1/2 {
gen gdp_t`casting'_`edition' = .
forvalues year=1999.25(0.5)2021.75 { 
replace gdp_t`casting'_`edition' = HISTWEO_gdp_g_`=`year'*100' if inrange(year+(`edition'-1)/2+`casting'-10,`year'-0.25,`year'+0.24)
}
}
}
drop HISTWEO*
// Only 290 observations with non-missing everywhere. Partially because of lack of forecasts which we don't need
cor *gdp*
//
mdesc gdp*
drop gdp_t5* gdp_t6* gdp_t7* gdp_t8*
cor *gdp*
// Now 1138 observations
mdesc gdp*
// Prob 5 year after nowcasting year is not needed
drop gdp_t15*
// Now 1210 observations
// Flagging rows with any missing
gen missing = 0
foreach var of varlist *gdp* {
replace missing = 1 if missing(`var')
}

*********************************************************************
*** NATIONAL ACCOUNTS WITH PASSTHROUGH RATES SEPARATE BY DATATYPE ***
*********************************************************************
gen outerfold = runiformint(1,5)
// Create interaction variables
cls
gen NA_g_gdp_datatype = NA_g_gdp*(datatype-1)
foreach var of varlist gdp* {
foreach type    in /*co*/ al {
gen Y_g_mean_`var'_`type'_dire = .
forvalues outerfold=1/5 {
reg Y_g_mean  NA_g_gdp NA_g_gdp_datatype if outerfold!=`outerfold' & missing==0 [aw=weight_`type'_g], nocons
ren NA_g_gdp          temp_NA_g_gdp
ren NA_g_gdp_datatype temp_NA_g_gdp_datatype
gen NA_g_gdp          = `var' 
gen NA_g_gdp_datatype = NA_g_gdp*(datatype-1)
predict temp
replace Y_g_mean_`var'_`type'_dire = temp if outerfold==`outerfold' & sample_now==0 & missing==0
drop temp NA_g_gdp NA_g_gdp_datatype
ren temp_NA_g_gdp NA_g_gdp
ren temp_NA_g_gdp_datatype NA_g_gdp_datatype
}

// Works for the first extrapolation observation
bysort code datatype (year): gen Y_l_mean_`var'_`type'_grow = Y_l_mean[_n-1]*(1+Y_g_mean_`var'_`type'_dire)^PCN_extrapolationtime
// Works for the other extrapolation observations
bysort code datatype (year): replace Y_l_mean_`var'_`type'_grow = Y_l_mean_`var'_`type'_grow[_n-1]*(1+Y_g_mean_`var'_`type'_dire)^PCN_extrapolationtime if missing(Y_l_mean_`var'_`type'_grow)
}
}

drop gdp*
ren *gdp_t* *v*
ren *_1* *1*
ren *_2* *2*
ren *v9* *v09*

keep code year datatype sample* weight* Y* PCN_extrapolationtime CLASS_region_povcalnet
order code datatype year
sort  code datatype year 
compress

save "03.intermediatedata\Predictions\WEO_vintage.dta", replace