********************
*** INTRODUCTION ***
********************
/*
This .do-file calculates the errors and collapses them
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

*****************************************
*** COMPARE ERRORS WITH POVERTY RATES ***
*****************************************
use "04.outputdata\Error_collapsed.dta", clear

keep if type=="mean"
keep if conversion1=="dnmu"
keep if conversion2=="grow"
drop type conversion1 conversion2 dv
format ad %3.2f
drop if strpos(method,"fce")
drop if strpos(method,"squ")
drop if strpos(method,"gni")
drop if strpos(method,"hfc")
sort sample ad

*********************************
*** CALCULATE ERRORS BY GMEAN ***
*********************************
// Loading data with predictions
use "03.intermediatedata\Predictions\predictions_final.dta", clear
// For the nowcasting sample we cannot calculate errors (since there are no true estimates), so dropping these observations
drop if sample_now==1
// Dropping the first observation by code-datatype as all growth/changes predictions don't work for them as as we want the sample upon which the erorrs are summarized to be identical
bysort code datatype (year): drop if _n==1

// Calculate loss functions and giving labels
foreach var of varlist Y_g_mean_* {
gen ad_`var' = abs(Y_g_mean-`var')
lab var ad_`var' "Absolute deviation of `var'"
}
format ad* %2.1f
// Keep relevant variables
keep code datatype year sample* weight* ad*
// Finding weighted mean and median error by sample
foreach sample in co al {
preserve
keep if sample_`sample'_g==1
collapse (mean) ad_* [aw=weight_`sample'_g]
gen sample = "`sample'"
save    "04.outputdata\mean`sample'_gmean.dta", replace
restore
}
// Append other dataset
use "04.outputdata\meanal_gmean.dta", clear
append using "04.outputdata\meanco_gmean.dta"
erase "04.outputdata\meanal_gmean.dta"
erase "04.outputdata\meanco_gmean.dta"
order sample

reshape long ad, i(sample) j(method) string
replace method    = substr(method,11,.)
gen conversion1   = substr(method,9,4)
gen conversion2   = substr(method,14,4)
gen sampleused    = substr(method,6,2)
replace method    = substr(method,1,4)
// Only keep errors over sample when this sample was used for the predictions_final
keep if sample==sampleused
drop sampleused conversion*
order sample method 
replace ad = 100*ad
format ad %3.2f
compress
drop if strpos(method,"fce")
drop if strpos(method,"squ")
drop if strpos(method,"gni")
drop if strpos(method,"hfc")
sort sample ad
