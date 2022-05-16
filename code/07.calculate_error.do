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

***************************
*** CALCULATE R SQUARED ***
***************************
// Loading data with predictions
use "03.intermediatedata\Predictions\predictions_final.dta", clear
// For the nowcasting sample we cannot calculate errors (since there are no true estimates), so dropping these observations
drop if sample_now==1
// Dropping the first observation by code-datatype as all growth/changes predictions don't work for them as as we want the sample upon which the erorrs are summarized to be identical
bysort code datatype (year): drop if _n==1

foreach outcome in Y_l_head Y_c_head Y_l_mean Y_g_mean Y_l_medi Y_g_medi Y_l_gini Y_g_gini {
foreach var of varlist `outcome'_* {
gen se_`var' =    (`outcome'-`var')^2
qui sum `outcome' [aw=weight_co_g]    if !missing(`var')
gen st_`var'= (`outcome'-`r(mean)')^2 if !missing(`var')
}
}
keep code weight* se* st* 

// Finding R2
collapse (sum) st* se* [aw=weight_co_g]
gen n = _n
reshape long st se, i(n) j(method) string
drop n
replace method    = substr(method,4,.)
gen outcome       = substr(method,1,6)
gen conversion1   = substr(method,16,4)
gen conversion2   = substr(method,21,4)
gen sampleused    = substr(method,13,2)
replace method    = substr(method,8,4)
drop if method=="perf"
drop if strpos(outcome,"medi")
drop if strpos(method,"gni")
drop if strpos(method,"hfc")
drop if strpos(method,"squ")

gen r2 = 1-se/st

order outcome method conversion* sampleused

keep if sampleused=="co"
drop sampleused

compress
format r2 %3.2f
bysort outcome: sum r2, f

*ssc install stripplot
replace outcome = "Change in poverty rates" if outcome=="c_head"
replace outcome = "Growth in Gini" if outcome=="g_gini"
replace outcome = "Growth in mean" if outcome=="g_mean"

stripplot r2, over(outcome) ///
graphregion(color(white)) color("26 134 147") msymbol(O) msize(medlarge) ///
legend(off) ytitle("Target  variable") xsize(10) ysize(6)  ///
xtitle("Out-of-sample R^2")
graph export "05.figures\R2.png", width(2000) as(png) replace

sort outcome r2
*br


*****************************
*** CREATE LOSS FUNCTIONS ***
*****************************
clear
set maxvar 10000
// Loading data with predictions
use "03.intermediatedata\Predictions\predictions_final.dta", clear
// For the nowcasting sample we cannot calculate errors (since there are no true estimates), so dropping these observations
drop if sample_now==1

// Calculate loss functions and giving labels
foreach outcome in l_head c_head l_mean g_mean l_medi g_medi l_gini g_gini {
foreach var of varlist Y_`outcome'_* {
gen se_`var' =    (Y_`outcome'-`var')^2
gen ad_`var' = abs(Y_`outcome'-`var')
gen dv_`var' =     Y_`outcome'-`var'
bysort code datatype (year): gen oo_`var' = Y_`outcome'-Y_`outcome'[_n-1]>=0 & `var'-Y_`outcome'[_n-1]<0 if !missing(Y_`outcome') & !missing(Y_`outcome'[_n-1]) & !missing(`var') & "`outcome'"=="l_head"
bysort code datatype (year): gen op_`var' = Y_`outcome'-Y_`outcome'[_n-1]<=0 & `var'-Y_`outcome'[_n-1]>0 if !missing(Y_`outcome') & !missing(Y_`outcome'[_n-1]) & !missing(`var') & "`outcome'"=="l_head" 
lab var se_`var' "Squared error of `var'"
lab var ad_`var' "Absolute deviation of `var'"
lab var dv_`var' "Deviation of `var'"
lab var oo_`var' "Trend over-optimistically predicted with `var'"
lab var op_`var' "Trend over-pessimistically predicted with `var'"
}
}
format se* ad* dv* oo* op* %2.1f

// Save data just with all errors 
keep code datatype year sample* weight* PCN_extrapolationtime Y_l_head Y_c_head Y_g_mean Y_l_mean Y_g_medi Y_l_medi Y_g_gini Y_l_gini se* ad* dv* oo* op*

save "04.outputdata\Error_all.dta", replace


****************************************
*** COLLAPSING TO SUMMARY STATISTICS ***
****************************************
use "04.outputdata\Error_all.dta", clear
// Dropping the first observation by code-datatype as all growth/changes predictions don't work for them as as we want the sample upon which the erorrs are summarized to be identical
bysort code datatype (year): drop if _n==1
drop Y_l_head Y_c_head Y_g_mean Y_l_mean Y_g_medi Y_l_medi Y_g_gini Y_l_gini
// Finding weighted mean and median error by sample
foreach sample in co al ri po {
foreach type   in mean median {
preserve
keep if sample_`sample'_g==1
keep *_`sample'_*
collapse (`type') se_* ad_* dv_* oo_* op_* [aw=weight_`sample'_g]
gen type   = "`type'"
gen sample = "`sample'"
save    "04.outputdata\\`type'`sample'.dta", replace
restore
}
}
// Append other dataset
use "04.outputdata\meanal.dta", clear
append using "04.outputdata\medianal.dta"
erase "04.outputdata\meanal.dta"
erase "04.outputdata\medianal.dta"
foreach sample in co ri po {
append using "04.outputdata\mean`sample'.dta"
append using "04.outputdata\median`sample'.dta"
erase        "04.outputdata\mean`sample'.dta"
erase        "04.outputdata\median`sample'.dta"
}
order sample type

reshape long se ad dv oo op , i(sample type) j(method) string
gen outcome       = substr(method,4,6)
replace method    = substr(method,11,.)
gen conversion1   = substr(method,9,4)
gen conversion2   = substr(method,14,4)
gen sampleused    = substr(method,6,2)
replace method    = substr(method,1,4)
// Only keep errors over sample when this sample was used for the predictions_final
keep if sample==sampleused
drop sampleused
order outcome sample type method conversion*
compress

************************************************
*** LABELLING AND SAVING DATASET WITH ERRORS ***
************************************************
lab var outcome       "Outcome variable"
lab var sample        "The spells used (all, comparable, rich, poor)"
lab var type          "Mean or median error"
lab var conversion1   "How the predictions are converted to headcount rates"
lab var conversion2   "When used, how  the mean and Gini are predicted"
lab var method        "Prediction method"
lab var se            "Squared error"
lab var ad            "Absolute deviation "
lab var dv            "Deviation (true-prediction)"
lab var oo            "Share of trends incorrectly predicted decline"
lab var op            "Share of trends incorrectly predicted increase"

format se ad dv oo op %3.2f
save "04.outputdata\Error_collapsed.dta", replace


