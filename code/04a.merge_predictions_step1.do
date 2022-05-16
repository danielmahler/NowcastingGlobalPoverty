********************
*** INTRODUCTION ***
********************
/*
This .do-file merges all the direct predictions. 
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

**********************************************
*** MERGE PREDICTINGS FROM VARIOUS METHODS ***
**********************************************
use "03.intermediatedata\Predictions\status_quo_national_accounts_and_perfect.dta", clear
merge 1:1 code year datatype using "03.intermediatedata\Predictions\ciforest.dta", nogen
merge 1:1 code year datatype using "03.intermediatedata\Predictions\ciforest_lag.dta", nogen
merge 1:1 code year datatype using "03.intermediatedata\Predictions\ciforest_ripo.dta", nogen
merge 1:1 code year datatype using "03.intermediatedata\Predictions\cartforest.dta", nogen
merge 1:1 code year datatype using "03.intermediatedata\Predictions\cartforest_ripo.dta", nogen 
merge 1:1 code year datatype using "03.intermediatedata\Predictions\grbo.dta", nogen
merge 1:1 code year datatype using "03.intermediatedata\Predictions\grbo_ripo.dta", nogen
merge 1:1 code year datatype using "03.intermediatedata\Predictions\lasso_postscript.dta", nogen
merge 1:1 code year datatype using "03.intermediatedata\Predictions\lasso_postscript_ripo.dta", nogen
merge 1:1 code year datatype using "03.intermediatedata\Predictions\WEO_vintage.dta", nogen

save "03.intermediatedata\Predictions\predictions_step1.dta", replace
