********************
*** INTRODUCTION ***
********************
/*
This .do-file loads and keeps the relevant WDI variables
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

**************************************
*** LOADING AND APPENDING WDI DATA ***
**************************************
// Get list of countries to loop over
use "02.inputdata\Class\CLASS.dta", clear
keep code
duplicates drop
// Taiwan not in WDI
drop if code=="TWN"
levelsof code
cap	erase        "02.inputdata\WDI\WDI_raw.dta"
foreach coun in `r(levels)' {
disp in red "`coun'"
wbopendata, country(`coun') clear 
keep countrycode indicator* yr1981-yr2020
if "`coun'"=="ABW" { 

	qui save         "02.inputdata\WDI\WDI_raw.dta", replace
	}
else {
	qui append using "02.inputdata\WDI\WDI_raw.dta"
	qui save         "02.inputdata\WDI\WDI_raw.dta", replace
	}
}


************************
*** DOUBLE RESHAPING ***
************************
use "02.inputdata\WDI\WDI_raw.dta", clear
drop indicatorname
reshape long yr, i(countrycode indicatorcode) j(year) // 5 minutes
rename yr WDI_l_
replace indicatorcode = subinstr(indicatorcode, ".","",.)
drop if missing(WDI_l_)
reshape wide WDI_l_, i(countrycode year) j(indicatorcode) string // 5 minutes

***************************
*** MERGE ON WDI LABELS ***
***************************
preserve
use         "02.inputdata\WDI\WDI_raw.dta", clear
replace indicatorcode = subinstr(indicatorcode, ".","",.)
keep indicator*
duplicates drop
qui count
forvalues vars=1/`r(N)' {
local var = indicatorcode[`vars']
local lab_`var' = indicatorname[`vars']
}
restore
foreach variable of varlist WDI_* {
local wdiname = subinstr("`variable'", "WDI_l_","",.)
label var `variable' "`lab_`wdiname''"
}
rename countrycode code
lab var year "Year"
compress
save "02.inputdata\WDI\WDI_processed.dta", replace