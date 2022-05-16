********************
*** INTRODUCTION ***
********************
/*
This .do-file loads and prepares the class file
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

****************************
*** LOADING THE RAW FILE ***
****************************
// Downloaded from here
// https://github.com/PovcalNet-Team/Class/tree/master/OutputData
use "02.inputdata\Class\CLASS.dta", clear
drop *current
// Adjust such that FY data are put in the second year
replace year = year + 1
// Extend backwards to 1981
expand 9 if year==1989
bysort code (year): replace year = _n+1980 if year==1989
replace incgroup_historical = "" if year<1989
// Turn into numerics
foreach var of varlist region* ssasubregion ida fcv incgroup {
rename `var' `var'_string
encode `var'_string, gen(`var')
drop `var'_string
}
rename ssasubregion region_ssasub
rename ida_historical ida
rename fcv_historical fcv
rename incgroup_historical incgroup

foreach var of varlist region* ida fcv incgroup {
rename `var' CLASS_`var'
}

compress

lab var CLASS_region_povcalnet "PovcalNet geographical regions"
lab var CLASS_region "World Bank geographical regions"
lab var CLASS_ida "Lending group (IDA/IBRD/Rest of the world)"
lab var CLASS_fcv "Country suffering from fragility, conflict or violence"
lab var CLASS_incgroup "Income group"
save "02.inputdata\Class\CLASS_processed.dta", replace