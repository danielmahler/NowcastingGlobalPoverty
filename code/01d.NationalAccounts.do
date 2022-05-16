********************
*** INTRODUCTION ***
********************
/*
This .do-file prepares the national accounts data.
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

use "02.inputdata\NationalAccounts\NationalAccounts.dta", clear

// Only keeping relevant variables (growth will be calculated betweeen survey spells)
drop *growth
// Only keeping relevant years
keep if inrange(year,1981,2021)

foreach var of varlist gdp* gni* hfce* {
rename `var' NA_l_`var'
}
rename *hfce* *hfc*

// Reformat labels
foreach var of varlist NA* {
if strpos("`: var label `var''", "usd2010") {
local varlabnew  `=subinstr("`: var label `var''","usd2010","2010 USD",.)'
lab var `var' "`varlabnew'"
}
if strpos("`: var label `var''", "ppp2017") {
local varlabnew  `=subinstr("`: var label `var''","ppp2017","2017 PPP",.)'
lab var `var' "`varlabnew'"
}
if strpos("`: var label `var''", "ppp2011") {
local varlabnew  `=subinstr("`: var label `var''","ppp2011","2011 PPP",.)'
lab var `var' "`varlabnew'"
}
if strpos("`: var label `var''", "lcu") {
local varlabnew  `=subinstr("`: var label `var''","lcu","LCU",.)'
lab var `var' "`varlabnew'"
}
if strpos("`: var label `var''", "HFCE") {
local varlabnew  `=subinstr("`: var label `var''","HFCE","Household final consumption expenditure",.)'
lab var `var' "`varlabnew'"
}
}
save "02.inputdata\NationalAccounts\NationalAccounts_processed.dta", replace