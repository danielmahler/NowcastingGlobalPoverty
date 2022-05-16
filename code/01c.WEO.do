********************
*** INTRODUCTION ***
********************
/*
This .do-file loads and keeps the relevant WEO variables.
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


********************
*** PROCESS DATA ***
********************
// Load data
import excel "02.inputdata\WEO\WEO_2021_04", sheet("WEO_2021_04") firstrow clear
// Keep relevant rows & columns
drop if missing(ISO)
isid ISO WEOSubjectCode
keep ISO WEOSubjectCode SubjectDescriptor Units K-AY
distinct WEOSubjectCode
sort ISO WEOSubjectCode
forvalues i=1/`r(ndistinct)' {
local text`i' = SubjectDescriptor[`i'] + " (" + lower(Units[`i']) + ")"
}
drop SubjectDescriptor Units
// Change labelling of years
local yr=1981
foreach var of varlist K-AY {
rename `var' y`yr'
replace y`yr' = "" if inlist(y`yr',"n/a","--")
destring y`yr', replace
local yr = `yr'+1
}
// Reshape year to long
reshape long y, i(ISO WEOSubjectCode) j(year)
rename y WEO_l_
// Reshape variable to wide
reshape wide WEO_l_, i(ISO year) j(WEOSubjectCode) string
local i = 1
// Fetch variable descriptors from above
order *, alpha
order ISO year
foreach var of varlist WEO* {
label var `var' "`text`i''"
local i = `i' + 1 
}
// Replace gdp with upper case letters
foreach var of varlist WEO* {
if strpos("`: var label `var''", "gdp") {
local varlabnew  `=subinstr("`: var label `var''","gdp","GDP",.)'
lab var `var' "`varlabnew'"
}
if strpos("`: var label `var''", "u.s.") {
local varlabnew  `=subinstr("`: var label `var''","u.s.","US",.)'
lab var `var' "`varlabnew'"
}
}

label var year "Year"
rename ISO code
label var code "Country code"
replace code = "XKX" if code=="UVK"
replace code = "PSE" if code=="WBG"
order code year


// Change employment variable so it becomes a rate.
replace WEO_l_LE = WEO_l_LE/WEO_l_LP*100 
lab var WEO_l_LE "Employment rate, percent of total population"
save "02.inputdata\WEO\WEO_2021_04.dta", replace
