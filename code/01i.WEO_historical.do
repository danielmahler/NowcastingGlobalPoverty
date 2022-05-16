********************
*** INTRODUCTION ***
********************
/*
This .do-file cleans all the historical WEO databases
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

******************************
*** PROCESS 1999-2003 DATA ***
******************************
// Load data
forvalues year=1999/2003 {
forvalues edition=1/2 {
if `year'!=2003 | `edition'==1 {
import delimited "02.inputdata\WEO\Historical\WEO_`year'_0`edition'.csv", varnames(1) clear
drop a*
keep series_code country v*
foreach var of varlist v* {
cap replace `var' = "" if `var'=="n.a."
cap destring `var', replace
rename `var' y`=substr("`: var label `var''",1,4)'
}
drop y197*
reshape long y, i(series_code country) j(year) 
destring y, replace
destring year, replace
gen code = substr(series_code,2,3)
gen series = substr(series_code,-6,6)
destring code, replace
kountry code, from(imfn) to(iso3c)
drop code series_code country
rename _ISO3C_ code
drop if missing(code)
rename y gdp
gen vintage = "`year'_0`edition'"
order vintage series code year gdp
cap append using `data19992003'
tempfile data19992003
save    `data19992003'
}
}
}

******************************
*** PROCESS 2003-2021 DATA ***
******************************
// Load data
forvalues year=2003/2021 {
forvalues edition=1/2 {
if "`year'_`edition'"!="2003_1" {
import excel "02.inputdata\WEO\Historical\WEO_`year'_0`edition'.xlsx", sheet(WEO_`year'_0`edition') firstrow clear
 // Keep relevant rows & columns
drop if missing(ISO)
drop if missing(WEOSubjectCode)
isid ISO
drop Country Subject Units Scale Estimates
cap drop B*
compress
// Change labelling of years
local yr=1980
foreach var of varlist G-Z A* {
cap rename `var' y`yr'
cap replace y`yr' = "" if inlist(y`yr',"n/a","--")
cap destring y`yr', replace
local yr = `yr'+1
}
reshape long y, i(ISO WEOSubjectCode) j(year)
rename y gdp
rename ISO code
rename WEOSubject series
gen vintage = "`year'_0`edition'"
order vintage series code year gdp
cap append using `data20032021'
tempfile data20032021
save    `data20032021'
}
}
}
append using `data19992003'

keep if inrange(year,1981,2021)
replace code = "XKX" if inlist(code,"KOS","UVK")
replace code = "PSE" if code=="WBG"
drop if inlist(code,"ANT","SCG")

// Merge on popoulation data
merge m:1 code year using "02.inputdata/Population/Population.dta", nogen keep(1 3) 
// Convert early estimates to per capita
replace gdp = gdp/pop if series=="NGDP_R"
replace gdp = .       if series=="NGDP_R" & missing(pop)
bysort vintage code (year): replace gdp = 1       if _n==1 & series=="NGDPRPPPPCPCH" & !missing(gdp)
bysort vintage code (year): replace gdp = 1       if _n==1 & series=="NGDPRPPPPCPCH"
bysort vintage code (year): replace gdp = 1       if !missing(gdp) & missing(gdp[_n-1]) & _n!=1 & series=="NGDPRPPPPCPCH"
bysort vintage code (year): replace gdp = gdp[_n-1]*(1+gdp/100) if !missing(gdp) & !missing(gdp[_n-1]) & _n!=1 & series=="NGDPRPPPPCPCH"
drop series pop
// Convert 2020 april esimates to levels
sort vintage code year
replace gdp = . if gdp==0
// Finalizing
reshape wide gdp, i(code year) j(vintage) string

label var code "Country code"
label var year "Year"
rename gdp* HISTWEO_gdp_l_*
// Something odd is going on with Timor Leste from the 2014 fall vintage
drop if code=="TLS"
save "02.inputdata\WEO\HISTWEO.dta", replace
