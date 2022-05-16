********************
*** INTRODUCTION ***
********************
/*
Introduction: This .do-file creates a .dta population in millions.
              for each of the 218 economies the World Bank's operates with from 1981 to the nowcasting year.
			  The source is WDI for all economies but Taiwan where we use UN data.
Created by:   Daniel Gerszon Mahler
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

***********
*** WDI ***
***********
wbopendata, indicator(SP.POP.TOTL) long clear
drop if regionname=="Aggregates"
rename countrycode code
rename sp_pop_totl pop
keep code year pop
keep if year>=1981
replace pop = pop/10^6
format pop %6.2f
tempfile wdipop
save    `wdipop'

**********
*** UN ***
**********
// 2021
import excel "02.inputdata\Population\Population_UN.xlsx", sheet("MEDIUM VARIANT") cellrange(A17:CJ306) firstrow clear
keep Countrycode-Parentcode I
rename I CA
tempfile 2021
save    `2021'

// Until 2020
import excel "02.inputdata/Population/Population_UN.xlsx", sheet("ESTIMATES") cellrange(A17:BZ306) firstrow clear
merge 1:1 Countrycode using `2021', nogen
local yr = 1950
foreach var of varlist H-CA {
rename `var' unpop`yr'
local yr = `yr' + 1
}

keep if Type=="Country/Area"
drop Index Variant Notes Type Parentcode unpop1950-unpop1980
local yr = 1981
foreach var of varlist unpop1981-unpop2021 {
destring `var', replace
replace `var' = `var'/1000
label var `var' "Population in millions in `yr'"
local yr = `yr' + 1
}
// Get country codes
*ssc install kountry
qui kountry Countrycode, from(iso3n) to(iso3c)
rename _ISO3C_ code
rename Region uncountryname
*br uncountryname code if missing(code)
replace code="RUS" if uncountryname=="Russian Federation"
replace code="SRB" if uncountryname=="Serbia"
replace code="SDN" if uncountryname=="Sudan"
replace code="IMN" if uncountryname=="Isle of Man"
replace code="CHI" if uncountryname=="Channel Islands"
replace code="CUW" if uncountryname=="Curaçao"
replace code="MNP" if uncountryname=="Northern Mariana Islands"
replace code="SXM" if uncountryname=="Sint Maarten (Dutch part)"
replace code="MAF" if uncountryname=="Saint Martin (French part)"
// Three countries remaining, these are not in the 218 WB universe, so deleting them
drop if missing(code) 
drop Countrycode uncountryn

reshape long unpop, i(code) j(year)
label var code "ISO3 country code"
label var unpop "Population in millions (from UN)"
label var year "Year"
format unpop %6.2f
order *, alpha

// Removing non-Bank entities
preserve
use "02.inputdata/Class/CLASS.dta", clear
keep code
duplicates drop
tempfile class
save    `class'
restore
merge m:1 code using `class', nogen keep(3)

// Temporally saving the final datafile
tempfile unpop
save `unpop' 

******************************************
*** MERGING THE TWO POPULATION SOURCES ***
******************************************
use `wdipop',clear
merge 1:1 code year using `unpop', nogen
// merge=1: XKX not in UN data. 
// merge=2: TWN not in WDI data

****************************************
*** CREATING FINAL POPULATION SERIES ***
****************************************
// First use WDI if possible, then supplement with un
replace pop = unpop if missing(pop)
drop unpop
order code year pop
lab var pop "Population (in millions)"
save "02.inputdata\Population\Population.dta", replace
