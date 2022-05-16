********************
*** INTRODUCTION ***
********************
/*
This .do-file calculates global/regional/country-level poverty using all methods
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
*** GLOBAL POVERTY TREND ***
****************************

local favorite   dgdp_al_dnmu_grow
local best       plas_al_dnmu_grow
local bestdirect grbo_al_dire

use "02.inputdata\Population\Population.dta", replace
keep if inrange(year,2000,2021)
tempfile pop
save    `pop'

use "02.inputdata/Class/CLASS_processed.dta", replace
keep code CLASS_region
duplicates drop
tempfile region
save    `region'

use "03.intermediatedata\Predictions\predictions_final.dta", clear
keep if inrange(year,2000,2021)
keep if sample_now == 1
replace Y_l_head_`favorite' = Y_l_head_`bestdirect' if missing(Y_l_head_`favorite')
replace Y_l_head_`best'     = Y_l_head_`bestdirect' if missing(Y_l_head_`best')
keep code year Y_l_head_`favorite' Y_l_head_`best' Y_l_head_`bestdirect'
rename Y_l_head_* _*
tempfile nowcasts
save    `nowcasts'

povcalnet, country(all) year(2014 2015 2016 2017 2018 2019) fillgaps clear
rename countrycode code
keep if inlist(coveragetype,3,4) | inlist(code,"ARG","SUR")
isid code year
keep code year headcount

merge 1:1 code year using `pop',      nogen
merge 1:1 code year using `nowcasts', nogen
merge m:1 code      using `region',   nogen
sort code year
foreach var of varlist _* {
replace `var' = headcount*100 if missing(`var')
gen poor`var' = `var'/100*pop
}
drop headcount
rename _* rate_*
format poor* %4.1f 

order code year CLASS
compress

// Save 2021 data for map
preserve
keep if year==2021
save "04.outputdata\NowcastedPoverty.dta", replace
gsort -rate_dgdp_al_dnmu_grow
list code CLASS rate_dgdp if _n<25
list code rate_dgdp if _n<50 & CLASS!=7
restore

preserve
collapse (mean) rate* (rawsum) poor* [aw=pop], by(year)
gen CLASS_region = 0
tempfile world
save    `world'
restore 
collapse (mean) rate* (rawsum) poor* [aw=pop], by(year CLASS)
append using `world'

label define region 0 "World", add

twoway connected rate_`favorite' year if year>=2014, color("26 134 147") msymbol(O) || ///
       connected rate_`best' year if year>=2014, color("255 100 51") msymbol(T) || ///
	   connected rate_`bestdirect'      year if year>=2014, color("117 26 51")  msymbol(X)    ///
		by(CLASS, rows(2) yrescale note("") ///
iscale(*1.2) graphregion(color(white)))  ///
xtitle("") ylab(,angle(horizontal)) xlab(2015(2)2021) ytitle("Poverty rate (%)") ///
subtitle(,fcolor(white) nobox) xsize(10) ysize(5) ///
legend(order (1 "GDP growth w. inc/con passthrough rate" 2 "Best model overall" 3 "Best model predicting poverty rates directly") rows(1) region(lcolor(white)) span symxsize(*0.5))
graph export "05.figures\GlobalPovertyTrend.png", width(2000) as(png) replace
graph export "06.text\Figures\GlobalPovertyTrend.png", width(2000) as(png) replace

// Slides version
drop if inlist(CLASS,2,5)
twoway connected rate_dgdp_al_dnmu_grow year if year>=2014, color("26 134 147") msymbol(O) || ///
       connected rate_cirf_al_dnmu_grow year if year>=2014, color("255 100 51") msymbol(T) || ///
	   connected rate_cirf_al_dire      year if year>=2014, color("117 26 51")  msymbol(X)    ///
		by(CLASS, rows(2) yrescale note("") ///
iscale(*1.2) graphregion(color(white)))  ///
xtitle("") ylab(,angle(horizontal)) xlab(2015(2)2021) ytitle("Poverty rate (%)") ///
subtitle(,fcolor(white) nobox) xsize(10) ysize(6) ///
legend(order (1 "GDP growth w. passthrough rate" 2 "Best model" 3 "Best model predicting poverty rates") rows(1) region(lcolor(white)) span symxsize(*0.5))
graph export "05.figures\GlobalPovertyTrend_Slides.png", width(2000) as(png) replace
