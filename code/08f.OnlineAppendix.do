********************
*** INTRODUCTION ***
********************
/*
This .do-file creates the figures and table for the online appendix
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

************************************************
*** PLOT RATIOS AS A FUNCTION OF MEAN INCOME ***
************************************************
pip, country(all) year(all) clear
tab survey_coverage, nolab
keep if survey_coverage==3
bysort country_code survey_year: keep if _N==2
keep country_code countryname survey_year welfare_type *comp* mean
bysort country_code survey_year (welfare_type): gen ratio = mean[2]/mean[1]
bysort country_code: drop if _N<=6
sort country_code welfare_type survey_year
*br // Manually dropping the non-comparable parts
drop if country_code=="PHL" & survey_year==2000
bysort country_code survey_year (welfare_type): keep if _n==2

format mean %3.0f
twoway scatter ratio mean, mcolor("26 134 147") by(countryname, note("") graphregion(color(white))) ///
graphregion(color(white)) ytitle("Ratio of mean income to mean consumption") subtitle(,fcolor(white) nobox) ///
ylab(,angle(horizontal)) xsize(15) ysize(10) xtitle("Mean income (monthly 2011 USD PPP)")
graph export "05.figures\Income_to_consumption.png", width(2000) as(png) replace
graph export "06.text\Figures\Income_to_consumption.png", width(2000) as(png) replace

*****************************
*** COUNTRY TREND FIGURES ***
*****************************

local favorite   dgdp_al_dnmu_grow
local best       cirf_al_dnmu_grow
local bestdirect grbo_al_dire

use "02.inputdata\Class\CLASS_processed.dta", clear
keep economy code
duplicates drop
tempfile countryname
save    `countryname'

use "02.inputdata\Population\Population.dta", clear
keep if inrange(year,2000,2021)
expand 3
bysort code year: gen datatype = _n if _n!=3
tempfile pop
save    `pop'

use "03.intermediatedata\Predictions\predictions_final.dta", clear
keep if inrange(year,2000,2021)
keep code year datatype Y_l_head_`favorite' Y_l_head Y_l_head_`best' Y_l_head_`bestdirect'
rename Y_l_head_* _*
rename Y_l_head true

merge 1:1 code year datatype using `pop', nogen
merge m:1 code using `countryname', nogen
sort code datatype year
bysort code datatype (year): replace pop = pop[_n-1]*(year[_n+1]-year)+ pop[_n+1]*(year-year[_n-1]) if missing(pop)
keep if !missing(_`bestdirect')

foreach var of varlist true _* {
gen poor`var' = `var'/100*pop
bysort code: egen maxpoor`var' = max(poor`var')
}
egen max = rowmax(maxpoor*)
drop poor* maxpoor*

// Only keep 80 countries with most recorded poor
preserve
keep code max
duplicates drop
gsort -max
keep if _n<=80
tempfile max80
save    `max80'
restore
merge m:1 code using `max80', nogen keep(3)

order code economy
compress

foreach var of varlist _* {
replace `var' = . if !missing(true)
}
format _* true %2.0f

egen codenr = group(economy)
gen plotnr = round(codenr+9.99,20)/20

levelsof plotnr

foreach nr in `r(levels)' {

preserve

keep if plotnr == `nr'

twoway scatter true          year, color(black) msymbol(x) || ///
       line    _`favorite'   year, color("26 134 147") || ///
       line    _`best'       year, color("255 100 51") || ///
	   line    _`bestdirect' year, color("117 26 51")     ///
		by(economy, rows(5) note("") ///
iscale(*1.2) graphregion(color(white)))  ///
xtitle("") ylab(0(20)95,angle(horizontal)) xlab(2005(5)2021, grid) ytitle("Poverty rate (%)", size(small)) ///
subtitle(,fcolor(white) nobox) xsize(12) ysize(15) plotregion(margin(0 0 0 0)) ///
legend(order (1 "Survey estimates" 2 "GDP growth w. inc/con passthrough rate" 3 "Best model overall" 4 "Best model predicting poverty rates directly") rows(2) region(lcolor(white)) size(vsmall) span symxsize(*0.5)) ///
name(plot`nr', replace)
graph export "05.figures\CountryPovertyTrend_`nr'.png", width(2000) as(png) replace
graph export "06.text\Figures\CountryPovertyTrend_`nr'.png", width(2000) as(png) replace
restore
}

****************************
*** RANGE OF YEARS TABLE ***
****************************
use "02.inputdata\FinalInputData.dta", clear
keep if sample_al_l == 1
keep economy year 
replace year = round(year)
bysort economy: egen firstyear=min(year)
bysort economy: egen lastyear =max(year)
bysort economy: gen numbobs = _N
drop year
duplicates drop

isid make
encode make, gen(Make)
eststo clear
estpost tabstat firstyear lastyear numbobs, by(economy)
esttab, cells("firstyear(label(First year)) lastyear(label(Last year))  numbobs(label(Total obs.))") ///
noobs nomtitle nonumber varlabels(`e(labels)') varwidth(20) drop(Total) tex

**********************************
*** DATA FILE WITH PROJECTIONS ***
**********************************
use "03.intermediatedata\Predictions\predictions_final.dta", clear
drop CLASS PCN
// Drop medians
drop *medi* *lnmd*
drop *squ*
export delimited using "04.outputdata\Predictions.csv", replace

*****************************
*** DATA FILE WITH ERRORS ***
*****************************
set maxvar 10000
use "04.outputdata\Error_all.dta", clear
drop Y_l_head PCN
drop *squ*
// Drop median
drop *medi* *lnmd*
export delimited using "04.outputdata\Errors.csv", replace