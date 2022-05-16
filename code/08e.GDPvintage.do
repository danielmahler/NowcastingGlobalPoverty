********************
*** INTRODUCTION ***
********************
/*
This .do-file compares the errors by GDP vintage
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

************************
*** COLLAPSED ERRORS ***
************************
use "04.outputdata\Error_collapsed.dta", clear
keep if inlist(outcome,"g_mean","l_head")
keep if type=="mean"
keep if conversion1=="dnmu" | outcome=="g_mean"
keep if strpos(method,"v")
drop type conversion* dv oo op se
replace ad = 100*ad if outcome=="g_mean"
gen vintage=substr(method,2,3)
replace vintage=substr(vintage,1,2)+".25" if substr(vintage,3,1)=="1"
replace vintage=substr(vintage,1,2)+".75" if substr(vintage,3,1)=="2"
destring vintage, replace
replace vintage = (vintage-10)
// Figure
format ad* %2.1f
twoway connected ad vintage if sample=="al" & outcome=="l_head", sort lwidth(medthick)  color("26 134 147") || ///
pcarrowi 3.5 0.25 3.4 0.25 (12) "Apr", color("117 26 51") mlabsize(medium) mlabcolor("117 26 51") || ///
pcarrowi 3.5 0.75 3.4 0.75 (12) "Sep", color("117 26 51") mlabsize(medium)  mlabcolor("117 26 51") graphregion(color(white))  ///
ylab(2.5(0.5)4,angle(horizontal) labsize(medlarge)) ytitle("Mean absolute deviation (pct. points)", size(medlarge)) ///
xlab(-0.5 "t-1" 0.5 "t" 1.5 "t+1" 2.5 "t+2" 3.5 "t+3" 4.5 "t+4", labsize(medlarge)) xscale(range(-1 5)) ///
xline(0, lcolor(black) lpattern(shortdash)) xline(1,lcolor(black) lpattern(shortdash)) ///
xline(2,lcolor(black) lpattern(shortdash)) xtitle("Release year of growth data used to predict poverty in year t") ///
text(2.75 -0.9 "Forecast", place(e) size(.7cm)) text(2.75 0.1 "Nowcast", place(e) size(.7cm)) ///
text(2.75 1.1 "Nearcast", place(e) size(.7cm)) legend(off) plotregion(margin(0 0 0 0)) ///
xsize(10) ysize(9)
graph export "05.figures\WEOvintage_poverty.png", width(2000) as(png) replace
graph export "06.text\Figures\WEOvintage_poverty.png", width(2000) as(png) replace

****************************
*** TRY WHEN REWEIGHTING ***
****************************
use "04.outputdata\Error_all.dta", clear
bysort code datatype (year): drop if _n==1
drop Y_l_head Y_c_head Y_g_mean Y_l_mean Y_g_medi Y_l_medi Y_g_gini Y_l_gini
drop if missing(ad_Y_l_head_v141_al_dnmu_grow)
bysort code: gen weight=1/_N
// Finding weighted mean and median error by sample
collapse (mean) ad_* [aw=weight]
gen sample = "al"
reshape long ad, i(sample) j(method) string
gen outcome       = substr(method,4,6)
replace method    = substr(method,11,.)
gen conversion1   = substr(method,9,4)
gen conversion2   = substr(method,14,4)
gen sampleused    = substr(method,6,2)
replace method    = substr(method,1,4)
// Only keep errors over sample when this sample was used for the predictions_final
keep if sample==sampleused
drop sampleused
order outcome sample method conversion*
compress
keep if inlist(outcome,"g_mean","l_head")
keep if conversion1=="dnmu" | outcome=="g_mean"
keep if strpos(method,"v")
drop conversion*
replace ad = 100*ad if outcome=="g_mean"
gen     season = "apr" if substr(method,4,1)=="1"
replace season = "oct" if substr(method,4,1)=="2"
gen     vintage = "nowcast"  if strpos(method,"10")
replace vintage = "forecast" if strpos(method,"09")
replace vintage = "nearcast" if strpos(method,"11")
gen yr = substr(method,2,2)
destring yr, replace
replace yr = yr-10
tostring yr, gen(year)
replace year = "t"+year if yr<0
replace year = "t+"+year if yr>0
replace year = "t" if yr==0
replace year = year + " " + season
// Figure
graph  bar ad if outcome=="l_head" & sample=="al",   over(year, label(angle(90)) sort(yr)) ///
graphregion(color(white)) ytitle("Mean Absolute Deviation (pct. points)") subtitle(,fcolor(white) nobox) ///
blabel(bar, position(center) format(%3.2f) color(white)) ylab(,angle(horizontal)) ///
xsize(15) ysize(10) bar(1, color("26 134 147"))



******************************
*** LARGE CHANGE IN ERRORS ***
******************************
use "04.outputdata\Error_all.dta", clear
keep code datatype year sample* weight* *g_mean_v* *l_head_v*
drop se* oo* op* dv* sample_now
drop *_co*
drop if missing(ad_Y_l_head_v091_al_dnmu_grow)
format ad* %3.2f
cls
sum ad_Y*,d f

rename *_al_dnmu_grow *
rename *_al_dire *
rename ad_Y_* *
reshape long l_head_v g_mean_v, i(code datatype year) j(vintage) string

bysort code datatype year (vintage): gen dif_lhead = abs(l_head_v-l_head_v[_n-1])
bysort code datatype year (vintage): gen dif_gmean = abs(g_mean_v-g_mean_v[_n-1])

sum dif_lhead,d

hist dif_lhead

sum dif_gmean,d

hist dif_gmean

****************************************
*** LOOK AT CHANGES IN GDP FORECASTS ***
****************************************
use "02.inputdata\WEO\HISTWEO.dta", clear
// Convert to growth rates
foreach var of varlist HISTWEO* {
bysort code (year): gen growth_`var' = (`var'/`var'[_n-1]-1)*100
}
drop HISTWEO*
rename *HISTWEO_gdp_l_* **
egen missings = rownonmiss(growth*)
drop if missings==0
forvalues ref=9/20 {
gen gdp_t`ref'_1 = .
gen gdp_t`ref'_2 = .
forvalues yr=1999/2021 {
replace gdp_t`ref'_1 = growth_`yr'_01 if year==`yr'+10-`ref'
replace gdp_t`ref'_2 = growth_`yr'_02 if year==`yr'+10-`ref'
}
}
drop growth* missings
egen missings = rownonmiss(gdp*)
drop if missings==0
drop missings

// until t14
drop *15* *16* *17* *18* *19* *20*
egen missings = rowmiss(gdp*)
keep if missings==0
drop missings
bysort code: gen weight=1/_N

foreach var of varlist gdp* {
gen dif_`var' = abs(`var'-gdp_t14_2)
}
drop gdp*
rename *gdp_* **
collapse dif* [aw=weight]

gen temp = 1
reshape long dif_t, i(temp) j(vintage) string
drop temp
replace vintage = subinstr(vintage,"_1",".25",.)
replace vintage = subinstr(vintage,"_2",".75",.)
destring vintage, replace
replace vintage = vintage-10
twoway connected dif_t vintage, sort lwidth(medthick) color("26 134 147") || ///
pcarrowi 2.25 0.25 2.1 0.25 (12) "Apr", color("117 26 51") mlabsize(medlarge) mlabcolor("117 26 51") || ///
pcarrowi 1.95 0.75 1.8 0.75 (12) "Sep", color("117 26 51") mlabsize(medlarge) mlabcolor("117 26 51") graphregion(color(white))  ///
ylab(0(0.5)3.5,angle(horizontal) labsize(medlarge)) ytitle("Mean difference from estimate at t+4 (pct. points)", size(medlarge)) ///
xlab(-0.5 "t-1" 0.5 "t" 1.5 "t+1" 2.5 "t+2" 3.5 "t+3" 4.5 "t+4", labsize(medlarge)) xscale(range(-1 5)) ///
xline(0, lcolor(black) lpattern(shortdash)) xline(1,lcolor(black) lpattern(shortdash)) ///
xline(2,lcolor(black) lpattern(shortdash)) xtitle("Release year of growth data used to predict growth in year t") ///
text(0.75 -0.9 "Forecast", place(e) size(.7cm)) text(0.75 0.1 "Nowcast", place(e) size(.7cm)) ///
text(0.75 1.1 "Nearcast", place(e) size(.7cm)) legend(off) plotregion(margin(0 0 0 0)) ///
xsize(10) ysize(9)
graph export "05.figures\WEOvintage_growth.png", width(2000) as(png) replace
graph export "06.text\Figures\WEOvintage_growth.png", width(2000) as(png) replace


**********************************************************
*** LOOK AT CHANGES IN GDP FORECASTS COMPARED TO TRUTH ***
**********************************************************
/*
use "02.inputdata\WEO\HISTWEO.dta", clear
// Merge with national accounts serires
merge 1:1 code year using "02.inputdata\NationalAccounts\NationalAccounts_processed.dta", nogen keep(3) keepusing(NA_l_gdp*)
// Convert to growth rates
foreach var of varlist HISTWEO* NA_l* {
bysort code (year): gen growth_`var' = (`var'/`var'[_n-1]-1)*100
}
drop HISTWEO* NA_l*
gen     growthtrue = growth_NA_l_gdp_usd2010
replace growthtrue = growth_NA_l_gdp_ppp2017 if missing(growthtrue)
replace growthtrue = growth_NA_l_gdp_ppp2011 if missing(growthtrue)
replace growthtrue = growth_NA_l_gdp_lcu if missing(growthtrue)
drop growth_NA*
rename *HISTWEO_gdp_l_* **
egen missings = rownonmiss(growth*)
drop if missings==0
drop if missing(growthtrue)
forvalues ref=9/20 {
gen gdp_t`ref'_1 = .
gen gdp_t`ref'_2 = .
forvalues yr=1999/2021 {
replace gdp_t`ref'_1 = growth_`yr'_01 if year==`yr'+10-`ref'
replace gdp_t`ref'_2 = growth_`yr'_02 if year==`yr'+10-`ref'
}
}
drop growth_* missings
egen missings = rownonmiss(gdp*)
drop if missings==0
drop missings

// until t15
drop *16* *17* *18* *19* *20*
egen missings = rowmiss(gdp*)
keep if missings==0
drop missings
bysort code: gen weight=1/_N

foreach var of varlist gdp* {
gen dif_`var' = abs(`var'-growthtrue)
}
drop gdp*
rename *gdp_* **
collapse dif* [aw=weight]

gen temp = 1
reshape long dif_t, i(temp) j(vintage) string
drop temp
replace vintage = subinstr(vintage,"_1",".25",.)
replace vintage = subinstr(vintage,"_2",".75",.)
destring vintage, replace
replace vintage = vintage-10
twoway scatter dif_t vintage, graphregion(color(white)) ///
ylab(0(0.5)3,angle(horizontal)) ytitle("Mean difference from final estimate (pct. points)") ///
xlab(-0.5 "t-1" 0.5 "t" 1.5 "t+1" 2.5 "t+2" 3.5 "t+3" 4.5 "t+4" 5.5 "t+5") ///
xline(0, lcolor(black) lpattern(shortdash)) xline(1,lcolor(black) lpattern(shortdash)) ///
xline(2,lcolor(black) lpattern(shortdash)) xtitle("Release of data used to estimate growth in year t") ///
text(0.75 -0.8 "Forecast", place(e) size(.3cm)) text(0.75 0.2 "Nowcast", place(e) size(.3cm)) ///
text(0.75 1.2 "Nearcast", place(e) size(.3cm))
*/

****************************************************************************
*** LOOK AT CHANGES IN GDP FORECASTS FOR SUBSAMPLE WITH SURVEY ESTIMATES ***
****************************************************************************
use "02.inputdata\FinalInputData.dta", clear
// Keep relevant variables
drop CLASS* PCN* WDI* WEO* RS* NA* HISTWEO*_l_*
// Keep relevant rows
keep if sample_al_g == 1
rename HISTWEO_gdp_g_* growth_*
forvalues ref=9/20 {
gen gdp_t`ref'_1 = .
gen gdp_t`ref'_2 = .
forvalues yr=1999/2021 {
replace gdp_t`ref'_1 = growth_`yr'_01 if year==`yr'+10-`ref'
replace gdp_t`ref'_2 = growth_`yr'_02 if year==`yr'+10-`ref'
}
}
drop growth* 
egen missings = rownonmiss(gdp*)
drop if missings==0
drop missings

// until t15
drop *16* *17* *18* *19* *20*
egen missings = rowmiss(gdp*)
keep if missings==0
drop missings
bysort code: gen weight=1/_N

foreach var of varlist gdp* {
gen dif_`var' = abs(`var'-gdp_t15_2)
}
drop gdp*
rename *gdp_* **
collapse dif* [aw=weight]

gen temp = 1
reshape long dif_t, i(temp) j(vintage) string
drop temp
replace vintage = subinstr(vintage,"_1",".25",.)
replace vintage = subinstr(vintage,"_2",".75",.)
destring vintage, replace
replace vintage = vintage-10
replace dif_t = 100*dif_t
twoway connected dif_t vintage, sort color("26 134 147") || ///
pcarrowi 1.45 0.25 1.3 0.25 (12) "Apr", color("117 26 51") mlabcolor("117 26 51") || ///
pcarrowi 1.25 0.75 1.1 0.75 (12) "Sep", color("117 26 51") mlabcolor("117 26 51") graphregion(color(white))  ///
ylab(0(0.5)3,angle(horizontal)) ytitle("Mean difference from estimate at t+5 (pct. points)") ///
xlab(-0.5 "t-1" 0.5 "t" 1.5 "t+1" 2.5 "t+2" 3.5 "t+3" 4.5 "t+4" 5.5 "t+5") xscale(range(-1 6)) ///
xline(0, lcolor(black) lpattern(shortdash)) xline(1,lcolor(black) lpattern(shortdash)) ///
xline(2,lcolor(black) lpattern(shortdash)) xtitle("Release year of data used to estimate growth in year t") ///
text(0.75 -0.8 "Forecast", place(e) size(.3cm)) text(0.75 0.2 "Nowcast", place(e) size(.3cm)) ///
text(0.75 1.2 "Nearcast", place(e) size(.3cm)) legend(off) plotregion(margin(0 0 0 0))


*************************************
*** SAVE AS ABOVE BY INCOME GROUP ***
*************************************
use "02.inputdata\FinalInputData.dta", clear
// Keep relevant variables
drop PCN* WDI* WEO* RS* NA* HISTWEO*_l_*
// Keep relevant rows
keep if sample_al_g == 1
rename HISTWEO_gdp_g_* growth_*
forvalues ref=9/20 {
gen gdp_t`ref'_1 = .
gen gdp_t`ref'_2 = .
forvalues yr=1999/2021 {
replace gdp_t`ref'_1 = growth_`yr'_01 if year==`yr'+10-`ref'
replace gdp_t`ref'_2 = growth_`yr'_02 if year==`yr'+10-`ref'
}
}
drop growth* 
egen missings = rownonmiss(gdp*)
drop if missings==0
drop missings

// until t15
drop *16* *17* *18* *19* *20*
egen missings = rowmiss(gdp*)
keep if missings==0
drop missings
bysort code: gen weight=1/_N

foreach var of varlist gdp* {
gen dif_`var' = abs(`var'-gdp_t15_2)
}
drop gdp*
rename *gdp_* **
collapse dif* [aw=weight], by(CLASS_incgroup)

reshape long dif_t, i(CLASS_incgroup) j(vintage) string
replace vintage = subinstr(vintage,"_1",".25",.)
replace vintage = subinstr(vintage,"_2",".75",.)
destring vintage, replace
replace vintage = vintage-10
replace dif_t = 100*dif_t
twoway connected dif_t vintage, sort color("26 134 147") by(CLASS_incgroup, note("") graphregion(color(white))) ///
ylab(0(0.5)3,angle(horizontal)) ytitle("Mean difference from estimate at t+5 (pct. points)") ///
xlab(-0.5 "t-1" 0.5 "t" 1.5 "t+1" 2.5 "t+2" 3.5 "t+3" 4.5 "t+4" 5.5 "t+5") xscale(range(-1 6)) ///
xline(0, lcolor(black) lpattern(shortdash)) xline(1,lcolor(black) lpattern(shortdash)) ///
xline(2,lcolor(black) lpattern(shortdash)) xtitle("Release year of data used to estimate growth in year t") ///
legend(off) plotregion(margin(0 0 0 0))
