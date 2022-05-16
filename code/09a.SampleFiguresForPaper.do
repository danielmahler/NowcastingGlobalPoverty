********************
*** INTRODUCTION ***
********************
/*
Introduction: This .do-file produces figures for the paper explaining how it works to predict the mean and the mean/Gini.
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

***************************************************
*** FIND A GOOD EXAMPLE FOR PREDICTING THE MEAN ***
***************************************************
use "03.intermediatedata\Predictions\predictions_final.dta", clear
// Keep countries with a last survey between 2015-2019
bysort code: egen lastyear = max(year) if !missing(Y_l_head)
gsort code -lastyear
bysort code: replace lastyear = lastyear[_n-1] if missing(lastyear)
keep if inrange(lastyear,2015,2019)
// A country with a moderate poverty rate in 2021
keep if year==2021
keep if inrange(Y_l_head_dgdp_al_dnmu_grow,5,30)
// With a noticeable increase in mean consumption
keep if Y_g_mean_dgdp_al_dire>0.02
keep code Y_l_head_dgdp_al_dnmu_grow Y_g_mean_dgdp_al_dire
list
// Choosing Botswana

***************************
*** LOAD 1000 QUANTILES ***
***************************
use "02.inputdata\PovcalNet\FullDistributions_Collapsed.dta", clear
keep if code=="BWA"
keep if datayear>2015
drop code datayear datatype
gen welf2021 = welf*1.04^5

foreach type in welf welf2021 {
kdensity `type', kernel(epan) bwidth(0.5) gen(smooth`type') at(`type') nograph
}

// Add first row
set obs 1001
foreach var of varlist welf* smooth* {
replace `var' = 0 if _n==_N
}
sort welf
gen zero = 0

****************************************
*** PLOT DISTRIBUTION NEUTRAL OPTION ***
****************************************
twoway rarea smoothwelf    zero welf      if welf<20, color("26 134 147%100") || ///
       rarea smoothwelf2021 zero welf2021 if welf2021<20, color("117 26 51%50") || ///
	   pci 0 1.9 0.2 1.9, lcolor(black) lpattern(shortdash) ///
	   legend(span rows(1) size(medlarge)  region(lcolor(white) ) ///
	   order(1 "2016 (last survey)" 2 "2021 (nowcast)") symxsize(*.5)) ///
	   graphregion(color(white)) ///
	   ytitle("Density") ///
	   title("") ///
	   xtitle("Daily consumption per capita (2011 USD PPP)") ///
	   text(0.185 2.1 "Poverty line", place(e) size(medsmall)) ///
	   xsize(10) ysize(10) plotregion(margin(0 0 0 0)) ///
	   xlabel(0 1.9 5 10 15 20 ) ylab(,angle(horizontal)) 
graph export "05.figures\Example_growthmean.png", width(2000) as(png) replace
graph export "06.text\Figures\Example_growthmean.png", width(2000) as(png) replace

gen poor=welf<1.9
gen poor2021 = welf2021<1.9
sum poor*

******************
*** CONVEX GIC ***
******************
drop if _n==1
_pctile welf2021, p(40)
local p40 = r(r1)
qui sum welf2021 if welf2021<`p40', d
local b40sum = r(sum)
qui sum welf, d
local totalsum = r(sum)
local b40share = `b40sum'/`totalsum'
local spp = 0.05*(0.4/`b40share'-1)
qui povsim welf2021, type(welfare) gic(c) growth(0) premium(`spp') repetitions(1) name(03.intermediatedata\Povsim\Example_convex)  adjustp(1) replace

*******************
*** LINEAR GICs ***
*******************
tempfile datasofar
save    `datasofar'

local spp = 0
local count = 1
local step = 0.01
local delta = 1
	while abs(`delta')>0.0001 {
		use `datasofar', clear
		qui ineqdec0 welf2021
		scalar startgini = r(gini)
		scalar targetgini = r(gini)*(1-0.05)
		qui povsim welf2021, type(welfare) gic(l) growth(0) premium(`spp') repetitions(1) name(03.intermediatedata\Povsim\Example_linear) adjustp(1) replace
		use "03.intermediatedata\Povsim\Example_linear.dta", clear
		qui ineqdec0 welfare1
		scalar endgini = r(gini)
		local delta = round(targetgini-endgini,0.001)
		// First we try a SPP of 0.01 (or -0.01 depending on the whether the Gini is higher or lower than the target
		if `count'==1 & `delta'>0 {
			local step = -`step'
		}
		// If that didn't get close enough, we try to adjusts the SPP further. If suddenly overshoot, we reverse back a bit.
		if `count'>1 {
			if sign(`delta')==sign(`deltapast') { 
				local step = `step'*1.5
			}
			if sign(`delta')!=sign(`deltapast') { 
				local step = -`step'/2
			}
		}
		*disp as error "ITERATION `count' - DIFFERENCE `delta'"
		local spp = `spp'+`step'
		local count = `count' + 1
		local deltapast = `delta'
		disp "`delta'"
	}


******************
*** MERGE GICs ***
******************
use `datasofar'
gen mtile0 = _n
merge 1:1 mtile0 using "03.intermediatedata\Povsim\Example_linear.dta", keepusing(welfare1) nogen
rename welfare1 welf2021_lgic
merge 1:1 mtile0 using "03.intermediatedata\Povsim\Example_convex.dta", keepusing(welfare1) nogen
rename welfare1 welf2021_cgic
drop mtile0

**************************************
*** CHECKING GINIs ARE AS EXPECTED ***
**************************************
cls
qui ineqdec0 welf2021
local gini `r(gini)'
disp round(`gini',0.0001)
disp round(`gini'*0.95,0.0001)

qui ineqdec0 welf2021_lgic
disp round(`r(gini)',0.0001) 
qui ineqdec0 welf2021_lgic
disp round(`r(gini)',0.0001)


*******************
*** HISTORGRAMS ***
*******************

foreach type in welf2021_lgic welf2021_cgic {
kdensity `type', kernel(epan) bwidth(0.5) gen(smooth`type') at(`type') nograph
}

// Add first row
set obs 1001
foreach var of varlist welf* smooth* {
replace `var' = 0 if _n==_N
}
sort welf

gen pctile = _n/10

twoway rarea smoothwelf    zero welf      if welf<20, fcolor("26 134 147%25") lcolor("26 134 147") || ///
       rarea smoothwelf2021_lgic zero welf2021_lgic if welf2021_lgic<20, fcolor("117 26 51%25") lcolor("117 26 51")  || ///
	   rarea smoothwelf2021_cgic zero welf2021_cgic if welf2021_cgic<20, fcolor("255 100 51%25") lcolor("255 100 51") || ///
	   pci 0 1.9 0.2 1.9, lcolor(black) lpattern(shortdash) ///
	   legend(span rows(3) size(medlarge)  region(lcolor(white) ) ///
	   order(1 "2016 (last survey)" 2 "2021 (nowcast, linear GIC)" 3 "2021 (nowcast, convex GIC)") symxsize(*.5)) ///
	   graphregion(color(white)) ///
	   ytitle("Density") ///
	   title("") ///
	   xtitle("Daily consumption per capita (2011 USD PPP)") ///
	   text(0.185 2.1 "Poverty line", place(e) size(medsmall)) ///
	   xsize(10) ysize(10) plotregion(margin(0 0 0 0)) ///
	   xlabel(0 1.9 5 10 15 20 ) ylab(,angle(horizontal)) 
graph export "05.figures\Example_gic2.png", width(2000) as(png) replace
graph export "06.text\Figures\Example_gic2.png", width(2000) as(png) replace

	   gen gwelf = 0
	   gen gwelf2021_lgic = ((welf2021_lgic/welf)^(1/5)-1)*100
	   gen gwelf2021_cgic = ((welf2021_cgic/welf)^(1/5)-1)*100
	   
twoway line gwelf2021_lgic  pctile, color("117 26 51") lwidth(medthick) ||  ///
       line gwelf2021_cgic pctile if pctile>1, color("255 100 51") lwidth(medthick) ///
	   legend(span rows(3)  region(lcolor(white)) ///
	   order(1 "2021 (nowcast, linear GIC)" 2 "2021 (nowcast, convex GIC)") symxsize(*.5)) ///
	  graphregion(color(white)) ylab(-5(5)10) ///
	   ytitle("Annualized growth in consumption (%)") ///
	   xtitle("Percentile") ylab(,angle(horizontal)) ///
	   xsize(10) ysize(10) plotregion(margin(0 0 0 0)) yline(0, lcolor(black) lpattern(shortdash))
graph export "05.figures\Example_gic1.png", width(2000) as(png) replace
graph export "06.text\Figures\Example_gic1.png", width(2000) as(png) replace
	   


**********************************
*** PLOT LOGNORMAL/LOGLOGISTIC ***
**********************************
/*
povcalnet, country(all) year(all) clear
replace mean = mean*12/365
sum mean, d // 1-99pctl from 1.5 to 70 (0.15-1.85 in logs)
sum gini, d // 1-99pctl from 25-60 
*/
clear
set obs 21
gen gini = _n*2+18 
expand 21
bysort gini: gen mean = _n/2-0.49
// Infer the log standard deviation based on the Gini
gen stdY                  = invnormal((gini/100+1)/2)*(2^(1/2))
// Infer the log mean based on the mean
gen meanY_mu              = ln(mean)-0.5*stdY^2
// Back out the headcount rate
gen hc_ln = normal((ln(1.9)-meanY_mu)/stdY)*100
// Finds the poverty rate by using the estimate of the mean as well as the gini for a given survey,
// and imposes a Fisk distribution to infer what poverty would be.
// Infer the parameters
cap gen beta       = 1/(gini/100)
cap gen alpha_mu = mean*sin(c(pi)/beta) 
// Back out the headcount rate
cap gen hc_ll = 1/(1+(1.9/alpha_mu)^(-beta))*100

global color1 "26 134 147"
global color2 "117 26 51"
twoway contour hc_ln gini mean, ccuts(10 20 30 40 50 60 70 80 90) interp(none) ///
		crule(linear) scolor(white) ecolor($color1)  ///
		graphregion(color(white)) xlab(,labsize(medlarge)) zlab(,labsize(medlarge)) ///
		ytitle("Gini", size(medlarge)) ylab(, angle(horizontal) labsize(medlarge)) ///
		xtitle("Daily mean consumption (2011 PPP)", size(medlarge)) ///
		ztitle("Poverty rate (%)", size(medlarge))  ysize(10) xsize(13) plotregion(margin(0 0 0 0))
		graph export "05.figures\Example_lognormal.png", width(2000) as(png) replace
		graph export "06.text\Figures\Example_lognormal.png", width(2000) as(png) replace
		
twoway contour hc_ll gini mean, ccuts(10 20 30 40 50 60 70 80 90) interp(none) ///
		crule(linear) scolor(white) ecolor($color1)  ///
		graphregion(color(white)) xlab(,labsize(medlarge)) zlab(,labsize(medlarge)) ///
		ytitle("Gini", size(medlarge)) ylab(, angle(horizontal) labsize(medlarge)) ///
		xtitle("Daily mean consumption (2011 PPP)", size(medlarge)) ///
		ztitle("Poverty rate (%)", size(medlarge))  ysize(10) xsize(13) plotregion(margin(0 0 0 0))
		graph export "05.figures\Example_loglogistic.png", width(2000) as(png) replace
		graph export "06.text\Figures\Example_loglogistic.png", width(2000) as(png) replace
		