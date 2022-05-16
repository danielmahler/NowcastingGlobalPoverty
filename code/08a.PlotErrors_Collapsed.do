********************
*** INTRODUCTION ***
********************
/*
This .do-file plots the summary error statistics by method
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

********************************************
*** BEST WAY OF CONVERTING GINI CHNANGES ***
********************************************
// Deciding what to plot
local conversiontype "mean" // Choose "median" or "mean"
local sample         "al"     // Choose "co" or "al"
local error          "ad"     // Choose "se" (squared error), "ad" (absolute deviation), "dv" (deviation), or "ti" (trends incorrectly predicted)
local errortype      "mean"   // Choose "median" or "mean"

// Loading and keeping relevant data
use "04.outputdata\Error_collapsed.dta", clear
keep if sample == "`sample'"
keep if type   == "`errortype'"
keep sample type method conversion* `error'

// Dropping results not using Gini conversions
drop if inlist(conversion1,"dnmu","dnmd","dire","grow")
drop if method=="perf"

// Creating  variable over Gini convertion type
gen     outcome = "mean"   if !inlist(conversion1,"llmd","lnme")
replace outcome = "median" if  inlist(conversion1,"llmd","lnmd")

// Removing GIC models if target variable is median and Gini
if "`conversiontype'"=="median" {
drop if inlist(conversion1,"gicl","gicc")
}

gen     giniconversion = "Log normal"   if inlist(conversion1,"lnmu","lnmd")
replace giniconversion = "Log logistic" if inlist(conversion1,"llmu","llmd")
replace giniconversion = "Linear GIC"   if conversion1=="gicl"
replace giniconversion = "Convex GIC"   if conversion1=="gicc"

replace conversion2 = "(5) Levels"  if conversion2 == "dire"
replace conversion2 = "(6) Growth" if conversion2 == "grow"

isid outcome giniconversion conversion2 method

// Figure
graph  bar ad if outcome=="`conversiontype'",   over(method, label(angle(90))) ///
by( conversion2 giniconversion, cols(2) colfirst note("") graphregion(color(white)) iscale(*0.8)) ///
graphregion(color(white)) ytitle("Mean Absolute Deviation (pct. points)") subtitle(,fcolor(white) nobox) ///
blabel(bar, position(center) format(%3.2f) color(white)) ylab(,angle(horizontal)) ///
xsize(15) ysize(20) bar(1, color("26 134 147"))
graph export "05.figures\Mean_Gini_`conversiontype'_`sample'_`error'_`errortype'.png", width(2000) as(png) replace
graph export "06.text\Figures\Mean_Gini_`conversiontype'_`sample'_`error'_`errortype'.png", width(2000) as(png) replace

******************************
*** BEST METHOD IN GENERAL ***
******************************
// Deciding what to plot
local conversiontype "mean" // Choose "median" or "mean"
local sample         "co" // Choose "co", "al", "ri", or "po"
local error          "ad"     // Choose "se" (squared error), "ad" (absolute deviation), "dv" (deviation), or "ti" (trends incorrectly predicted)
local errortype      "mean" // Choose "median" or "mean"

// Loading and keeping relevant data
use "04.outputdata\Error_collapsed.dta", clear
if "`error'"=="ti" {
gen ti = oo+op
}
keep if sample == "`sample'"
keep if type   == "`errortype'"
keep if outcome== "l_head"

// Dropping results not using ML
drop if inlist(method,"perf","ngdp","pgdp","dgdp","nhfc","phfc","dhfc") | inlist(method,"ngni","pgni","dgni","dsqu","nsqu","psqu") | inlist(method,"dfce","nfce","pfce","lagh","igdp","rgdp","sgdp","cirl")
drop if strpos(method,"v")
*drop if method=="cirl"
// Only keep relevant conversion types
if "`conversiontype'"=="mean" {
drop if inlist(conversion1,"dnmd","lnmd","llmd")
}
if "`conversiontype'"=="median" {
drop if inlist(conversion1,"dnmu","lnmu","llmu","gicl","gicc")
}

// For mean+Gini conversions, use best performer
*replace conversion1 = substr(conversion1,1,2) if strpos(conversion1,"mu") | strpos(conversion1,"md")
bysort method conversion2: egen min`error' = min(`error') if !missing(conversion2) & !strpos(conversion1,"dn")
replace `error' = min`error' if !missing(min`error')
replace conversion1 = "gini" if !missing(min`error')
drop min`error'
duplicates drop sample type method conversion1 conversion2 `error', force

gen     figlabel = "1. Levels of poverty rates"                if conversion1=="dire"
replace figlabel = "2. Changes in poverty rates"               if conversion1=="grow"
replace figlabel = "3. Levels of `conversiontype'"             if strpos(conversion1,"dn") & conversion2=="dire"
replace figlabel = "4. Growth in the `conversiontype'"        if strpos(conversion1,"dn") & conversion2=="grow"
replace figlabel = "5. Levels of `conversiontype' & Gini"      if conversion1=="gini" & conversion2=="dire"
replace figlabel = "6. Growth in the `conversiontype' & Gini" if conversion1=="gini" & conversion2=="grow"

if "`error'"=="ad" {
local ytitle "Mean Absolute Deviation (pct. points)"
}
if "`error'"=="se" {
replace se = se^(1/2)
local ytitle "Root Mean Squared Error (pct. points)"
}


isid figlabel method

graph set window fontface "Palatino Linotype"

if "`error'"!="ti" {
graph  bar `error',  over(method, label(angle(90))) ///
by(figlabel, cols(2) note("") graphregion(color(white)) iscale(*0.8)) ///
graphregion(color(white)) ytitle("`ytitle'", size(medsmall)) ///
blabel(bar, position(center) format(%3.2f) color(white)) ylab(,angle(horizontal)) ///
xsize(15) ysize(15) subtitle(,fcolor(white) nobox)  bar(1, color("26 134 147")) 
graph export "05.figures\Best_method_`conversiontype'_`sample'_`error'_`errortype'.png", width(2000) as(png) replace
graph export "06.text\Figures\Best_method_`conversiontype'_`sample'_`error'_`errortype'.png", width(2000) as(png) replace
}

if "`error'"=="ti" {
local ytitle "Share of trends incorrectly predicted"
graph  bar oo op,  stack over(method, label(angle(90))) ///
by(figlabel, cols(2) note("") graphregion(color(white)) iscale(*0.8)) ///
graphregion(color(white)) ytitle("`ytitle'", size(medsmall)) ///
blabel(bar, position(center) format(%3.2f) color(white)) ylab(,angle(horizontal)) ///
xsize(15) ysize(15) subtitle(,fcolor(white) nobox)  bar(1, color("26 134 147")) bar(2, color("117 26 51")) ///
legend(order(1 "Share of trends incorrectly predicted as a decline" 2 "Share of trends incorrectly predicted as an increase") size(small) region(lcolor(white)) rows(2)) 
*graph export "05.figures\Best_method_`conversiontype'_`sample'_`error'_`errortype'.png", width(2000) as(png) replace
*graph export "06.text\Figures\Best_method_`conversiontype'_`sample'_`error'_`errortype'.png", width(2000) as(png) replace
}


********************************************************
*** COMPARING BEST PERFORMER, PEFECT, AND STATUS QUO ***
********************************************************
// Deciding what to plot
local sample    "ri" // Choose "co", "al", "ri", or "po"
local error     "ad"     // Choose "se" (squared error), "ad" (absolute deviation), "dv" (deviation), or "ti" (trends incorrectly predicted)
local errortype "mean" // Choose "median" or "mean"

// Loading and keeping relevant data
use "04.outputdata\Error_collapsed.dta", clear
keep if sample == "`sample'"
keep if type   == "`errortype'"
keep if outcome== "l_head"
drop if strpos(conversion1,"md")
if "`error'"=="ti" {
gen ti = oo+op
}

// Drop non-GDP NA methods
drop if strpos(method,"fce")
drop if strpos(method,"sgdp")
drop if strpos(method,"v")
drop if strpos(method,"cirl")
// Finding best performing ML method and the best ML method at predicting poverty directly
egen bestML       = min(`error') if !inlist(method,"perf","lagh") & !strpos(method,"gdp")
egen bestMLdirect = min(`error') if !inlist(method,"perf","lagh") & conversion1=="dire"

// Dropping inequality perfect scenarios
drop if method=="perf" & conversion1!="dnmu"

// Dropping results not using ML except for the best one one overall and the best direct one
keep if inlist(method,"perf","ngdp","pgdp","dgdp","lagh") | `error'== bestML | `error'==bestMLdirect

// Create label for figures
gen     figlabel = "Using only GDP growth to shift the mean"      if method == "ngdp"
replace figlabel = "Using GDP growth with a passthrough rate"     if method == "pgdp"
replace figlabel = "GDP growth with passthrough rate by inc/cons" if method == "dgdp"
replace figlabel = "If growth in the mean is predicted perfectly" if method == "perf"
replace figlabel = "Using latest official poverty rate"           if method == "lagh"
replace figlabel = "Best method predicting poverty directly"      if `error'== bestMLdirect
replace figlabel = "Best method overall"                          if `error'== bestML

if "`error'"=="ad" {
local ytitle "Mean Absolute Deviation (pct. points)"
}

if "`error'"=="se" {
replace se = se^(1/2)
local ytitle "Root Mean Squared Error (pct. points)"
}

if "`error'"!="ti" {
graph  bar `error',  over(figlabel, label(angle(35)) sort(`error') descending) ///
graphregion(color(white)) ytitle("`ytitle'") bar(1, color("26 134 147")) ///
blabel(bar, position(center) format(%3.2f) color(white)) ylab(,angle(horizontal)) ///
xsize(20) ysize(16) graphregion(margin(35 4 0 6))
graph export "05.figures\Best_method_and perfect_`sample'_`error'.png", width(2000) as(png) replace
graph export "06.text\Figures\Best_method_mean_and_perfect_`sample'_`error'.png", width(2000) as(png) replace
}

if "`error'"=="ti" {
local ytitle "Share of trends incorrect"
drop if method=="lagh"
graph  bar oo op,  stack over(figlabel, label(angle(35)) sort(`error') descending) ///
graphregion(color(white)) ytitle("`ytitle'") bar(1, color("26 134 147")) bar(2, color("117 26 51")) ///
legend(order(1 "Share of trends incorrectly predicted as a decline" 2 "Share of trends incorrectly predicted as an increase") region(lcolor(white)) rows(2) span symxsize(*0.5)) ///
blabel(bar, position(center) format(%3.2f) color(white)) ylab(,angle(horizontal)) ///
xsize(20) ysize(16) graphregion(margin(35 4 0 6))
graph export "05.figures\Best_method_and perfect_`sample'_`error'.png", width(2000) as(png) replace
graph export "06.text\Figures\Best_method_mean_and_perfect_`sample'_`error'.png", width(2000) as(png) replace
}

***********************************
*** COMPARING NATIONAL ACCOUNTS ***
***********************************
// Deciding what to plot
local sample    "ri" // Choose "co" or "al"
local error     "ad"     // Choose "se" (squared error), "ad" (absolute deviation), "dv" (deviation), or "ti" (trends incorrectly predicted)
local errortype "mean" // Choose "median" or "mean"

// Loading and keeping relevant data
use "04.outputdata\Error_collapsed.dta", clear
keep if sample == "`sample'"
keep if type   == "`errortype'"
drop if strpos(conversion1,"md")
keep sample type method conversion* `error'

// Only keeping NA methods
keep if inlist(method,"nsqu","psqu","dsqu","ngdp","pgdp","dgdp") | inlist(method,"nhfc","phfc","dhfc","ngni","pgni","dgni") | inlist(method,"nfce","pfce","dfce")

// Create label for figures
gen NAtype = upper(substr(method,2,3))
replace NAtype = "HFCE" if NAtype=="HFC"
replace NAtype = "Status quo" if NAtype=="SQU"
replace method = substr(method,1,1)
gen     figlabel = "No passthrough rate"          if method=="n"
replace figlabel = "Passthrough rate"             if method=="p"
replace figlabel = "Passthrough rate by inc/cons" if method=="d"

if "`error'"=="ad" {
local ytitle "Mean Absolute Deviation (pct. points)"
}
if "`error'"=="se" {
replace se = se^(1/2)
local ytitle "Root Mean Squared Error (pct. points)"
}

graph  bar `error',  by(NAtype, note("") graphregion(color(white)) rows(1)) over(figlabel, label(angle(35)) sort(`error') descending) ///
graphregion(color(white)) ytitle("`ytitle'") bar(1, color("26 134 147")) ///
blabel(bar, position(center) format(%3.2f) color(white)) ylab(,angle(horizontal)) ///
xsize(20) ysize(12) plotregion(margin(10 2 2 2))  subtitle(,fcolor(white) nobox)
*graph export "05.figures\Best_method_and perfect_`sample'_`error'.png", width(2000) as(png) replace
*graph export "06.text\Figures\Best_method_mean_and_perfect_`sample'_`error'.png", width(2000) as(png) replace

forvalues weight=1(1)100 {
gen error_`weight' = op+`weight'/10*oo
}

drop if inlist(method,"lagh","perf")

local error error_60
sort `error'
list sample-conversion2 oo op `error' if _n<=5
