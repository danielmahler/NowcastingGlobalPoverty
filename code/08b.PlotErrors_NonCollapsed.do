********************
*** INTRODUCTION ***
********************
/*
This .do-file plots figures using the non-collapsed errors
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
*** DISTRIBUTION OF ERRORS ***
******************************
use "04.outputdata\Error_all.dta", clear
// Select method to plot
local method_baseline "dgdp_al_dnmu_grow"
// Comparison method (for second plots)
local method_compare "grbo_al_dire"
// Select sample to plot (choose co or al)
local sample al

// Create log absolute deviations and order them
gen ld_Y_l_head_`method_baseline' = log10(ad_Y_l_head_`method_baseline'+0.1)
gen ld_Y_l_head_`method_compare'  = log10(ad_Y_l_head_`method_compare'+0.1)
bysort sample_`sample'_g (ld_Y_l_head_`method_baseline'): gen or_Y_l_head_`method_baseline' = _n/_N*100
bysort sample_`sample'_g (ld_Y_l_head_`method_compare'):  gen or_Y_l_head_`method_compare'  = _n/_N*100

twoway line ld_Y_l_head_`method_baseline' or_Y_l_head_`method_baseline' if sample_`sample'_g==1, sort || ///
       line ld_Y_l_head_`method_compare'  or_Y_l_head_`method_compare'  if sample_`sample'_g==1, sort    ///
	   ylab(`=log10(0+0.1)' "0" ///
	        `=log10(0.1+0.1)' "0.1" ///
			`=log10(0.25+0.1)' "0.25" ///
			`=log10(0.5+0.1)' "0.5" ///
			`=log10(1+0.1)' "1" ///
			`=log10(2+0.1)' "2" ///
			`=log10(5+0.1)' "5" ///
			`=log10(10+0.1)' "10" ///
			`=log10(20+0.1)' "20" ///
			`=log10(50+0.1)' "50", angle(horizontal)) ///
	   xsize(10) ysize(10) graphregion(color(white)) ///
	   xlab("") xtitle("Prediction errors ordered from smallest to largest") ///
	   ytitle("Absolute deviation (percentage points)") ///
	   plotregion(margin(0 0 0 0)) ///
	   legend(region(lcolor(white)) rows(2) span ///
	   order(1 "Status quo with passthrough rate by income/consumption" ///
	         2 "Best model that predicts poverty rates directly")) 

// Create log deviations and order them
drop ld* or*
gen ld_Y_l_head_`method_baseline' = log10(dv_Y_l_head_`method_baseline'+0.1) if dv_Y_l_head_`method_baseline'>=0
gen ld_Y_l_head_`method_compare'  = log10(dv_Y_l_head_`method_compare'+0.1)  if dv_Y_l_head_`method_baseline'>=0
replace ld_Y_l_head_`method_baseline' = -log10(-dv_Y_l_head_`method_baseline'+0.1)-2 if dv_Y_l_head_`method_baseline'<0
replace ld_Y_l_head_`method_compare'  = -log10(-dv_Y_l_head_`method_compare'+0.1)-2 if dv_Y_l_head_`method_compare'<0
bysort sample_`sample'_g (dv_Y_l_head_`method_baseline'): gen or_Y_l_head_`method_baseline' = _n/_N*100
bysort sample_`sample'_g (dv_Y_l_head_`method_compare'):  gen or_Y_l_head_`method_compare'  = _n/_N*100

twoway line ld_Y_l_head_`method_baseline' or_Y_l_head_`method_baseline' if sample_`sample'_g==1, sort || ///
       line ld_Y_l_head_`method_compare'  or_Y_l_head_`method_compare'  if sample_`sample'_g==1, sort  ///
	   ylab(`=log10(0+0.1)' "0" ///
	        `=log10(0.2+0.1)' "0.2" ///
			`=log10(0.5+0.1)' "0.5" ///
			`=log10(2+0.1)' "2" ///
			`=log10(5+0.1)' "5" ///
			`=log10(20+0.1)' "20" ///
			`=log10(50+0.1)' "50" ///
	        `=-log10(0.2+0.1)-2' "-0.2" ///
			`=-log10(0.5+0.1)-2' "-0.5" ///
			`=-log10(2+0.1)-2' "-2" ///
			`=-log10(5+0.1)-2' "-5" ///
			`=-log10(20+0.1)-2' "-20" ///
			`=-log10(50+0.1)-2' "-50", angle(horizontal)) ///
	   xsize(10) ysize(10) graphregion(color(white)) ///
	   xlab("") xtitle("Prediction errors ordered from smallest to largest") ///
	   ytitle("Deviation (percentage points)") ///
	   plotregion(margin(0 0 0 0)) ///
	   legend(region(lcolor(white)) rows(2) span ///
	   order(1 "Status quo with passthrough rate by income/consumption" ///
	         2 "Best model that predicts poverty rates directly")) 


********************************************
*** PLOT OF ERROR AND EXTRAPOLATION TIME ***
********************************************
use "04.outputdata\Error_all.dta", clear
rename PCN_extrapolationtime extrapolationtime

// Select method to plot
local method_baseline "dgdp_al_dnmu_grow"
// Comparison method (for second plots)
local method_compare "grbo_al_dire"
// Comparison method (for second plots)
local method_compare2 "cirf_al_dnmu_grow"
// Select sample to plot (choose co or al)
local sample al

keep if sample_`sample'_g==1

// Adding some noise to the x-variable to reveal overlapping points
gen extrapolationtime_plot = extrapolationtime + runiform()-0.5

twoway  lpolyci ad_Y_l_head_`method_baseline' extrapolationtime_plot [aw=weight_`sample'_g] if extrapolationtime<=10, ///
		lwidth(medthick) lcolor("26 134 147") fcolor("26 134 147%25") acolor("26 134 147%0") bwidth(1.5) || ///
		lpoly ad_Y_l_head_`method_compare' extrapolationtime_plot [aw=weight_`sample'_g] if extrapolationtime<=10, /// 
		lwidth(medthick) lcolor("117 26 51") fcolor("117 26 51%25") acolor("117 26 51%0") bwidth(1.5)  || ///
		lpoly ad_Y_l_head_`method_compare2' extrapolationtime_plot [aw=weight_`sample'_g] if extrapolationtime<=10, /// 
		lwidth(medthick) lcolor("255 100 51") fcolor("255 100 51%25") acolor("255 100 51%0") bwidth(1.5)   ///
graphregion(color(white)) xtitle("Extrapolation time", size(medlarge)) ytitle("Mean absolute deviation""(percentage points)", size(medlarge)) ///
legend(order(2 "GDP growth w. passthrough rate by inc./cons." 4 "Best model overall" 3 "Best model that predicts poverty directly") rows(2) span symxsize(*0.3) size(small) region(lcolor(white))) ///
plotregion(margin(0 0 0 0)) xsize(10) ysize(10) ///
ylab(0(2)10,angle(horinzontal) labsize(medlarge) format(%2.0f)) xlab(,grid labsize(medlarge)) saving("05.figures\extrapolation.gph", replace)

******************************
*** PLOT OF ERROR AND YEAR ***
******************************
use "04.outputdata\Error_all.dta", clear

// Select method to plot
local method_baseline "plas_al_dnmu_grow"
// Comparison method (for second plots)
local method_compare "grbo_al_dire"
// Comparison method (for second plots)
local method_compare2 "cirf_al_dnmu_grow"
// Select sample to plot (choose co or al)
local sample al

keep if sample_`sample'_g==1

twoway  lpolyci ad_Y_l_head_`method_baseline' year [aw=weight_`sample'_g], ///
		lwidth(medthick) lcolor("26 134 147") fcolor("26 134 147%25") acolor("26 134 147%0") bwidth(5) || ///
		lpoly ad_Y_l_head_`method_compare' year  [aw=weight_`sample'_g] , ///
		lwidth(medthick) lcolor("117 26 51") fcolor("117 26 51%25") acolor("117 26 51%0") bwidth(5) || ///
		lpoly ad_Y_l_head_`method_compare2' year  [aw=weight_`sample'_g] , ///
		lwidth(medthick) lcolor("255 100 51") fcolor("255 100 51%25") acolor("255 100 51%0") bwidth(5) ///
		graphregion(color(white)) xtitle("Year", size(medlarge)) ytitle("Mean absolute deviation""(percentage points)", size(medlarge)) ///
legend(order(2 "GDP growth w. passthrough rate by inc./cons." 4 "Best model overall" 3 "Best model that predicts poverty directly") rows(3) span symxsize(*0.3) size(medlarge) region(lcolor(white))) ///
plotregion(margin(0 0 0 0)) xsize(10) ysize(10) ///
ylab(0(2)10,angle(horinzontal) labsize(medlarge) format(%2.0f)) xlab(,grid labsize(medlarge)) ///
saving("05.figures\year.gph", replace)

************************************
*** PLOT OF ERROR AND GDP GROWTH ***
************************************
use "04.outputdata\Error_all.dta", clear

// Select method to plot
local method_baseline "dgdp_al_dnmu_grow"
// Comparison method (for second plots)
local method_compare "grbo_al_dire"
// Comparison method (for second plots)
local method_compare2 "cirf_al_dnmu_grow"
// Select sample to plot (choose co or al)
local sample al

keep if sample_`sample'_g==1

isid code datatype year
merge 1:1 code datatype year using "02.inputdata\FinalInputData.dta", keep(1 3) nogen keepusing(NA_g_gdp)

twoway  lpolyci ad_Y_l_head_`method_baseline' NA_g_gdp [aw=weight_`sample'_g] if inrange(NA_g_gdp,-0.1,0.1), ///
		lwidth(medthick) lcolor("26 134 147") fcolor("26 134 147%25") acolor("26 134 147%0") bwidth(0.025) || ///
		lpoly ad_Y_l_head_`method_compare' NA_g_gdp  [aw=weight_`sample'_g] if inrange(NA_g_gdp,-0.1,0.1) , ///
		lwidth(medthick) lcolor("117 26 51") fcolor("117 26 51%25") acolor("117 26 51%0")  bwidth(0.025) || ///
		lpoly ad_Y_l_head_`method_compare2' NA_g_gdp  [aw=weight_`sample'_g] if inrange(NA_g_gdp,-0.1,0.1) , ///
		lwidth(medthick) lcolor("255 100 51") fcolor("255 100 51%25") acolor("255 100 51%0")  bwidth(0.025) ///
graphregion(color(white)) xtitle("Annualized growth in real GDP/capita", size(medlarge)) ytitle("Mean absolute deviation""(percentage points)", size(medlarge)) ///
legend(order(2 "GDP growth w. passthrough rate by inc./cons." 4 "Best model overall" 3 "Best model that predicts poverty directly") rows(2) span symxsize(*0.3) size(small) region(lcolor(white))) ///
plotregion(margin(0 0 0 0)) xsize(10) ysize(10) ///
ylab(0(2)10,angle(horinzontal) labsize(medlarge) format(%2.0f)) xlab(,grid labsize(medlarge)) ///
saving("05.figures\gdpgrowth.gph", replace)

**********************************
*** PLOT OF ERROR AND INFLATION ***
**********************************
use "04.outputdata\Error_all.dta", clear

// Select method to plot
local method_baseline "dgdp_al_dnmu_grow"
// Comparison method (for second plots)
local method_compare "grbo_al_dire"
// Comparison method (for second plots)
local method_compare2 "cirf_al_dnmu_grow"
// Select sample to plot (choose co or al)
local sample al

keep if sample_`sample'_g==1

isid code datatype year
merge 1:1 code datatype year using "02.inputdata\FinalInputData.dta", keep(1 3) nogen keepusing(WDI_l_FPCPITOTLZG)
rename WDI_l_FPCPITOTLZG inflation

twoway  lpolyci ad_Y_l_head_`method_baseline' inflation [aw=weight_`sample'_g] if inrange(inflation,-1,15), ///
		lwidth(medthick) lcolor("26 134 147") fcolor("26 134 147%25") acolor("26 134 147%0") bwidth(2.5) || ///
		lpoly ad_Y_l_head_`method_compare' inflation  [aw=weight_`sample'_g]  if inrange(inflation,-1,15), ///
		lwidth(medthick) lcolor("117 26 51") fcolor("117 26 51%25") acolor("117 26 51%0")  bwidth(2.5) || ///
		lpoly ad_Y_l_head_`method_compare2' inflation  [aw=weight_`sample'_g]  if inrange(inflation,-1,15), ///
		lwidth(medthick) lcolor("255 100 51") fcolor("255 100 51%25") acolor("255 100 51%0")  bwidth(2.5) ///
graphregion(color(white)) xtitle("Inflation (%)", size(medlarge)) ytitle("Mean absolute deviation""(percentage points)", size(medlarge)) ///
legend(order(2 "GDP growth w. passthrough rate by inc./cons." 4 "Best model overall" 3 "Best model that predicts poverty directly") rows(2) span symxsize(*0.3) size(small) region(lcolor(white))) ///
plotregion(margin(0 0 0 0)) xsize(10) ysize(10) ///
ylab(0(2)10,angle(horinzontal) labsize(medlarge) format(%2.0f)) xlab(,grid labsize(medlarge)) ///
saving("05.figures\inflation.gph", replace)


**********************************
*** PLOT OF ERROR AND RURALITY ***
**********************************
use "04.outputdata\Error_all.dta", clear

// Select method to plot
local method_baseline "dgdp_al_dnmu_grow"
// Comparison method (for second plots)
local method_compare "grbo_al_dire"
// Comparison method (for second plots)
local method_compare2 "cirf_al_dnmu_grow"
// Select sample to plot (choose co or al)
local sample al

keep if sample_`sample'_g==1

isid code datatype year
merge 1:1 code datatype year using "02.inputdata\FinalInputData.dta", keep(1 3) nogen keepusing(WDI_l_SPRURTOTLZS)
rename WDI_l_SPRURTOTLZS ruralshare

twoway  lpolyci ad_Y_l_head_`method_baseline' ruralshare [aw=weight_`sample'_g], ///
		lwidth(medthick) lcolor("26 134 147") fcolor("26 134 147%25") acolor("26 134 147%0") bwidth(7.5) || ///
		lpoly ad_Y_l_head_`method_compare' ruralshare  [aw=weight_`sample'_g], ///
		lwidth(medthick) lcolor("117 26 51") fcolor("117 26 51%25") acolor("117 26 51%0")  bwidth(7.5) || ///
		lpoly ad_Y_l_head_`method_compare2' ruralshare  [aw=weight_`sample'_g], ///
		lwidth(medthick) lcolor("255 100 51") fcolor("255 100 51%25") acolor("255 100 51%0")  bwidth(7.5) ///
graphregion(color(white)) xtitle("Rural population (% of total population)", size(medlarge)) ytitle("Mean absolute deviation""(percentage points)", size(medlarge)) ///
legend(order(2 "GDP growth w. passthrough rate by inc./cons." 4 "Best model overall" 3 "Best model that predicts poverty directly") rows(2) span symxsize(*0.3) size(small) region(lcolor(white))) ///
plotregion(margin(0 0 0 0)) xsize(10) ysize(10) ///
ylab(0(2)10,angle(horinzontal) labsize(medlarge) format(%2.0f)) xlab(,grid labsize(medlarge)) 
graph export "05.figures\rurality.png", as(png) width(2000) replace

**********************************
*** PLOT OF ERROR AND EXPORTS ***
**********************************
use "04.outputdata\Error_all.dta", clear

// Select method to plot
local method_baseline "dgdp_al_dnmu_grow"
// Comparison method (for second plots)
local method_compare "grbo_al_dire"
// Comparison method (for second plots)
local method_compare2 "cirf_al_dnmu_grow"
// Select sample to plot (choose co or al)
local sample al

keep if sample_`sample'_g==1

isid code datatype year
merge 1:1 code datatype year using "02.inputdata\FinalInputData.dta", keep(1 3) nogen keepusing(WDI_l_NEEXPGNFSZS)
rename WDI_l_NEEXPGNFSZS export

twoway  lpolyci ad_Y_l_head_`method_baseline' export [aw=weight_`sample'_g] if export<85, ///
		lwidth(medthick) lcolor("26 134 147") fcolor("26 134 147%25") acolor("26 134 147%0") bwidth(7.5) || ///
		lpoly ad_Y_l_head_`method_compare' export  [aw=weight_`sample'_g]  if export<85, ///
		lwidth(medthick) lcolor("117 26 51") fcolor("117 26 51%25") acolor("117 26 51%0")  bwidth(7.5) || ///
		lpoly ad_Y_l_head_`method_compare2' export  [aw=weight_`sample'_g]  if export<85, ///
		lwidth(medthick) lcolor("255 100 51") fcolor("255 100 51%25") acolor("255 100 51%0")  bwidth(7.5) ///
graphregion(color(white)) xtitle("Export (% of GDP)", size(medlarge)) ytitle("Mean absolute deviation""(percentage points)", size(medlarge)) ///
legend(order(2 "GDP growth w. passthrough rate by inc./cons." 4 "Best model overall" 3 "Best model that predicts poverty directly") rows(2) span symxsize(*0.3) size(small) region(lcolor(white))) ///
plotregion(margin(0 0 0 0)) xsize(10) ysize(10) ///
ylab(0(2)10,angle(horinzontal) labsize(medlarge) format(%2.0f)) xlab(,grid labsize(medlarge)) ///
saving("05.figures\export.gph", replace)

*********************************
*** PLOT OF ERROR AND CAPITAL ***
*********************************
use "04.outputdata\Error_all.dta", clear

// Select method to plot
local method_baseline "dgdp_al_dnmu_grow"
// Comparison method (for second plots)
local method_compare "grbo_al_dire"
// Comparison method (for second plots)
local method_compare2 "cirf_al_dnmu_grow"
// Select sample to plot (choose co or al)
local sample al

keep if sample_`sample'_g==1

isid code datatype year
merge 1:1 code datatype year using "02.inputdata\FinalInputData.dta", keep(1 3) nogen keepusing(WDI_l_NEGDITOTLZS)
rename WDI_l_NEGDITOTLZS gcf

twoway  lpolyci ad_Y_l_head_`method_baseline' gcf [aw=weight_`sample'_g] if inrange(gcf,10,40), ///
		lwidth(medthick) lcolor("26 134 147") fcolor("26 134 147%25") acolor("26 134 147%0") bwidth(2.5) || ///
		lpoly ad_Y_l_head_`method_compare' gcf  [aw=weight_`sample'_g]  if inrange(gcf,10,40), ///
		lwidth(medthick) lcolor("117 26 51") fcolor("117 26 51%25") acolor("117 26 51%0")  bwidth(2.5) || ///
		lpoly ad_Y_l_head_`method_compare2' gcf  [aw=weight_`sample'_g]  if inrange(gcf,10,40), ///
		lwidth(medthick) lcolor("255 100 51") fcolor("255 100 51%25") acolor("gcf 100 51%0")  bwidth(2.5) ///
graphregion(color(white)) xtitle("Gross capital formation (% of GDP)", size(medlarge)) ytitle("Mean absolute deviation""(percentage points)", size(medlarge)) ///
legend(order(2 "GDP growth w. passthrough rate by inc./cons." 4 "Best model overall" 3 "Best model that predicts poverty directly") rows(2) span symxsize(*0.3) size(small) region(lcolor(white))) ///
plotregion(margin(0 0 0 0)) xsize(10) ysize(10) ///
ylab(0(2)10,angle(horinzontal) labsize(medlarge) format(%2.0f)) xlab(,grid labsize(medlarge)) ///
saving("05.figures\gcf.gph", replace)

*********************
*** COMBINE PLOTS ***
*********************
grc1leg "05.figures\extrapolation.gph" "05.figures\year.gph" "05.figures\gdpgrowth.gph" ///
"05.figures\inflation.gph" "05.figures\export.gph" "05.figures\gcf.gph", ///
legendfrom("05.figures\gdpgrowth.gph") iscale(*0.8) graphregion(color(white)) rows(3) 
graph display, ysize(10) xsize(8)
graph export "05.figures\Heterogeneity.png", width(2000) as(png) replace
graph export "06.text\Figures\Heterogeneity.png", width(2000) as(png) replace

erase "05.figures\extrapolation.gph" 
erase "05.figures\year.gph" 
erase "05.figures\gdpgrowth.gph" 
erase "05.figures\inflation.gph" 
erase "05.figures\export.gph" 
erase "05.figures\gcf.gph"