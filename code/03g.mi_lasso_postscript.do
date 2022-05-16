********************
*** INTRODUCTION ***
********************
/*
This .do-file takes the final MI lasso predictions and makes them fully consistent with the other prediction files.
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

*****************
*** LOAD DATA ***
*****************
use "03.intermediatedata\Predictions\lasso.dta", clear
rename wbcode code
drop year sample
rename orig_year year
destring datatype, replace
bysort code datatype (year): gen PCN_extrapolationtime = (year-year[_n-1])
rename Y*_al_l*  Y*_rlas_al_dire
rename Y*_al_pl* Y*_plas_al_dire
rename Y*_co_l*  Y*_rlas_co_dire
rename Y*_co_pl* Y*_plas_co_dire

*******************************************************
*** REMOVE DATATYPE FROM COUNTRIES WITHOUT ANY DATA ***
*******************************************************
bysort code: egen surveys = count(Y_l_mean)
replace datatype = . if surveys==0
drop surveys


***********************************************************
*** CENSOR GROWTH/CHANGE VARIABLES TO REASONABLE LEVELS ***
***********************************************************

foreach var of varlist Y_c_head_* {
	replace `var' = -100  if `var' <-100
	replace `var' =  100  if `var' >100 & !missing(`var')
}

foreach var of varlist Y_g_* {
	replace `var' = -0.5  if `var' <-0.5
	replace `var' =  1  if `var' >1 & !missing(`var')
}

********************************************************
*** CONVERTING GROWTH/CHANGE PREDICTIONS INTO LEVELS ***
********************************************************
foreach type in rlas_al rlas_co plas_al plas_co {
	foreach outc in mean medi gini {
		// Works for the first extrapolation observation
		bysort code datatype (year): gen Y_l_`outc'_`type'_grow = Y_l_`outc'[_n-1]*(Y_g_`outc'_`type'_dire+1)^PCN_extrapolationtime
		// Works for the remaining extrapolation observations
		bysort code datatype (year): replace Y_l_`outc'_`type'_grow = Y_l_`outc'_`type'_grow[_n-1]*(Y_g_`outc'_`type'_dire+1)^PCN_extrapolationtime if missing(Y_l_`outc'_`type'_grow)
	}
	// Works for the first extrapolation observation
	bysort code datatype (year): gen Y_l_head_`type'_grow = Y_l_head[_n-1]+Y_c_head_`type'_dire*PCN_extrapolationtime
	// Works for the remaining extrapolation observations
	bysort code datatype (year): replace Y_l_head_`type'_grow =  Y_l_head_`type'_grow[_n-1]+Y_c_head_`type'_dire*PCN_extrapolationtime if missing(Y_l_head_`type'_grow)
}

***************************************************
*** CENSOR LEVEL VARIABLES TO REASONABLE LEVELS ***
***************************************************
foreach var of varlist Y_l_head_* {
	replace `var' = 0   if `var' <0
	replace `var' = 100 if `var'>100 & !missing(`var')
}
foreach var of varlist Y_l_gini_* {
	replace `var' = 70 if `var'>70 & !missing(`var')
	replace `var' = 20 if `var'<20
	}
	
foreach var of varlist Y_l_mean_* Y_l_medi_* {
	replace `var' = 10   if `var' <10
	replace `var' = 3000 if `var' >3000 & !missing(`var')
}


******************
*** FINALIZING ***
******************
keep code year datatype sample* weight* *las*

format *l_mean* *l_medi* %4.0f
format *g_* *c_* %3.2f
format *l_head* *l_gini* %3.1f

order code datatype year
sort  code datatype year sample* weight* Y*

save "03.intermediatedata\Predictions\lasso_postscript.dta", replace

*******************
*** SOME CHECKS ***
*******************

keep if sample_co_g==1
keep code datatype year Y_g* Y_c*
sum Y*, f

foreach var of varlist Y*co* {
qui sum `var', d
hist `var' if inrange(`var',`r(p1)',`r(p99)'), name(`var', replace)
}

**********************
*** LOAD RIPO DATA ***
**********************
use "03.intermediatedata\Predictions\lasso_ripo.dta", clear
rename wbcode code
drop year sample
rename orig_year year
destring datatype, replace
bysort code datatype (year): gen PCN_extrapolationtime = (year-year[_n-1])
rename Y*_ri_l*  Y*_rlas_ri_dire
rename Y*_ri_pl* Y*_plas_ri_dire
rename Y*_po_l*  Y*_rlas_po_dire
rename Y*_po_pl* Y*_plas_po_dire

*******************************************************
*** REMOVE DATATYPE FROM COUNTRIES WITHOUT ANY DATA ***
*******************************************************
bysort code: egen surveys = count(Y_l_mean)
replace datatype = . if surveys==0
drop surveys


***********************************************************
*** CENSOR GROWTH/CHANGE VARIABLES TO REASONABLE LEVELS ***
***********************************************************

foreach var of varlist Y_c_head_* {
	replace `var' = -100  if `var' <-100
	replace `var' =  100  if `var' >100 & !missing(`var')
}

foreach var of varlist Y_g_* {
	replace `var' = -0.5  if `var' <-0.5
	replace `var' =  1  if `var' >1 & !missing(`var')
}

********************************************************
*** CONVERTING GROWTH/CHANGE PREDICTIONS INTO LEVELS ***
********************************************************
foreach type in rlas_ri rlas_po plas_ri plas_po {
	foreach outc in mean medi gini {
		// Works for the first extrapolation observation
		bysort code datatype (year): gen Y_l_`outc'_`type'_grow = Y_l_`outc'[_n-1]*(Y_g_`outc'_`type'_dire+1)^PCN_extrapolationtime
		// Works for the remaining extrapolation observations
		bysort code datatype (year): replace Y_l_`outc'_`type'_grow = Y_l_`outc'_`type'_grow[_n-1]*(Y_g_`outc'_`type'_dire+1)^PCN_extrapolationtime if missing(Y_l_`outc'_`type'_grow)
	}
	// Works for the first extrapolation observation
	bysort code datatype (year): gen Y_l_head_`type'_grow = Y_l_head[_n-1]+Y_c_head_`type'_dire*PCN_extrapolationtime
	// Works for the remaining extrapolation observations
	bysort code datatype (year): replace Y_l_head_`type'_grow =  Y_l_head_`type'_grow[_n-1]+Y_c_head_`type'_dire*PCN_extrapolationtime if missing(Y_l_head_`type'_grow)
}

***************************************************
*** CENSOR LEVEL VARIABLES TO REASONABLE LEVELS ***
***************************************************
foreach var of varlist Y_l_head_* {
	replace `var' = 0   if `var' <0
	replace `var' = 100 if `var'>100 & !missing(`var')
}
foreach var of varlist Y_l_gini_* {
	replace `var' = 70 if `var'>70 & !missing(`var')
	replace `var' = 20 if `var'<20
	}
	
foreach var of varlist Y_l_mean_* Y_l_medi_* {
	replace `var' = 10   if `var' <10
	replace `var' = 3000 if `var' >3000 & !missing(`var')
}


******************
*** FINALIZING ***
******************
keep code year datatype sample* weight* *las*

format *l_mean* *l_medi* %4.0f
format *g_* *c_* %3.2f
format *l_head* *l_gini* %3.1f

order code datatype year
sort  code datatype year sample* weight* Y*

save "03.intermediatedata\Predictions\lasso_postscript_ripo.dta", replace

*******************
*** SOME CHECKS ***
*******************

keep if sample_po_g==1
keep code datatype year Y_g* Y_c*
sum Y*, f

foreach var of varlist Y*po* {
qui sum `var', d
hist `var' if inrange(`var',`r(p1)',`r(p99)'), name(`var', replace)
}