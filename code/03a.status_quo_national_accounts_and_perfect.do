********************
*** INTRODUCTION ***
********************
/*
This .do-file generates status quo predictions, predictions from using other national accounts rules and 'perfect' mean predictions, i.e. predictions that are identical to the true values. The latter will be helpful to look at how well our poverty predictions are if we are able to perfectly predict the mean.
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

***********************
*** LOAD INPUT DATA *** 
***********************
use "02.inputdata\FinalInputData.dta", clear
// Rename WDI variable on FCE 
rename WDI_g_NECONTOTLK NA_g_fce
// Most covariates not needed for this part
drop WDI* WEO* RS*

***************************************************
*** NATIONAL ACCOUNTS WITHOUT PASSTHROUGH RATES ***
***************************************************
foreach type in co al ri po {
foreach na   in gdp gni hfc fce {
bysort code datatype (year): gen Y_g_mean_n`na'_`type'_dire = NA_g_`na' if _n!=1
// Works for the first extrapolation observation
bysort code datatype (year): gen Y_l_mean_n`na'_`type'_grow = Y_l_mean[_n-1]*(1+Y_g_mean_n`na'_`type'_dire)^PCN_extrapolationtime
// Works for the other extrapolation observations
bysort code datatype (year): replace Y_l_mean_n`na'_`type'_grow = Y_l_mean_n`na'_`type'_grow[_n-1]*(1+Y_g_mean_n`na'_`type'_dire)^PCN_extrapolationtime if missing(Y_l_mean_n`na'_`type'_grow)
}
}

************************************************
*** NATIONAL ACCOUNTS WITH PASSTHROUGH RATES ***
************************************************
// No tuning, only parameter estimation, so only outer CV needed
// Creating folds
gen outerfold = runiformint(1,5)
cls
foreach outcome in mean medi {
foreach type    in co al ri po {
foreach na      in gdp gni hfc fce {
gen Y_g_`outcome'_p`na'_`type'_dire = .
forvalues outerfold=1/5 {
qui reg Y_g_`outcome' NA_g_`na' if outerfold!=`outerfold' [aw=weight_`type'_g], nocons
predict temp
replace Y_g_`outcome'_p`na'_`type'_dire = temp if outerfold==`outerfold' & sample_now==0
drop temp
}
// Regresions with full sample for nowcasting predictions
reg Y_g_`outcome' NA_g_`na' [aw=weight_`type'_g], nocons
predict temp
replace Y_g_`outcome'_p`na'_`type'_dire = temp if sample_now==1
drop temp
// Works for the first extrapolation observation
bysort code datatype (year): gen Y_l_`outcome'_p`na'_`type'_grow = Y_l_`outcome'[_n-1]*(1+Y_g_`outcome'_p`na'_`type'_dire)^PCN_extrapolationtime
// Works for the other extrapolation observations
bysort code datatype (year): replace Y_l_`outcome'_p`na'_`type'_grow = Y_l_`outcome'_p`na'_`type'_grow[_n-1]*(1+Y_g_`outcome'_p`na'_`type'_dire)^PCN_extrapolationtime if missing(Y_l_`outcome'_p`na'_`type'_grow)
}
}
}

*********************************************************************
*** NATIONAL ACCOUNTS WITH PASSTHROUGH RATES SEPARATE BY DATATYPE ***
*********************************************************************
// Create interaction variables
cls
foreach NA of varlist NA_g* {
gen `NA'_datatype = `NA'*(datatype-1)
}
foreach outcome in mean medi  {
foreach type    in co al ri po {
foreach na      in gdp gni hfc fce {
gen Y_g_`outcome'_d`na'_`type'_dire = .
forvalues outerfold=1/5 {
qui reg Y_g_`outcome' NA_g_`na' NA_g_`na'_datatype if outerfold!=`outerfold' [aw=weight_`type'_g], nocons
predict temp
replace Y_g_`outcome'_d`na'_`type'_dire = temp if outerfold==`outerfold' & sample_now==0
drop temp
}
// Regresions with full sample for nowcasting predictions
disp in red "`outcome' -- `type' -- `na'"
reg Y_g_`outcome' NA_g_`na' NA_g_`na'_datatype  [aw=weight_`type'_g], nocons
predict temp
replace Y_g_`outcome'_d`na'_`type'_dire = temp if sample_now==1
drop temp
// Works for the first extrapolation observation
bysort code datatype (year): gen Y_l_`outcome'_d`na'_`type'_grow = Y_l_`outcome'[_n-1]*(1+Y_g_`outcome'_d`na'_`type'_dire)^PCN_extrapolationtime
// Works for the other extrapolation observations
bysort code datatype (year): replace Y_l_`outcome'_d`na'_`type'_grow = Y_l_`outcome'_d`na'_`type'_grow[_n-1]*(1+Y_g_`outcome'_d`na'_`type'_dire)^PCN_extrapolationtime if missing(Y_l_`outcome'_d`na'_`type'_grow)
}
}
}

****************************************************************
*** GDP WITH PASSTHROUGH RATES SEPARATE BY DATATYPE AND MORE ***
****************************************************************
// Create interaction variables
cls
gen NA_g_gdp_con_neg  = NA_g_gdp if datatype==1 & NA_g_gdp<0
gen NA_g_gdp_inc_pos  = NA_g_gdp if datatype==2 & NA_g_gdp>=0
gen NA_g_gdp_inc_neg  = NA_g_gdp if datatype==2 & NA_g_gdp<0
gen NA_g_gdp_con_LIC  = NA_g_gdp if datatype==1 & CLASS_incgroup==2
gen NA_g_gdp_con_LMIC = NA_g_gdp if datatype==1 & CLASS_incgroup==3
gen NA_g_gdp_con_UMIC = NA_g_gdp if datatype==1 & CLASS_incgroup==4
gen NA_g_gdp_inc_HIC  = NA_g_gdp if datatype==2 & CLASS_incgroup==1
gen NA_g_gdp_inc_LIC  = NA_g_gdp if datatype==2 & CLASS_incgroup==2
gen NA_g_gdp_inc_LMIC = NA_g_gdp if datatype==2 & CLASS_incgroup==3
gen NA_g_gdp_inc_UMIC = NA_g_gdp if datatype==2 & CLASS_incgroup==4
gen NA_g_gdp_con_ECA  = NA_g_gdp if datatype==1 & CLASS_region==2
gen NA_g_gdp_con_LAC  = NA_g_gdp if datatype==1 & CLASS_region==3
gen NA_g_gdp_con_MNA  = NA_g_gdp if datatype==1 & CLASS_region==4
gen NA_g_gdp_con_NAM  = NA_g_gdp if datatype==1 & CLASS_region==5
gen NA_g_gdp_con_SAR  = NA_g_gdp if datatype==1 & CLASS_region==6
gen NA_g_gdp_con_SSA  = NA_g_gdp if datatype==1 & CLASS_region==7
gen NA_g_gdp_inc_EAP  = NA_g_gdp if datatype==2 & CLASS_region==1
gen NA_g_gdp_inc_ECA  = NA_g_gdp if datatype==2 & CLASS_region==2
gen NA_g_gdp_inc_LAC  = NA_g_gdp if datatype==2 & CLASS_region==3
gen NA_g_gdp_inc_MNA  = NA_g_gdp if datatype==2 & CLASS_region==4
gen NA_g_gdp_inc_NAM  = NA_g_gdp if datatype==2 & CLASS_region==5
gen NA_g_gdp_inc_SAR  = NA_g_gdp if datatype==2 & CLASS_region==6
gen NA_g_gdp_inc_SSA  = NA_g_gdp if datatype==2 & CLASS_region==7
foreach var of varlist NA_g_gdp_* {
replace `var' = 0 if missing(`var') & !missing(NA_g_gdp)
}
foreach outcome in mean medi  {
foreach type    in al co ri po {
gen Y_g_`outcome'_rgdp_`type'_dire = .
gen Y_g_`outcome'_igdp_`type'_dire = .
gen Y_g_`outcome'_sgdp_`type'_dire = .
// Training sample predictions
	forvalues outerfold=1/5 {
	// Growth sign
	qui reg Y_g_`outcome' NA_g_gdp NA_g_gdp_inc_pos NA_g_gdp_inc_neg NA_g_gdp_con_neg if outerfold!=`outerfold' [aw=weight_`type'_g], nocons
	predict temp
	replace Y_g_`outcome'_sgdp_`type'_dire = temp if outerfold==`outerfold' & sample_now==0
	drop temp
	// Income group
	qui reg Y_g_`outcome' NA_g_gdp NA_g_gdp_con_LIC NA_g_gdp_con_LMIC NA_g_gdp_con_UMIC NA_g_gdp_inc_HIC NA_g_gdp_inc_LIC NA_g_gdp_inc_LMIC NA_g_gdp_inc_UMIC if outerfold!=`outerfold' [aw=weight_`type'_g], nocons
	predict temp
	replace Y_g_`outcome'_igdp_`type'_dire = temp if outerfold==`outerfold' & sample_now==0
	drop temp
	// Region
	qui reg Y_g_`outcome' NA_g_gdp NA_g_gdp_con_ECA NA_g_gdp_con_LAC NA_g_gdp_con_MNA NA_g_gdp_con_NAM NA_g_gdp_con_SAR NA_g_gdp_con_SSA NA_g_gdp_inc_EAP NA_g_gdp_inc_ECA NA_g_gdp_inc_LAC NA_g_gdp_inc_MNA NA_g_gdp_inc_NAM NA_g_gdp_inc_SAR NA_g_gdp_inc_SSA if outerfold!=`outerfold' [aw=weight_`type'_g], nocons
	predict temp
	replace Y_g_`outcome'_rgdp_`type'_dire = temp if outerfold==`outerfold' & sample_now==0
	drop temp
	}
// Regresions with full sample for nowcasting predictions
	// Growth sign
	reg Y_g_`outcome'  NA_g_gdp NA_g_gdp_inc_pos NA_g_gdp_inc_neg NA_g_gdp_con_neg  [aw=weight_`type'_g], nocons
	predict temp
	replace Y_g_`outcome'_sgdp_`type'_dire = temp if sample_now==1
	drop temp
	// Income group
	reg Y_g_`outcome'  NA_g_gdp NA_g_gdp_con_LIC NA_g_gdp_con_LMIC NA_g_gdp_con_UMIC NA_g_gdp_inc_HIC NA_g_gdp_inc_LIC NA_g_gdp_inc_LMIC NA_g_gdp_inc_UMIC [aw=weight_`type'_g], nocons
	predict temp
	replace Y_g_`outcome'_igdp_`type'_dire = temp if sample_now==1
	drop temp	
	// Region
	reg Y_g_`outcome'  NA_g_gdp NA_g_gdp_con_ECA NA_g_gdp_con_LAC NA_g_gdp_con_MNA NA_g_gdp_con_NAM NA_g_gdp_con_SAR NA_g_gdp_con_SSA NA_g_gdp_inc_EAP NA_g_gdp_inc_ECA NA_g_gdp_inc_LAC NA_g_gdp_inc_MNA NA_g_gdp_inc_NAM NA_g_gdp_inc_SAR NA_g_gdp_inc_SSA   [aw=weight_`type'_g], nocons
	predict temp
	replace Y_g_`outcome'_rgdp_`type'_dire = temp if sample_now==1
	drop temp
// Works for the first extrapolation observation
foreach x in s i r {
bysort code datatype (year): gen Y_l_`outcome'_`x'gdp_`type'_grow = Y_l_`outcome'[_n-1]*(1+Y_g_`outcome'_`x'gdp_`type'_dire)^PCN_extrapolationtime
// Works for the other extrapolation observations
bysort code datatype (year): replace Y_l_`outcome'_`x'gdp_`type'_grow = Y_l_`outcome'_`x'gdp_`type'_grow[_n-1]*(1+Y_g_`outcome'_`x'gdp_`type'_dire)^PCN_extrapolationtime if missing(Y_l_`outcome'_`x'gdp_`type'_grow)
}
}
}

************************************
*** GENERATE PERFECT PREDICTIONS ***
************************************
foreach type    in co al ri po {
foreach outcome in mean medi gini {
bysort code datatype (year): gen Y_l_`outcome'_perf_`type'_dire = Y_l_`outcome' if _n!=1
}
}

***********************************************************
*** GENERATE PREDICTIONS BASED ON LAGGED HEADCOUNT RATE ***
***********************************************************
foreach type    in co al ri po {
bysort code datatype (year): gen Y_l_head_lagh_`type'_dire = Y_l_head[_n-1]
}


keep code year datatype sample* weight* Y* PCN_extrapolationtime CLASS_region_povcalnet
order code datatype year
sort  code datatype year 
compress

save "03.intermediatedata\Predictions\status_quo_national_accounts_and_perfect.dta", replace