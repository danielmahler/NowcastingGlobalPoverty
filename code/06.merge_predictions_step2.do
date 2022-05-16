*************************
***READ THE TEXT BELOW***
*************************
/*
This .do-file takes direct predictions and merges them from the ones that have used distributional assumptions to get poverty predictions. It then cleans the final prediction data
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

*************************
*** MERGE PREDICTIONS ***
*************************
use "03.intermediatedata\Predictions\predictions_step1.dta", clear
merge 1:1 code datatype year using  "03.intermediatedata\Predictions\predictions_two_parameter_distributions.dta", nogen
merge 1:1 code datatype year using  "03.intermediatedata\Predictions\predictions_povcal.dta", nogen
merge 1:1 code datatype year using  "03.intermediatedata\Predictions\predictions_gic.dta", nogen

***************************
*** CLEANING FINAL DATA ***
***************************
// Create status quo series
foreach start in Y_l_head Y_l_mean Y_g_mean Y_l_medi Y_g_medi {
foreach end   in co_dire co_grow al_dire al_grow al_dnmu_grow co_dnmu_grow al_dnmd_grow co_dnmd_grow ///
				 po_dire po_grow ri_dire ri_grow ri_dnmu_grow po_dnmu_grow ri_dnmd_grow po_dnmd_grow {
foreach type  in hfc gni {
foreach pass  in n p d {
cap gen      `start'_`pass'squ_`end' = `start'_`pass'gdp_`end'
cap replace  `start'_`pass'squ_`end' = `start'_`pass'`type'_`end' if !missing(`start'_`pass'hfc_`end') & CLASS_region_povcalnet!=7
}
}
}
}

// If missing, use GDP equivalent for HFC, GNI, and FCE:
foreach start in Y_l_head Y_l_mean Y_g_mean Y_l_medi Y_g_medi {
foreach end   in co_dire co_grow al_dire al_grow al_dnmu_grow co_dnmu_grow al_dnmd_grow co_dnmd_grow ///
                 po_dire po_grow ri_dire ri_grow ri_dnmu_grow po_dnmu_grow ri_dnmd_grow po_dnmd_grow {
foreach type  in hfc gni fce {
cap replace `start'_n`type'_`end' = `start'_ngdp_`end' if missing(`start'_n`type'_`end')
cap replace `start'_p`type'_`end' = `start'_pgdp_`end' if missing(`start'_p`type'_`end')
cap replace `start'_d`type'_`end' = `start'_dgdp_`end' if missing(`start'_d`type'_`end')
}
}
}


// All other cases just replace headcount predictions with simple GDP
foreach sample in al co ri po {
foreach var of varlist Y_l_head*`sample'* {
replace `var' = Y_l_head_ngdp_`sample'_dnmu_grow if missing(`var') & !missing(Y_l_head_ngdp_`sample'_dnmu_grow) & !strpos("`var'", "_v")
}
}

// Missing GDP data for LBN in 2021 and SYR in 2019-2021. In these case replace predictions with CIRF dire
foreach sample in al co ri po {
foreach var of varlist Y_l_head*`sample'* {
replace `var' = Y_l_head_cirf_`sample'_dnmu_grow if missing(`var') & !missing(Y_l_head_cirf_`sample'_dnmu_grow) & !strpos("`var'", "_v")
}
}

mdesc Y_l_head* if sample_al_g==1

// For the perfect predictions there can be no prediction for the nowcasting year
foreach var of varlist *perf* {
replace `var' = . if sample_now==1
}
// No labelling for now (the variable content should be clear from the variale name)
foreach var of varlist Y_*co* Y_*al* Y_*ri* Y_*po* {
label var `var' ""
}

sort code datatype year
order *, alpha
order code datatype year sample* weight* Y*
format Y_l_head* Y_l_gini* %3.1f
format Y_l_medi* Y_l_mean* %3.1f
cap drop _merge
compress
save "03.intermediatedata\Predictions\predictions_final.dta", replace