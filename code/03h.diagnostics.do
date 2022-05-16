********************
*** INTRODUCTION ***
********************
/*
This .do-file tests if the predictions are reasonable (within reasonable intervals) and whether the tuning grids are reasonable.
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

*********************************************
*** PREDICTIONS OUTSIDE LOGICAL INTERVALS ***
*********************************************
order Y*, alpha
format Y_l_me* %4.0f
format Y_l_gi* %2.0f
format Y_g* %3.2f
format Y_c* Y_l_h* %3.1f
sum Y_g*,f
foreach var of varlist Y_l_gini* {
hist `var', title(`var') name(`var',replace)
}

bysort code datatype (year): gen first = (_n==1)
mdesc if sample_now == 0 & first!=1

mdesc *_g_* *_c_* if sample_now==1 & !missing(datatype)
mdesc Y_l_*dire* if sample_now==1

******************************
*** TUNING GRID REASONABLE ***
******************************
foreach method in carf ciforest grbo {
foreach target in c_head l_head g_gini l_gini g_mean l_mean g_medi l_medi {
foreach sample in co al {
use "03.intermediatedata\Tuning\\`method'_Y_`target'_`sample'.dta", clear
gen method = "`method'"
gen sample = "`sample'"
gen target = "`target'"
cap append using `datasofar'
tempfile datasofar
save `datasofar'
}
}
} 
order method target sample

tab nrounds
tab max_depth
tab eta
tab colsample_bytree
tab subsample
tab mtryseq if method=="carf"
tab nodesizeseq


tab mtryseq sample if method=="ciforest"
tab mtryseq if substr(target,1,1)=="l" & method=="ciforest"
tab mtryseq if substr(target,1,1)!="l" & method=="ciforest"
tab minsplit sample
tab minsplit sample if substr(target,1,1)=="l"
tab minsplit sample if substr(target,1,1)!="l"