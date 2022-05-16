********************
*** INTRODUCTION ***
********************
/*
This .do-file first computes the predicted headcount rates assume a parametric distribution (for example lognormal) to compute the poverty rate rather than shifting the poverty line, which requires querying PovcalNet. 
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

*********************************
*** LOAD RELEVANT PREDICTIONS *** 
*********************************
use "03.intermediatedata\Predictions\predictions_step1.dta", clear
// Drop headcount rates and change/growth variables -- not needed for this exercise.
drop Y_l_head* Y_c_head* Y_g*

*******************************************
*** START LOOPING OVER PREDICTION TYPES ***
*******************************************
foreach method in perf cirf cirl carf grbo rlas plas {
disp in red "`method'"
foreach sample in al co ri po {
foreach type   in dire grow {

		******************
		*** LOG NORMAL ***
		******************
		// Finds the poverty rate by using the estimate of the mean as well as the gini for a given survey,
		// and uses a lognormal assumption to infer what poverty would be.

		// Infer the log standard deviation based on the Gini
		cap gen stdY                  = invnormal((Y_l_gini_`method'_`sample'_`type'/100+1)/2)*(2^(1/2))
		// Infer the log mean based on the mean
		cap gen meanY_mu               = ln(Y_l_mean_`method'_`sample'_`type'*12/365)-0.5*stdY^2
		cap gen meanY_md               = ln(Y_l_medi_`method'_`sample'_`type'*12/365)
		// Back out the headcount rate
		cap gen Y_l_head_`method'_`sample'_lnmu_`type' = normal((ln(1.9)-meanY_mu)/stdY)*100
		cap gen Y_l_head_`method'_`sample'_lnmd_`type' = normal((ln(1.9)-meanY_md)/stdY)*100
		cap drop stdY meanY* 

		*************************
		*** FISK DISTRIBUTION ***
		*************************
		// Finds the poverty rate by using the estimate of the mean as well as the gini for a given survey,
		// and imposes a Fisk distribution to infer what poverty would be.
		// Infer the parameters
		cap gen beta       = 1/(Y_l_gini_`method'_`sample'_`type'/100)
		cap gen alpha_md = Y_l_medi_`method'_`sample'_`type'*12/365
		cap gen alpha_mu = Y_l_mean_`method'_`sample'_`type'*12/365*beta/c(pi)*sin(c(pi)/beta) 
		// Back out the headcount rate
		cap gen Y_l_head_`method'_`sample'_llmu_`type' = 1/(1+(1.9/alpha_mu)^(-beta))*100
		cap gen Y_l_head_`method'_`sample'_llmd_`type' = 1/(1+(1.9/alpha_md)^(-beta))*100
		cap drop alpha* beta

******************************************
*** ENDS LOOPING OVER PREDICTION TYPES ***
******************************************
} 
}
}

keep code year datatype sample* weight* Y_l_head*

save  "03.intermediatedata\Predictions\predictions_two_parameter_distributions.dta", replace
