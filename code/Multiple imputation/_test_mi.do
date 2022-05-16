/*==================================================
project:       mi example for testing
Author:        Andres Castaneda 
----------------------------------------------------
Creation Date:    20 Nov 2018 - 11:06:45
==================================================*/

/* 
Example from Bartlett and Morris (2015) 
https://www.stata-journal.com/sjpdf.html?articlenum=st0387
*/


qui {
	
	use breastcancerfull, clear  // equivalent to webuse brcancer, clear
	
	* fractional polynomial (FP) transformation
	fp generate double age^(-2 -0.5), replace scale
	fp generate double pgr^(0.5), replace scale 
	
	*Cox proportional hazards model
	noi stcox age_1 age_2 gradd1 enodes pgr_1 tam, nohr nolog
	
	* Create missing values at random. Proportion = size. 
	count 
	local TN   = r(N)
	local size = 30
	foreach var of varlist age_1 age_2 gradd1 enodes pgr_1 tam {
		tempvar x
		gen `x' = runiform()
		local N = int(`TN'*(`size')/100+.5)
		sort `x'
		replace `var' = . if _n <=`N'
	}
	
	*Cox proportional hazards model with missing data
	noi stcox age_1 age_2 gradd1 enodes pgr_1 tam, nohr nolog

	
	* set for MI
	mi set mlong
	mi register imputed age_1 age_2 pgr_1 enodes gradd1 tam  // register 
	mi impute chained (reg) age_1 age_2 pgr_1 enodes /*      impute using MICE
	 */ (logit) gradd1 tam, add(20) rseed(6934)
	
	
	* Cox proportional
	noi mi estimate: stcox age_1 age_2 gradd1 enodes pgr_1 tam, nohr  
	mi estimate, vartable dftable // Imputation Diagnostics
	
	*diagnostic plot
	midiagplots age_1, m(1/10) legend(position(6)) combine
}

exit 
/* End of do-file */

><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

