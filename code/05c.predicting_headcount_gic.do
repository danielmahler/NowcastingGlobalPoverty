*************************
***READ THE TEXT BELOW***
*************************
/*
This .do-file takes as input a vector of predicted means and ginis,
and uses the full distribution for each survey (recovered by querying PovcalNet many times)
combined with linear or convex growth incidence curves to predict the headcount rate
when implementing the changes in mean and changes in Gini.
*/

*****************************
*** SET WORKING DIRECTORY ***
*****************************
if (lower("`c(username)'") == "wb514665") {
	cd "C:\Users\WB514665\OneDrive - WBG\Research\NowcastingGlobalPoverty"
	*cd "\\wbmserccpi201\GDIM\Papers\TrendsPatterns\Do-files\ARCHIVE\Now"
}
else if (lower("`c(username)'") == "wb384996") {
	cd "c:\Users\wb384996\OneDrive - WBG\Papers\global_poverty_nowcasting"
}

**************************************************************
*** PREPARE FILE WHERE THE FINAL PREDICTIONS WILL BE SAVED ***
**************************************************************
// Un-asteriks the lines below if you want to rerurn all methods. 
*use "03.intermediatedata\Predictions\predictions_step1.dta", clear
*keep code year datatype sample* weight*
*save "03.intermediatedata\Predictions\predictions_gic.dta", replace

*************************************
*** TYPES OF METHODS TO LOOP OVER ***
*************************************
foreach method in cirf /*cirl */ rlas plas perf carf grbo { 
foreach sample in /* al co */ ri po {
foreach conv   in dire grow {
use "03.intermediatedata\Predictions\predictions_step1.dta", clear
capture confirm variable Y_l_gini_`method'_`sample'_`conv'
if !_rc {

*********************************
*** LOAD RELEVANT PREDICTIONS *** 
*********************************
// We need to convert means and ginis to headcounts so we can neglect the predicted headcounts
drop Y_l_head*
// Create lagged variables
gen gini_lag = .
gen mean_lag = .
gen year_lag = . 
// Code to make sure that the lagged variables are from the latest survey when doing multiple extrapolation years
gen year_backup = year
replace year =  . if missing(Y_l_mean)
forvalues i = 1/30 {
bysort code datatype (year): replace year_lag  = year[_n-`i']     if missing(year_lag)
bysort code datatype (year): replace mean_lag  = Y_l_mean[_n-`i'] if missing(mean_lag)
bysort code datatype (year): replace gini_lag  = Y_l_gini[_n-`i'] if missing(gini_lag)
}
drop year
rename year_backup year
// Only keeping surveys for which we want to predict the headcount (not the first by country-datatype-year)
bysort code datatype (year): drop if (_n==1 & sample_now==0) | datatype==.
// Saving the data. It will be loaded in the loop below and information for each row will be stored in locals

// Top-code Gini's at 70. Should aldready be done in other files.
replace Y_l_gini_`method'_`sample'_`conv' = 70 if Y_l_gini_`method'_`sample'_`conv'>70 & !missing(Y_l_gini_`method'_`sample'_`conv')

tempfile inputdata
save `inputdata'

// Counts number of surveys for the loop
qui count
local surveys = `r(N)'

*********************************************
*** CREATE MATRIX THAT WILL STORE RESULTS ***
*********************************************
set matsize 2000
// The .do-file below allos one to use strings in the naming of the rows and columns of matrices
run "01.code\00.matrix_rename.do"
// We will store four elements per country: Survey year, datatype, predicted headcount with convex GIC, predicted headcount with linear GIC
matrix RESULTS = J(`surveys',4,.)

**********************************
*** START LOOPING OVER SURVEYS ***
**********************************
forvalues nn=1/`surveys' { // 
		
		// Storing country-specific information which will be used during the loop
		use `inputdata', clear
		loc coun	   = code[`nn']
		loc ncyr       = year[`nn']
		loc year       = year_lag[`nn']
		loc type	   = datatype[`nn']
		loc growthmean = Y_l_mean_`method'_`sample'_`conv'[`nn']/mean_lag[`nn']-1
		loc targetgini = Y_l_gini_`method'_`sample'_`conv'[`nn']
		loc growthgini = Y_l_gini_`method'_`sample'_`conv'[`nn']/gini_lag[`nn']-1		

		// If any predicted value is missing, move to next survey
		if `growthmean'!=. & `growthgini'!=. {

		// Stores survey information in a matrix
		matrcrename RESULTS row `nn' `coun' 
		matrix RESULTS[`nn',1] = `ncyr'
		matrix RESULTS[`nn',2] = `type'
		disp as error "`method'_`sample'_`conv' --- `coun' -- `=floor(`ncyr')'"
	
		// Loads the full distribution of the survey in question
		use "02.inputdata\PovcalNet\FullDistributions_collapsed.dta", clear 
		qui keep if code=="`coun'" & floor(datayear)==floor(`year') & datatype==`type'

		*************************************
		*** CONVEX GROWTH INCIDENCE CURVE ***
		*************************************
		// Since we are using povsim.ado, and povsim implements distributional assumption through shared prosperity premiums,
		// we need to figure out how much a change in the Gini matters for shared prosperity
		// This requires calculating the share of income going to the b40.
		// This is done below, with the final share stored in the local b40share
		_pctile welf, p(40)
		local p40 = r(r1)
		qui sum welf if welf<`p40', d
		local b40sum = r(sum)
		qui sum welf, d
		local totalsum = r(sum)
		local b40share = `b40sum'/`totalsum'
		// See paper by Lakner, Mahler, Negre & Prydz for the derivation of this formula
		// This is a nice 1-1 relationship between SPP and changes in the Gini
		local spp = -`growthgini'*(1+`growthmean')*(0.4/`b40share'-1)
		*disp as error "`=round(`spp',.01)' --- `=round(`growthgini',.01)'"
		
		// If the SPP's are larger/smaller we need to do apply the change in Gini in steps
		if abs(`spp')>5 {
			if `spp'>=5 {
				local spp = 5
			}
		if `spp'<=-5 {
				local spp = -5
			}
		}
		
		// Runs povsim
		qui povsim welf, type(welfare) gic(c) growth(`growthmean') premium(`spp') repetitions(1) adjustp(1) replace ///		   
		poverty(03.intermediatedata\Povsim\povrates) line(1.9) ///
        name(03.intermediatedata\Povsim\fulldistr) 

		// Stores predicted headcount in matrix
		matrix RESULTS[`nn',3] = `r(finalpov_1)'
		
		*************************************
		*** LINEAR GROWTH INCIDENCE CURVE ***
		*************************************
		// We need to figure out what SPP to use with the linear GIC
		// Here there is no 1-1 relationship between SPP and changes in the Gini, so we find it iteratively
		// If this doesn't produce the desired Gini, we'll increase/decrease the spp by 0.01 to start with
		local step  = 0.01
		local count = 1
		local delta = 1

		// Saving the country data to be loaded before each iteration of trying a different SPP
		qui save   "03.intermediatedata\Povsim\\relevantcountryfile.dta", replace
		
		// If the difference in the desired and computed Gini is 0.001 or more, 
		// it adjusts the SPP and tries again. This continues until the Gini is close enough to the target
		
		while abs(`delta')>0.001 {
			
			// Load the relevant country data
			use "03.intermediatedata\Povsim\relevantcountryfile.dta", clear

			// The linear GIC in povsim only works with SPP's in the range of -0.2 to 0.2.
			// If the SPP's are larger/smaller we need to do apply the change in Gini in steps
			if abs(`spp')>=0.2 {
				if `spp'>=0.2 {
					local spp = 0.19
				}
				if `spp'<=-0.2 {
					local spp = -0.19
				}
				
			qui povsim welf, type(welfare) gic(l) growth(0) premium(`spp') repetitions(1) adjustp(1) replace ///
			name("03.intermediatedata\Povsim\relevantcountryfile")
			// Saves the country data adjusted by the SPP of -0.2 or 0.2.
			use "03.intermediatedata\Povsim\relevantcountryfile.dta", clear
			rename welfare1 welf
			qui save "03.intermediatedata\Povsim\relevantcountryfile.dta", replace
			}
			
			// Displays the SPP being tried at the moment
			*disp "SPP: `=round(`spp',0.001)'"						
						
			// Runs povsim		
			qui povsim welf, type(welfare) gic(l) growth(`growthmean') premium(`spp') repetitions(1) adjustp(1) replace ///
			name(03.intermediatedata\Povsim\fulldistr)
			
			// Calculates the Gini with the SPP above:
			use "03.intermediatedata\Povsim\fulldistr.dta", clear
			qui ineqdec0 welfare1
			scalar endgini = `r(gini)'
			scalar targetgini = `targetgini'/100
			// Calculates difference between computed and desired Gini
			local delta = round(targetgini-endgini,0.001)
			
			// First we try to add 0.01 (or -0.01 depending on the whether the Gini is higher or lower than the target
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
		}
			// Stores predicted headcount in matrix
			gen poor = welfare1<1.9
			qui sum poor
			matrix RESULTS[`nn',4] = `r(mean)'*100
			
			
		// End missing mean/gini loop
		}
}

**************************************************************
*** CONVERTS MATRIX WITH PREDICTED HEADCOUNTS INTO DATASET ***
**************************************************************
*If not done before; type "search svmat2, historical" and install the package
clear
set more off
svmat2 RESULTS, names(col) rnames(code)
rename c1 year
rename c2 datatype
rename c3 Y_l_head_`method'_`sample'_gicc_`conv'
rename c4 Y_l_head_`method'_`sample'_gicl_`conv'

order code datatype year

tempfile predictions
save    `predictions'

**************************************************************
*** MERGES PREDICTED HEADCOUNT RATES FOR DIFFERENT METHODS ***
**************************************************************
use "03.intermediatedata\Predictions\predictions_gic.dta", clear
merge 1:1 code year datatype using `predictions', nogen 
save "03.intermediatedata\Predictions\predictions_gic.dta", replace

} 
}
}
}