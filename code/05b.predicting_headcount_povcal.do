
********************
*** INTRODUCTION ***
********************
/*
This .do-file dows the following:
(1) Takes as input a predicted mean for a particular method, 
(2) Uses the growth in the predicted mean from the past survey (time t-1) to predict the headcount at time t 
The latter step is done by querying PovcalNet 10 observations at a time, so the PovcalNet ado must be installed to run this file
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

**************************************************************
*** PREPARE FILE WHERE THE FINAL PREDICTIONS WILL BE SAVED ***
**************************************************************
// Un-asteriks the lines below if you want to rerurn all methods. 
*use "03.intermediatedata\Predictions\predictions_step1.dta", clear
*keep code year datatype sample* weight* 
*save "03.intermediatedata\Predictions\predictions_povcal.dta", replace

************************************
*** SELECT METHODS TO LOOP OVER  ***
************************************
// Select the methods for which you want to nowcast poverty
foreach yvar   in mean medi { 
foreach method in /*cirf cirl */rlas  plas /*perf carf grbo*/ ///
                  /* ngdp pgdp dgdp nhfc phfc dhfc */ ///
				  /* ngni pgni dgni nsqu psqu dsqu */  ///
				  /* nfce pfce dfce sgdp igdp rgdp */  ///
				  /* v091 v092 v101 v102 v111 v112 v121 v122 v131 v132 v141 v142 */ { 
foreach sample in /* al co */ ri po  { 
foreach type   in dire grow { 
use "03.intermediatedata\Predictions\predictions_step1.dta", clear
// Check if particular prediction exists
capture confirm variable Y_l_`yvar'_`method'_`sample'_`type'
if !_rc {
cap erase `datasofar'
cap erase `predictions'
cap erase `querydata'
// Displays what method is being processed at the moment
disp as error "Y_l_`yvar'_`method'_`sample'_`type'"

************************************************
*** PREPARES THE DATA FOR POVCALNET QUERYING ***
************************************************
quietly {
keep code datatype year Y_l_`yvar' Y_l_`yvar'_`method'_`sample'_`type'
// Create coverage varaible so that it matches the format of the povcalnet ado
gen     coverage = "national" if !inlist(code,"ARG","SUR")
replace coverage = "urban"    if  inlist(code,"ARG","SUR")
// Create lagged variables (makes queyring a bit easier)
gen lagyear   = .
gen lag`yvar' = .
// Code to make sure that the lagged variables are from the latest survey when doing multiple extrapolation years
gen year_backup = year
replace year =  . if missing(Y_l_`yvar')
forvalues i = 1/30 {
bysort code datatype (year): replace lagyear    = year[_n-`i']       if missing(lagyear)
bysort code datatype (year): replace lag`yvar'  = Y_l_`yvar'[_n-`i'] if missing(lag`yvar')
}
// Only keep relevant rows for which we want to predict the headcount (i.e. not the first survey by country)
bysort code datatype (year): drop if missing(Y_l_`yvar'_`method'_`sample'_`type') | missing(lag`yvar') 
// A problem emerges for country-years with two poverty estimates (both income and consumption) where we only want one
// The 'sort' below and the variable "querynumber" are essential for later on only keeping the right datatype
sort code lagyear datatype
gen querynumber = _n
// Revert year back
drop year
rename year_backup year
// Save the datafile. It will be loaded before each povcalnet query to fetch the right country/year/coverage information
tempfile querydata
save `querydata'
}


*****************************************************
*** QUERIES POVCALNET 10 OBSERVATIONS AT THE TIME ***
*****************************************************
qui count
local totalobs = `r(N)'
// Looping through all surveys-years for which predicted headcounts are desired
// The PovcalNet ado allows for querying 50 surveys at the time
local step = 50
forvalues row = 1(`step')`totalobs' {

	// Start by having these locals empty
	loc ccc
	loc cov
	loc ic
	loc yr
	loc pyr 
	loc pline
	
	// In the locals ccc, cov, pyr, pline, I store information of the 50 (next) surveys I want to query
	forvalues obs=`row'/`=`row'+`step'-1' {
		// We have to load the data anew each time because the open data is replaced with what comes from the PovcalNet query after each query
		use `querydata', clear
		// This if assures that the finale query (where perhaps less than 50 observations are left) does not return an error
		if `obs'<=`totalobs' {
			loc ccc		`ccc'   `=code[`obs']'
			loc cov     `cov'   `=coverage[`obs']'
			loc pyr		`pyr'   `=floor(lagyear[`obs'])'
			loc pline	`pline' `=lag`yvar'[`obs']/Y_l_`yvar'_`method'_`sample'_`type'[`obs']*1.9'
		}
	}

	// Queries PovcalNet
	qui povcalnet cl, country(`ccc') year(`pyr') povline(`pline') coverage(`cov') clear 
	
	// Creates a variable maintaining the original order of the dataset.
	// This will be crucial for later on maintaining the right datatypes for each query
	gen order = `row'+_n/100

	// Keep relevant variables
	keep countrycode datatype headcount datayear order
	// In the first 10 queries the next line returns an error. 
	// After that it appends the data queried thus far
	capture append using `datasofar', nolabel
	tempfile datasofar
	qui save `datasofar'
}

******************************************
*** ASSURES THE RIGHT DATATYPE IS KEPT ***
******************************************
sort order
replace headcount = 100*headcount

rename countrycode code
rename datayear    lagyear
if "`yvar'"=="mean" {
rename headcount   Y_l_head_`method'_`sample'_dnmu_`type'
}
if "`yvar'"=="medi" {
rename headcount   Y_l_head_`method'_`sample'_dnmd_`type'
}

/*
Some queries return two poverty estimates (both income and consumption). 
The lines below generates a variable whenever the line represents a new query
We have a new query if we are in the survey-year from which we are nowcasting and that year does not have both an income and consumption survey, 
if we in the survey-year from which we are nowcasting, it has both an income and consumption survey (like HTI) and the row is an uneven number, or if we are not in the survey-year from which we are nowcasting and the row is an uneven number
*/

bysort code: egen maxyear = max(lagyear)
// Inc at max year
bysort code: gen maxyearinc = lagyear==maxyear & datatype==2
bysort code: gen maxyearcon = lagyear==maxyear & datatype==1
bysort code: egen maxyearanyinc = max(maxyearinc)
bysort code: egen maxyearanycon = max(maxyearcon)
bysort code lagyear: gen newquery = (lagyear==maxyear & (maxyearanycon==0 | maxyearanyinc==0))
bysort code lagyear: replace newquery = 1 if lagyear==maxyear & maxyearanycon==1 & maxyearanyinc==1 & mod(_n,2)==1
bysort code lagyear: replace newquery = 1 if lagyear!=maxyear & (_n==1 | _n==3)
// This generates a variable mimicking the row number of the original data
gen querynumber = sum(newquery)
// The current variable datatype is what povcalnet returned, but not necessarily what we wanted
rename datatype datatype_queried
// Merge with the original data to recover the datatype we wanted
merge m:1 querynumber using `querydata', keepusing(datatype year) nogen

 // Drop observations that were not the datatype we wanted
drop if datatype_queried != datatype




// Drop irrelevant variables for further analysis
drop datatype_queried newquery order querynumber lagyear max*

tempfile predictions
save    `predictions'


**************************************************************
*** MERGES PREDICTED HEADCOUNT RATES FOR DIFFERENT METHODS ***
**************************************************************
use "03.intermediatedata\Predictions\predictions_povcal.dta", clear
merge 1:1 code year datatype using `predictions', nogen 
save "03.intermediatedata\Predictions\predictions_povcal.dta", replace
}
}
}
}
}