/*==================================================
project:       Impute missing values
Author:        Andres Castaneda
Dependencies:  The World Bank
----------------------------------------------------
Creation Date:    15 Nov 2018 - 10:05:07
Modification Date:
Do-file version:    01
References:
*https://datahelpdesk.worldbank.org/knowledgebase/articles/201175-how-does-the-world-bank-code-its-indicators


Output:             dta
==================================================*/

/*==================================================
0: Program set up
==================================================*/
version 15
drop _all
*##s

* Personal directory
if (lower("`c(username)'") == "wb384996") {
	*global perdrive "c:\Users\wb384996\OneDrive - WBG\Papers\04.Global_Poverty_Nowcasting_FY19-20"
	global perdrive "c:\Users\wb384996\OneDrive - WBG\Papers\global_poverty_nowcasting"
}
if (lower("`c(username)'") == "wb200957") {
	global perdrive ""
}
if (lower("`c(username)'") == "wb514665") {
	global perdrive ""
}

cd "${perdrive}"
global dodir  "01.programs\02.dofile/mi"
global graph  "03.output/05.figures/mi"
global txtout "03.output/04.writeups/mi"


* Run setup and mata functions
run "${dodir}/00.setup.do"  // run set up
run "${dodir}/_make_mifunctions.do"  // load and compile mata function

//==================================================
//         counting missing value
//==================================================

* Load data
use "${input}\changes_on_changes\FinalInputData.dta", clear
rename *, lower

rename code wbcode 
drop  wdi* g* c* // delete this for complete analysis

local N = _N
local th = .5 // threshold
disp "threshold: `=round(`N'*`th')'"
missings report,  minimum(`=round(`N'*`th')')

if ("`r(varlist)'" != "") drop `r(varlist)'


ds *, has(type numeric)
local numvars = "`r(varlist)'"

qui misstable pattern, freq
local a = "`r(vars)'"
local b: list numvars - a
local varmiss: list numvars - b
disp "`varmiss'"


putmata Vars = (`numvars'), replace  view         // variable labels
mata: Vnames = tokens("`numvars'")  // variable names


// Matrix with index of minimums
mata: minin = mi_minmat(Vars) // Add second argument h to change the minimum from 1 to h-1
mata: A = minin.Nmiss

* Histogram
tempvar nmiss
getmata `nmiss' = A, force
count
replace `nmiss' = `nmiss'/r(N)
label var `nmiss'  "proportion of missing values"
hist `nmiss', bin(50) percent
sum `nmiss', detail


// Split WDI codes into sections
mata: Coding = mi_split_wdi(Vnames)

mata: mi_nomiss(minin, Vnames)

local numvars: list numvars - cvar  // complete cases

// ----------------------------------------------------
// t-tests   
// ----------------------------------------------------


local ttest = 0
if (`ttest' == 1) {
	cap drop miss_*
	qui misstable sum, gen(miss_)
	
	
	local union: list varmiss & numvars
	*##e
	global nw = wordcount("`numvars'")
	
	local n = wordcount("`varmiss'")*wordcount("`numvars'") - wordcount("`union'")
	
	mata: T = J(0,0, "")
	local i  = 0
	foreach var of local varmiss {
		local covars: list numvars - var
		foreach nvar of local covars {
			local ++i
			
			*display _newline(3) "ttest of `nvar' by missingness of `var'"
			cap ttest `nvar', by(miss_`var')
			
			if (_rc) local p = "."
			else local p = strofreal(round(r(p), .0001))
			
			noi mata: T = mi_ttest_results(T, "`var'", "`nvar'", "`p'")
			
		}
		
	}
	
	mata: t = strtoreal(T[,3]); /*
	*/   T2 = T[selectindex(t:<0.05),]
	
	preserve
	drop _all
	getmata (missvar numvars p)=T2, replace force
	destring p, replace force
	
	contract missvar, freq(n)
	replace missvar = subinstr(missvar, "weo_", "", 1)
	
	local nw = ${nw}
	
	graph hbar n, over(missvar, sort(n) reverse) yline(`nw') /*
	*/  yscale(range(0 `=`nw'+5'))  ylabel(0(`=round(`nw'/5)')`=`nw'+5') /*
	*/  ytitle("No. of vars that do not vary between missingness", size(small)) /*
	*/  title("Variables close to MCAR", span size(medium))  intensity(25)
	restore
	drop miss_*
	
} // end of t-test



// ------------------------------------------------------------------------
// Little's MCAR TEST
// ------------------------------------------------------------------------

local mcartest = 0
if (`mcartest' == 1) {
	unab weo: weo*
	ds *, has(type numeric)
	local noweo = "`r(varlist)'"
	local exvar = "year training countrycode" // vars to exclude
	local noweo: list noweo - weo
	local noweo: list noweo - exvar
	
	local dcm "= `noweo'"
	local dcm ""
	
	misstable pattern, freq
	local a = "`r(vars)'"
	local b: list weo - a
	local weomiss: list weo - b
	
	local i = 1
	local w: word 1 of `weomiss'
	
	local g = 1
	local weotest = "`weomiss'"
	while (`g' == 1) {
		cap mcartest `weotest' `dcm', iterate(300)
		if (_rc) {
			local n: word count `weotest'
			local nw: word `n' of `weotest'
			local weotest: list weotest - nw
			local fw = "`fw' `nw'"
		}
		else {
			mcartest `weotest' `dcm', iterate(300)
			disp as text "success vars " _n as result _col(5) " `weotest'" _n(2)
			disp as text "failed vars " _n as result _col(5) " `fw'"
			
			local gg = 1
			local weotest2 = "`weotest'"
			
			while ("`fw'" != "") {
				gettoken nv fw: fw
				local weotest2 = "`weotest2' `nv'"
				cap mcartest `weotest2' `dcm' , iterate(300)
				if (_rc) {
					local weotest2: list weotest2 - nv
					local fw2 = "`fw2' `nv'"
				}
			}
			mcartest `weotest2' `dcm', iterate(300)
			disp as text "success vars round 2" _n as result _col(5) " `weotest2'" _n(2)
			disp as text "failed vars round 2" _n as result _col(5) " `fw2'"
			local g = 0
		}
	}
	
	disp "probability of not being MCAR " r(p)
	
	
	mcartest  weo* , unequal emoutput nolog iterate(2000)
	mcartest  weo_bca_ngdpd weo_ggx , unequal emoutput nolog iterate(2000)
} // end of little's mcar test

// ------------------------------------------------------------------------
// setting as panel and mi
// ------------------------------------------------------------------------


gen o_year = year // original year
replace year =  yearly(strofreal(round(year)), "Y")
format year %ty

* New ID variable
encode wbcode, gen(code)
gen id = real(strofreal(code)+"0"+strofreal(datatype))

levelsof id, local(ids)
foreach id of local ids {
	if regexm("`id'", "([0-9]+)0([12])$") {
		local code: label code `=regexs(1)'
		if (`=regexs(2)' == 1) {
			local data "C"
		}
		else local data "I"
	}
	label define id `id' "`code'-`data'", modify
}
label values id id


mi set mlong
mi xtset id year

*---------- Missing analysis 
mi misstable summarize weo*
mi misstable patterns weo*
mi misstable nested weo*


*---------- register miss variable WEO
mi register imputed `numvars'
mi register regular  `cvar' 

pwcorr weo* headcount mean, obs

/*==================================================
Checking the imputation model
==================================================*/

/* PMM is not appropriate if you have reason to believe the unobserved
values are outside the range of the observed values.
when writing  mi impute chained command we are building models,
not just listing variables to impute. */


mi impute chained (pmm, knn(10) ) weo* = mean, add(20) dryrun rseed(123456)

//==================================================
// Imputing
//==================================================


global m = 5
mi impute chained (pmm, knn(10) ) weo* = mean, add(${m}) /* 
*/ rseed(123456)  by(datatype) force /* savetrace(extrace, replace) */  // savetrace is not working

save "${input}/mi/mi_weo.dta", replace



*---------- Diagnosis
mata: mi_maxmiss(Vars, Vnames, 3)   // three variables with more missing
foreach var of local maxvar {
	midiagplots `var' if datatype == 1, combine /* 
	*/ note("")  subtitle("`var'") /* 
	*/ xtitle("") ytitle("Kdensity") 
	
	save "${graph}/midiag_`var'.png", replace
}

*##e
foreach var of local maxvar {
	mi xeq 0: sum `var'
	mi xeq 1/${m}: sum `var' if miss_`var'
	mi xeq 0: kdensity `var'; graph export chk`var'0.png, replace
	forval i=1/${m} {
		mi xeq `i': kdensity `var' if miss_`var'
	}
}



; graph export chk`var'`i'.png, replace



mi estimate: regress headcount weo*
mi estimate: lassoregress headcount weo*
mi estimate, vartable dftable



mi impute mvn `weomi' = mean, add(20)

mi impute mvn weo* , add(10)

foreach x of numlist 21/40 {
	cap noi mi impute mvn weo* = mean, add(30) prior(ridge, df(`x')) burnin(300)
}


exit

/* Two very useful entries in the Stata MI manual can be viewed by issuing findit
mi workflow and findit mi glossary and then go to the appropriate place in
the MI manual */


*----------1.1:Make sure dataset is squared

use "${input}\WDI_transposed.dta", clear
merge 1:1 countrycode year using "${input}\country_year.dta"

sum year if _merge == 3  // drop years out of WDI sample
drop if year > r(max)

missings dropobs v_* if _merge ==1, force  // drop missing
drop _merge


*----------1.2: Set multiple imputation environment
cd "${input}"

mi set mlong

missings report
scalar vars = "`r(varlist)'"
local impvars = "`r(varlist)'"

mata: mi_checkName(29)  /* Check max number of character in
variable names (mi accepts up to 29) */

mi register imputed `impvars'



/*==================================================
2: counting missing value
==================================================*/

*----------2.1: Extract variables with least missing values
qui desc v_*, varlist
global v_vars = "`r(varlist)'"
mata: Vnames = tokens(st_global("v_vars"))'  // variable names
putmata Vars = (v_*), replace  view         // variable labels

// Split WDI codes into sections
mata: Coding = mi_split_wdi(Vnames)


*----------2.2:
// Matrix with index of minimums
mata: minin = mi_minmat(Vars) // Add second argument h to change the minimum from 1 to h-1
mata: A = minin.Nmiss

tempvar nmiss
getmata `nmiss' = A, force
count
replace `nmiss' = `nmiss'/r(N)
label var `nmiss'  "proportion of missing values"
hist `nmiss', bin(50) percent


/*==================================================
3: analysis of missing values
==================================================*/

*----------3.1:
mcartest "${v_vars}"


*----------3.2:

/*==================================================
4: Find related variables in WDI by topic
==================================================*/


*----------4.1:




mata: mi_nimextract(minin, Vnames') // extract minimum m
disp "${lmiss}"


if wordcount("${lmiss}") > 1 {
	mi misstable nested ${lmiss}
	
	local i = 1
	while ("`r(stmt`i')'" != "") {
		local a: word 1 of `r(stmt`i')'
		local b = "`b' `a'"
		local ++i
	}
	
	disp "`b'"
}

*----------4.2:
mi misstable patterns  ${lmiss}, bypattern

mi misstable tree  ${lmiss}
mi misstable summarize  ${lmiss}










/*==================================================
5:
==================================================*/


*----------5.1:


*----------5.2:


/*==================================================
6:
==================================================*/


*----------6.1:


*----------6.2:





exit
/* End of do-file */

><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

Notes:
1.
2.
3.


Version Control:


s
		