
use "${input}\changes_on_changes\FinalInputData.dta", clear
rename *, lower

local N = _N
local th = .5 // threshold
disp "threshold: `=round(`N'*`th')'"
missings report,  minimum(`=round(`N'*`th')')

if ("`r(varlist)'" != "") drop `r(varlist)'



/*==================================================
           2: counting missing value
==================================================*/

*----------2.1: Extract variables with least missing values
ds weo* wdi* g* c*, has(type numeric)
ds *, has(type numeric)
local v_vars = "`r(varlist)'"

putmata Vars = (`v_vars'), replace  view         // variable labels
mata: Vnames = tokens("`v_vars'")  // variable names



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


sum `nmiss', detail

// Split WDI codes into sections
mata: Coding = mi_split_wdi(Vnames)
	

// ------------------------------------------------------------------------
// Ttests
// ------------------------------------------------------------------------



use "${input}\changes_on_changes\FinalInputData.dta", clear
rename *, lower

global dodir "${perdrive}\01.programs\02.dofile\mi"


run "${dodir}\00.setup.do"  // run set up
run "${dodir}\_make_mifunctions.do"  // load and compile mata function


local N = _N
local th = .5 // threshold
disp "threshold: `=round(`N'*`th')'"
missings report,  minimum(`=round(`N'*`th')')

if ("`r(varlist)'" != "") drop `r(varlist)'


ds weo* wdi* g* c*, has(type numeric)
ds *, has(type numeric)
local v_vars = "`r(varlist)'"

putmata Vars = (`v_vars'), replace  view         // variable labels
mata: Vnames = tokens("`v_vars'")  // variable names




mata: mi_nomiss(minin, Vnames)


ds *, has(type numeric)
local numvars = "`r(varlist)'"
local numvars: list numvars - cvar


qui misstable pattern, freq
local a = "`r(vars)'"
local b: list numvars - a
local varmiss: list numvars - b

cap drop miss_*
qui misstable sum, gen(miss_)


local union: list varmiss & numvars

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

		mata: T = mi_ttest_results(T, "`var'", "`nvar'", "`p'")
		
	}

}


// ------------------------------------------------------------------------
// 
// ------------------------------------------------------------------------


mata: mi_nomiss(minin, Vnames)



ds *, has(type numeric)
local numvars = "`r(varlist)'"
local numvars: list numvars - cvar

local cdm "= `cvar'"
local cdm ""

misstable pattern, freq
local a = "`r(vars)'"
local b: list numvars - a
local varmiss: list numvars - b

local g = 1
local wtest = "`varmiss'"
while (`g' == 1) {
	cap mcartest `wtest' `cdm', iterate(300)
	if (_rc) {
		local n: word count `wtest'
		local nw: word `n' of `wtest'
		local wtest: list wtest - nw
		local fw = "`fw' `nw'"
	}
	else {
		mcartest `wtest' `cdm', iterate(300)
		disp as text "success vars " _n as result _col(5) " `wtest'" _n(2)
		disp as text "failed vars " _n as result _col(5) " `fw'"

		local gg = 1
		local wtest2 = "`wtest'"

		while ("`fw'" != "") {
			gettoken nv fw: fw
			local wtest2 = "`wtest2' `nv'"
			cap mcartest `wtest2' `cdm' , iterate(300)
			if (_rc) {
				local wtest2: list wtest2 - nv
				local fw2 = "`fw2' `nv'"
			}
		}
		mcartest `wtest2' `cdm', iterate(300)
		disp as text "success vars round 2" _n as result _col(5) " `wtest2'" _n(2)
		disp as text "failing vars round 2" _n as result _col(5) " `fw2'"
		local g = 0
	}
}

disp "probability of not being MCAR " r(p)


mcartest  weo* , unequal emoutput nolog iterate(2000)
mcartest  weo_bca_ngdpd weo_ggx , unequal emoutput nolog iterate(2000)







program define dyngo 
dyndoc mi_weo_analysis.txt, replace
end 








sysuse nlsw88, clear
gen byte miss = missing(union, tenure)

ttest wage, by(miss)
tab miss collgrad, row chi2
*------------- end example ---



cd ~
log using tech.log,  text replace
set more off
about
sysdir
adopath
creturn list
query compilenumber
query
set trace on
capture noisily povcalnet, countr(all) clear
set trace off
log close
cd







use "${input}\changes_on_changes\FinalInputData.dta", clear
rename *, lower

run "${dodir}\00.setup.do"  // run set up
run "${dodir}\_make_mifunctions.do"  // load and compile mata function

drop  wdi* g* c* // delete this for complete analysis

local N = _N
local th = .5 // threshold
disp "threshold: `=round(`N'*`th')'"
missings report,  minimum(`=round(`N'*`th')')
local a = "`r(varlist)'"
disp "`a'"

