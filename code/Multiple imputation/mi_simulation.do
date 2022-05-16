/*******************
imputey.do
Example file exploring the question of imputing
the dependent variable in a regression

Written by Russell Dimond, Summer 2012 for the
Social Science Computing Cooperative at UW-Madison
*##s
********************/
version 16

drop _all
frame reset

global netdrive ""
* Personal directory
if (lower("`c(username)'") == "wb384996" & "`c(hostname)'" == "PCDGR0GZW2") {
	global perdrive "c:/Users/wb384996/OneDrive - WBG/Papers/NowcastingGlobalPoverty"
}
if (lower("`c(username)'") == "wb384996" & "`c(hostname)'" == "wbgmsddg001") {
	global perdrive "e:/PovcalNet/01.personal/wb384996/papers/NowcastingGlobalPoverty"
}

if (lower("`c(username)'") == "wb200957") {
	global perdrive ""
}
if (lower("`c(username)'") == "wb514665") {
	global perdrive ""
}

cd "${perdrive}"

global dodir  "01.code"
global graph  "03.output/05.figures/mi"
global txtout "03.output/04.writeups/mi"
global input  "02.inputdata"


* Run setup and mata functions
run "${dodir}/Andres/00.setup.do"  // run set up
run "${dodir}/Andres/_make_mifunctions.do"  // load and compile mata function
run "${dodir}/Andres/mi_simulation_programs.do"

// generate random data
// x1-x3 drawn independently from standard normal distribution
// y is sum of x's, plus normal error term
set obs 800
set seed 33321
forval i=1/10 {
	gen x`i'=invnorm(runiform())
}

egen y=rowtotal(x*)

replace y=y+invnorm(runiform())

// drop values at random
local max_miss = .95 // max percentage of missing value per variable
foreach var of varlist * {
	local rand_threshold = runiform(.9, `max_miss')
	replace `var'=. if runiform()<`rand_threshold' // random obs smaller than threshold
}

// Complete cases
forval i=1/2 {
	gen c`i'=invnorm(runiform())
}

* misstable sum, gen(miss_)

// complete cases analysis 
* reg y x* c*
ds y x*, has(type numeric)
global impvars = "`r(varlist)'"

* ds miss_* c*, has(type numeric)
ds c*, has(type numeric)
global regvars = "`r(varlist)'"  // with regular vars
global regvars = ""  // without regular vars

mi set wide
mi register imputed ${impvars}
* mi register regular ${regvars}

local impvars = "${impvars}"
local regvars = "${regvars}"

char _dta[regvars] `regvars'
char _dta[impvars] `impvars' /// vars to impute


local lim = 5
local i = 1
local base "base`i'"
local misscount "misscount`i'"
local mimodelin "mimodelin`i'"
local mimodelout "mimodelout`i'"
local method "chained (pmm, knn(5))"

global m = 5
frame copy default temp, replace
while (`"`impvars'"' != `""' | `i' > `lim') {
	frame copy temp `base', replace
	
	if (`i' > 1) {
		local method "mvn"
	}
	
	misscount, o(`base') n(`misscount')
	local Nvars = `r(Nvars)'
	
	cap noi mi1by1 , origin(`base') mcount(`misscount') m(${m}) ///
	nvars(`Nvars') inframe(`mimodelin') outframe(`mimodelout') ///
	method("`method'")	
	
	if (_rc != 0) {
    noi disp in red "my1by1 failed on `base' to `mimodelin'"
    continue, break
  }
	
	cap noi cleanprepare, old(`mimodelout') new(temp) m(${m})
	
	if (_rc != 0) {
    noi disp in red "Error on cleanprepare using frame `mimodelout'"
     continue, break 
  }
	
	local impvars  = "`r(missvars)'"
	local regvars  = "`r(nregvars)'"
	local dropvars = "`r(dropvars)'" 
	
	local i = `i' + 1
	
	local base = "base`i'"
	local misscount "misscount`i'"
	local mimodelin "mimodelin`i'"
	local mimodelout "mimodelout`i'"
}

mybeep

*##e

//-------------------------------------------
// Second phase
//-------------------------------------------

frame copy mimodelout temp, replace

frame change temp
mi unset
drop mi_miss

local impvars "${impvars}"
local varsin "${varsin}"

global missvars: list impvars - varsin

// Find missing vars (not imputed variables)
local findvars ""
foreach x of global missvars {
	local findvars "`findvars' `x'_*_"
}
ds `findvars', has(type numeric)
if ("`r(varlist)'" != "") drop `r(varlist)'


local rimp = ceil(runiform(0, ${m})) // random imputation
local nregvars ""
foreach x of global varsin {
	local nregvars "`nregvars' `x'_`rimp'_"
}


local missvars = "${missvars}"
qui ds, has(type numeric)

// drop not selected variables for imputation
local dropvars = "`r(varlist)'"
local dropvars: list  dropvars - missvars
local dropvars: list  dropvars - nregvars

// old regular vars
local regvars = "${regvars}"
local dropvars: list  dropvars - regvars
drop `dropvars'

global nregvars = "`nregvars' `regvars'"


mi set wide
mi register imputed ${missvars}
mi register regular ${nregvars}
mi describe


* mi impute mvn WDI_g_SEPRMDURS = ${nregvars}, add(${m}) force

mi impute mvn ${missvars} = ${nregvars}, add(${m}) force


exit 	