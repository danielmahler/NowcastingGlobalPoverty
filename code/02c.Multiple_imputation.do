/*==================================================
project:       Impute missing values
Author:        R.Andres Castaneda
Dependencies:  The World Bank
----------------------------------------------------
Creation Date:     15 Nov 2018 - 10:05:07
Modification Date: 21 Jan 2020 
Do-file version:    01
References:
Output:             dta
==================================================*/

/*==================================================
0: Program set up
==================================================*/

cwf default
cls
version 16.1
drop _all
set seed 9983746
* frame reset

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
run "${dodir}/00.setup.do"  // run set up
run "${dodir}/00.make_mifunctions.do"  // load and compile mata function

//==================================================
//         counting missing value
//==================================================


use "${input}/FinalInputData.dta", clear
run "${dodir}/00.prepare_mi_data.do"  // prepare data 


//-------------------------------------------------
// Adopt new working frame
//-------------------------------------------------

* Load data
global k = 1 // whole process interation
global m = 5 // number of attempts to incorporate vars 1 by 1.

run "${dodir}/00.mi_simulation_programs.do"

frame change default

local klim = 4
while (${k} <= `klim') {
	
	use "${input}/mi/mi_mainframe${k}.dta", clear
	register 
  
	local impvars = "`r(impvars)'"
	local regvars = "`r(regvars)'"
	
	local lim = 15
	local i = 2
	local base "base`i'_${k}"
	local misscount "misscount`i'_${k}"
	local mimodelin "mimodelin`i'_${k}"
	local mimodelout "mimodelout`i'_${k}"
	local method "chained (pmm, knn(4))"
	
	* frame copy default temp, replace
	* frame copy base2 temp, replace
	frame temp: local impvars: char _dta[impvars]

	while (`"`impvars'"' != `""' & `i' <= `lim') {
		frame copy temp `base', replace
		
		if (`i' > 2) {
			local method "mvn"
		}
		
		misscount, o(`base') n(`misscount')
		local Nvars = `r(Nvars)'
		
		cap noi mi1by1 , origin(`base') mcount(`misscount') m(${m}) ///
		nvars(`Nvars') inframe(`mimodelin') outframe(`mimodelout') ///
		method("`method'")	
		
		if (_rc != 0) {
			noi disp in red "mi1by1 failed on `base' to `mimodelin'"
			continue, break
		}
		
    noi disp "" _n
		cap noi cleanprepare, old(`mimodelout') new(temp)
		
		if (_rc != 0) {
			noi disp in red "Error on cleanprepare using frame `mimodelout'"
			continue, break 
		}
		
		local impvars  = "`r(impvars)'"
		local regvars  = "`r(regvars)'"
		
		local i = `i' + 1
		
		local base = "base`i'_${k}"
		local misscount "misscount`i'_${k}"
		local mimodelin "mimodelin`i'_${k}"
		local mimodelout "mimodelout`i'_${k}"
	}  // end of while to impute variables
	
	//========================================================
	// Join all imputed data frames
	//========================================================
	
	
	cwf default
	cap frame drop joins
	frame create joins
	cwf joins
	local files: dir "${input}/mi/" files "mi_mimodel*_${k}.dta",  respectcase
	disp `"`files'"'
	
	local i = 0
	qui foreach file of local files {
		local j = `i'
		local i = `i' + 1
		use "${input}/mi/`file'", clear
		mi unset
		
		if (`i' != 1) {
			merge 1:1 wbcode year datatype using `tmp`j'', nogen update
		}
		
		tempfile tmp`i'
		save `tmp`i'', replace
	}
	
  // Drop variables that still have missing values
  qui missings report *_?_
  if ("`r(varlist)'" != "") {
      drop `r(varlist)'
  }
  
	save "${input}/mi/mi_joinedframe_unset${k}.dta", replace
	
	local rimp = ceil(runiform(1, 5))
	keep wbcode year datatype `: char _dta[regvars]' *_`rimp'_
	rename(*_`rimp'_) *
	
	*##e
	global k = ${k} + 1
	save "${input}/mi/mi_mainframe${k}.dta", replace
	
}


// create data to be loaded in R 
run "${dodir}/00.mi_fromDTA2R.do"  // prepare data 

mybeep

exit
/* End of do-file */


><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

Notes:
1.
2.
3.


Version Control:

