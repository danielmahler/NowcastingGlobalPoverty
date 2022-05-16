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
version 14
drop _all

*------------ directory paths
global netdrive "\\wbgfscifs01\gtsd\05.projects_requ\04.Global_Poverty_Nowcasting"
global main     "${netdrive}\04.Global_Poverty_Nowcasting_FY19-20"
global input "${main}\02.input"

if ("`c(hostname)'"  == "wbgmsbdat002") {
	global perdrive "d:\andres\04.Global_Poverty_Nowcasting_FY19-20"

}
else {
	if (lower("`c(username)'") == "wb384996") {
		global perdrive "c:\Users\wb384996\OneDrive - WBG\Papers\04.Global_Poverty_Nowcasting_FY19-20"
	}
	if (lower("`c(username)'") == "wb200957") {
		global perdrive ""
	}
	if (lower("`c(username)'") == "wb514665") {
		global perdrive ""
	}
}

global dodir "${perdrive}\01.programs\02.dofile\mi"


run "${dodir}\00.setup.do"  // run set up
run "${dodir}\_make_mifunctions.do"  // load and compile mata function

/*==================================================
              1: 
==================================================*/

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




mata: mi_nimextract(minin, Vnames) // extract minimum m
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


