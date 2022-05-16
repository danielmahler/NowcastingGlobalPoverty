/*==================================================
project:       Transpose WDI data
Author:        Andres Castaneda 
Dependencies:  The World Bank
----------------------------------------------------
Creation Date:    15 Nov 2018 - 09:57:54
Modification Date:   
Do-file version:    01
References:          
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

if (lower("`c(username)'") == "wb384996") {
	global perdrive "c:\Users\wb384996\OneDrive - WBG\Papers\04.Global_Poverty_Nowcasting_FY19-20"
}
if (lower("`c(username)'") == "wb200957") {
	global perdrive ""
}
if (lower("`c(username)'") == "wb514665") {
	global perdrive ""
}

global dodir "${perdrive}\01.programs\02.dofile"



/*==================================================
       1:  Code of countries and squared data
==================================================*/

local file "${input}\WDICountry.csv"
import delimited using "`file'", clear varnames(nonames)
rename v1 countrycode

foreach v of varlist v* {
    
    local vname = `v'[1]
    capture confirm number `vname'
    if _rc == 0 local vname v_`vname'
    local vname = strtoname("`vname'")
    rename `v' `vname'  
}

drop if length(countrycode) != 3


levelsof countrycode if Currency_Unit  == ""  /* 
 */, local(regions) clean

global g_regions: subinstr local regions " " "|", all
drop if regexm(countrycode, "${g_regions}")

contract countrycode
drop _freq
expand 60
bysort countrycode: egen year = seq()
replace year = year + 1959

compress
save "${input}\country_year.dta", replace



/*==================================================
        2:  Double Reshape of WDI 
==================================================*/

local file "${input}\WDIData.csv"
import delimited using "`file'", clear

rename v1 countryname
rename v2 countrycode
foreach v of varlist v* {
    
    local vname = `v'[1]
    capture confirm number `vname'
    if _rc == 0 local vname v_`vname'
    local vname = strtoname("`vname'")
    rename `v' `vname'  
}

drop in 1

missings dropobs, force
missings dropvars, force

* drop regions
drop if regexm(countrycode, "${g_regions}")


* drop repeated indicators
desc, varlist
local allvars = "`r(varlist)'"
loca indvar  = "Indicator_Code Indicator_Name"
local noindvar: list allvars - indvar

duplicates drop `noindvar', force

desc v_*, varlist
duplicates drop countrycode `noindvar', force

gen indicator = strtoname(Indicator_Code)

reshape long v_, i(countrycode indicator) j(year) string   /* about 20 min */

keep countrycode indicator year v_ 
reshape wide v_, i(countrycode year) j(indicator) string /* about hour and a half */

destring year, replace force 

save "${input}\WDI_transposed.dta", replace


exit
/* End of do-file */

><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

Notes:
1.
2.
3.


Version Control:


