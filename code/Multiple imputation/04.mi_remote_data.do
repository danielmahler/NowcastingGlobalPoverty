/*==================================================
project:       preapre remote data for merging into final data
Author:        R.Andres Castaneda Aguilar 
E-email:       acastanedaa@worldbank.org
url:           
Dependencies:  The World Bank
----------------------------------------------------
Creation Date:    27 Nov 2019 - 12:17:35
Modification Date:   
Do-file version:    01
References:          
Output:             
==================================================*/

/*==================================================
              0: Program set up
==================================================*/


version 16
drop _all


cap which kountry
if (_rc) ssc install kountry

cd "c:\Users\wb384996\OneDrive - WBG\Papers\global_poverty_nowcasting\02.input\"

global input "j:\Data\GLOBAL\ADMIN0_ZonalResults\"

/*==================================================
              1: DMSP
==================================================*/

*----------1.1: load data

tempfile fd
save `fd', empty

local files: dir "${input}/dmsp_Results" file "F*csv", respectcase 
disp `"`files'"'
qui foreach file of local files {
	import delimited "${input}/dmsp_Results/`file'", /* 
	 */ colrange(2:) case(lower) clear varnames(1)

	destring sum mean max sd, replace force
	drop if iso3 == ""	
	
	if regexm(`"`file'"', "F1[0-9]([0-9]+)\.csv") local year = regexs(1)
	gen year = `year'
	drop if sum == 0
	
	append using `fd'
	save `fd', replace

}

collapse  (mean) sum  mean (max) max, by(iso3 year)
gen source = "DMSP"
sort iso3 year 
save `fd', replace

*----------1.2:


/*==================================================
              2: VIIRS
==================================================*/


*----------2.1: load data

drop _all
tempfile fv
save `fv', empty

local files: dir "${input}/VIIRS_Results" file "VIIRS*csv", respectcase 
disp `"`files'"'
qui foreach file of local files {
	import delimited "${input}/VIIRS_Results/`file'", /* 
	 */ colrange(2:) case(lower) clear varnames(1)

	destring sum mean max sd, replace force
	drop if iso3 == ""	

	if regexm(`"`file'"', "_([0-9]+)([0-1][0-9])\.csv") local year = regexs(1)
	if regexm(`"`file'"', "_([0-9]+)([0-1][0-9])\.csv") local month = regexs(2)

	gen year = `year'
	gen month = `month'
	
	append using `fv'
	save `fv', replace
}
drop if sum == -1

collapse  (sum) sum (mean) mean (max) max, by(iso3 year)
gen source = "VIIRS"

append using `fd'
sort iso3 source year 



//========================================================
//  Rescalte observations in DMSP to VIIRS
//========================================================


//------------resscalling factor for common years

duplicates tag iso3 year, gen(tag)
preserve 

keep if tag == 1
drop mean tag

foreach x in sum max {
	bysort iso3 year (source): gen f`x' = `x'/`x'[_n+1]
	replace f`x' = 1 if (f`x' == .)
}

keep iso3 source f*
tempfile df2
save `df2'
restore 

merge m:1 iso3 source using `df2', gen(df2)
drop if (tag == 1 & source == "DMSP")
drop tag


//------------rescalling factors for non-common year

// I assume equal means... difficult to believe but no other assumption can be made

preserve 
keep if df2 == 1
foreach x in sum max {
	bysort iso3 source: egen m`x' = mean(`x') 
}
collapse msum  mmax, by(iso3 source)
foreach x in sum max {
	bysort iso3 (source): gen f`x' = m`x'/m`x'[_n+1]
	replace f`x' = 1 if (f`x' == .)
	drop m`x'
}

tempfile df3
save `df3'

restore

merge m:1 iso3 source using `df3', gen(df3) update

foreach x in sum max {
	gen nl_`x' = `x'/f`x' // nl = nighttime lights
}

keep iso3 year nl*

//------------Save  nighttime lights data

rename iso3 countrycode 
save "remote_data/nighttime.dta", replace


/*==================================================
              3: CHIRPS
==================================================*/


*----------3.1:
*##s
drop _all
tempfile fc
save `fc', empty

global indate = date("01 Jan 1980", "DMY") 

local files: dir "${input}/CHIRPS" file "precipSummary*csv", respectcase 
disp `"`files'"'
qui foreach file of local files {
	import delimited "${input}/CHIRPS/`file'", /* 
	 */ case(lower) clear varnames(1) colrange(3:)

	destring sum mean sd, replace force
	

	if regexm("`file'", "([0-9]+)") local date = real(regexs(1)) + ${indate}
	local month: disp %tdn `date'
	local year: disp %tdCY `date'


	gen year = `year'
	gen month = `month'
	
	append using `fc'
	save `fc', replace
}


drop if missing(mean)
drop if sum == 0

collapse  (sum) pt_sum=sum (mean) pt_mean=mean , by(wb_adm0_na year)

gen source = "CHIRPS"

kountry wb_adm0_na, from(other) stuck marker
rename _ISO3N_ iso3n
kountry iso3n , from(iso3n) to (iso3c)
rename _ISO3C_ countrycode


// drop countries without iso3c code

drop if countrycode == ""
keep countrycode year pt*


//------------save precipitation file

save "remote_data/precipitation.dta", replace

*##e

*----------3.2:





exit
/* End of do-file */

><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

Notes:
1.
2.
3.


Version Control:






local date      = c(current_date)
local time      = c(current_time)
local datetime  = clock("`date'`time'", "DMYhms")   // number, not date

local vintage:  disp %tdD-m-CY date("`c(current_date)'", "DMY")
	