********************
*** INTRODUCTION ***
********************
/*
This .do-file merges the different dataset created in the previous .do-files, and keeps the relevant features.
*/

*****************************
*** SET WORKING DIRECTORY ***
*****************************
// Uses the server if connected, otherwise uses local folder
// Stores the local folder in a global, such that everything will be saved there also even if using the server
*##s
if (lower("`c(username)'") == "wb514665") {
	cd "C:\Users\WB514665\OneDrive - WBG\Research\NowcastingGlobalPoverty"
}
else if (lower("`c(username)'") == "wb384996") {
	cd "c:\Users\wb384996\OneDrive - WBG\Papers\global_poverty_nowcasting"
}

*****************************
*** START WITH CLASS DATA ***
*****************************
use "02.inputdata\Class\CLASS_processed.dta", clear

**********************************
*** MERGE WITH POPULATION DATA ***
**********************************
merge 1:1 code year using "02.inputdata\Population\Population.dta", nogen
ren pop PCN_l_pop

*****************************************
*** MERGE WITH NATIONAL ACCOUNTS DATA ***
*****************************************
merge 1:1 code year using "02.inputdata\NationalAccounts\NationalAccounts_processed.dta", nogen

*************************
*** MERGE ON WEO DATA ***
*************************
merge 1:1 code year using "02.inputdata\WEO\WEO_2021_04.dta", nogen
*_merge==1: Economies not in WEO
// Drop population variable, which we take from separate file
drop WEO_l_LP
// Drop GDP/HFCE/GNI national accounts variables 
drop WEO_l_NGDP WEO_l_NGDPD WEO_l_NGDPDPC WEO_l_NGDPPC WEO_l_NGDPRPC WEO_l_NGDPRPPPPC WEO_l_NGDP_FY WEO_l_NGDP_R WEO_l_NGDP_RPCH WEO_l_PPPPC WEO_l_PPPGDP WEO_l_PPPSH

************************************
*** MERGE ON HISTORICAL WEO DATA ***
************************************
merge 1:1 code year using "02.inputdata\WEO\HISTWEO.dta", nogen

*************************
*** MERGE ON WDI DATA ***
*************************
merge 1:1 code year using "02.inputdata\WDI\WDI_processed.dta", nogen
* _merge==1: Taiwan
// Drop population variable
drop WDI_l_SPPOPTOTL
// National accounts variables
drop WDI_l_NECONPRVTCD WDI_l_NECONPRVTCN WDI_l_NECONPRVTCNAD WDI_l_NECONPRVTKD WDI_l_NECONPRVTKDZG WDI_l_NECONPRVTKN WDI_l_NECONPRVTPCKD WDI_l_NECONPRVTPCKDZG WDI_l_NECONPRVTPPCD WDI_l_NECONPRVTPPKD WDI_l_NYGDPMKTPCD WDI_l_NYGDPMKTPCN WDI_l_NYGDPMKTPCNAD WDI_l_NYGDPMKTPKD WDI_l_NYGDPMKTPKDZG WDI_l_NYGDPMKTPKN WDI_l_NYGDPMKTPPPCD WDI_l_NYGDPMKTPPPKD WDI_l_NYGDPPCAPCD WDI_l_NYGDPPCAPCN WDI_l_NYGDPPCAPKD WDI_l_NYGDPPCAPKDZG WDI_l_NYGDPPCAPKN WDI_l_NYGDPPCAPPPCD WDI_l_NYGDPPCAPPPKD WDI_l_NYGNPMKTPCD WDI_l_NYGNPMKTPCN WDI_l_NYGNPMKTPCNAD WDI_l_NYGNPMKTPKD WDI_l_NYGNPMKTPKDZG WDI_l_NYGNPMKTPKN WDI_l_NYGNPMKTPPPCD WDI_l_NYGNPMKTPPPKD WDI_l_NYGNPPCAPCN WDI_l_NYGNPPCAPCD WDI_l_NYGNPPCAPKD WDI_l_NYGNPPCAPKDZG WDI_l_NYGNPPCAPKN WDI_l_NYGNPPCAPPPCD WDI_l_NYGNPPCAPPPKD
// Drop national accounts variables in current prices that are also in constant prices
foreach type in WDI_l_NECONTOTLC WDI_l_NEDABTOTLC WDI_l_NYGDSTOTLC WDI_l_NEGDITOTLC WDI_l_NECONGOVTC ///
				WDI_l_NERSBGNFSC WDI_l_NEEXPGNFSC WDI_l_NEGDIFTOTC WDI_l_NEGDISTKBC ///
				WDI_l_NEIMPGNFSC WDI_l_NERSBGNFSC WDI_l_NVAGRTOTLC WDI_l_NVINDMANFC ///
				WDI_l_NVINDTOTLC WDI_l_NVSRVTOTLC WDI_l_NYGDPDISCC WDI_l_NYGDPFCSTC WDI_l_NYTAXNINDC {
cap drop `type'D
cap drop `type'N
}
// Convert other national accounts variables to per capita
foreach type in WDI_l_NECONTOTLK WDI_l_NEDABTOTLK WDI_l_NYGDYTOTLK WDI_l_NEGDITOTLK WDI_l_NECONGOVTK ///
				WDI_l_FMASTDOMSC WDI_l_FMASTNFRGC WDI_l_FMLBLBMNYC WDI_l_NEEXPGNFSK WDI_l_NEGDIFPRVC ///
				WDI_l_NEGDIFTOTK WDI_l_NEGDISTKBK WDI_l_NEIMPGNFSK WDI_l_NERSBGNFSK WDI_l_NVAGRTOTLK ///
				WDI_l_NVINDMANFK WDI_l_NVINDTOTLK WDI_l_NVSRVTOTLK WDI_l_NYEXPCAPMK WDI_l_NYGDPDISCK ///
				WDI_l_NYGDPFCSTK WDI_l_NYGNSICTRC WDI_l_NYGSRNFCYC WDI_l_NYTAXNINDK WDI_l_NYTRFNCTRC ///
				WDI_l_NYTTFGNFSK WDI_l_NERSBGNFSK {
cap replace `type'D = `type'D/pop
cap replace `type'N = `type'N/pop
cap local varlabnew  `=subinstr("`: var label `type'D'","(","per capita (",.)'
cap lab var `type'D "`varlabnew'"
cap local varlabnew  `=subinstr("`: var label `type'N'","(","per capita (",.)'
cap lab var `type'N "`varlabnew'"
}

// Drop poverty/inequality variables which are the ones we are nowcasting (or from the same surveys, and hence will be unavailable)
drop WDI_l_SIS* WDI_l_SIP* WDI_l_SID*
// Drop growth national accounts variables (in the spirit of not using lagged variables as features)
drop WDI_l_NECONGOVTKDZG WDI_l_NECONTOTLKDZG WDI_l_NEEXPGNFSKDZG WDI_l_NEGDIFTOTKDZG WDI_l_NEGDITOTLKDZG WDI_l_NEIMPGNFSKDZG WDI_l_NYADJNNTYKDZG WDI_l_NYADJNNTYPCKDZG
// Drop one of perfectly correlated pairs
drop WDI_l_SPURBTOTLINZS WDI_l_SPPOPTOTLMAZS

************************************
*** MERGE ON REMOTE SENSING DATA ***
************************************
merge 1:1 code year using  "02.inputdata\RemoteSensing\RemoteSensing.dta", nogen

****************************
*** MERGE ON SURVEY DATA ***
****************************
// First duplicate existing data such that code-year-datatype identifies a row
expand 2
bysort code year: gen datatype = _n
label define datatype 1 "Consumption" 2 "Income"
label values datatype datatype
lab var datatype "Income or consumption used for poverty estimate"

merge 1:1 code year datatype using "02.inputdata\PovcalNet\Survey_estimates_processed.dta"
*_merge==2: Decimal years
*_merge==1: Years without household surveys for a particular datatype
order NA* WEO* WDI*, last
// Calculating weighted features for decimal years
foreach var of varlist PCN_l_pop NA* WEO* WDI* RS* HISTWEO* {
	bysort code datatype (year): replace `var' = `var'[_n-1]*(year[_n+1]-year)+`var'[_n+1]*(year-year[_n-1]) if missing(`var') & _merge==2
}

// Assign rounded down incgroup/FCV/IDA-status/region for decimal years (averaging over strings doesn't make sense)
foreach var of varlist CLASS* economy {
	bysort code datatype (year): replace `var' = `var'[_n-1]  if missing(`var') & _merge==2
}

// Only retaining country-datatype-year combinations with a survey estimate or the nowcasting sample. 
// For countries without any datapoints we keep one row from 2000 to present
bysort code: egen povdatapoints = sum(!missing(Y_l_head)) 
keep if _merge!=1 | (inrange(year,2000,2021) & povdatapoints==0)
drop  _merge
replace datatype = . if povdatapoints==0
drop povdatapoint
duplicates drop
order economy code year sample* weight* Y* CLASS* PCN*
// Assign nowcasting year rows for countries without survey data as nowcasting samples
replace sample_now  = 1 if missing(sample_now)
foreach type in al co ri po {
replace sample_`type'_l = 0 if missing(sample_`type'_l)
replace sample_`type'_g = 0 if missing(sample_`type'_g)
}
// Extrapolation time
bysort code datatype (year): gen PCN_extrapolationtime = (year-year[_n-1])
lab var PCN_extrapolationtime "Extrapolation time"
rename comparable PCN_comparable

**************************************************************************
*** DROPPING VARIABLES WITH MANY MISSINGS IN NOWCASTING OR SURVEY YEAR ***
**************************************************************************
// Investigate missingness at nowcasting year
/*
preserve
keep if year==2020
collapse (count) CLASS* NA* WEO* WDI* RS*
gen i = 1
foreach var of varlist CLASS* NA* WEO* WDI* RS* {
rename `var' nonmissing`var'
}
reshape long nonmissing, i(i) j(indicator) string
drop if nonmissing==0
drop if nonmissing<10
restore
*/

// Dropping variables with more than 90% missing in nowcasting year
foreach var of varlist WDI* WEO* RS* {
	qui mdesc `var' if year==2020
	if `r(percent)'>90 {
		drop `var' 
	}
}

// Dropping variables with more than 90% missing in years of poverty data
foreach var of varlist WDI* WEO* RS* {
	qui mdesc `var' if !missing(Y_l_head)
	if `r(percent)'>90 {
		drop `var' 
	}
}

***************************************************
*** CREATING ANNUALIZED CHANGE/GROWTH VARIABLES ***
***************************************************

// Annualized growth variables
foreach var of varlist Y_l_mean Y_l_medi Y_l_gini PCN_l_pop NA* WEO* WDI* RS* HISTWEO* {
	bysort code datatype (year): gen t_`var' = (`var'/`var'[_n-1])^(1/PCN_extrapolationtime)-1
	local varname1    `=lower(substr("`: var label `var''",1,1))'
	local varnamerest `=lower(substr("`: var label `var''",2,.))'
	lab var t_`var' "Growth in `varname1'`varnamerest'"
}
rename t_*_l_* *_g_*

// For NA variables, only keep one growth variable per NA measure
foreach type in gdp gni hfc {
foreach unit in ppp2011 ppp2017 lcu {
cap replace NA_g_`type'_usd2010 = NA_g_`type'_`unit' if missing(NA_g_`type'_usd2010)
cap drop NA_g_`type'_`unit'
}
cap rename NA_g_`type'_usd2010 NA_g_`type'
local TYPE = upper(`"`type'"')
cap lab var NA_g_`type' "Growth in `TYPE' per capita"
}
foreach var of varlist NA_l* {
if strpos("`: var label `var''", "HFC") {
local varlabnew  `=subinstr("`: var label `var''","HFC","Household final consumption expenditure",.)'
lab var `var' "`varlabnew'"
}
}
foreach var of varlist NA_g* {
if strpos("`: var label `var''", "Household final") {
local varlabnew  `=subinstr("`: var label `var''","Household final","household final",.)'
lab var `var' "`varlabnew'"
}
}

// Annualized change variables
foreach type in "%" "percent" "Percent" "rate" "Rate" "index" "Index" "headocount" "Headcount" "ratio" "Ratio" {
foreach var of varlist Y_l_head WEO_l_* WDI_l_* RS_l_* {
if strpos("`:var la `var''","`type'") | strpos("`var'","RS") {
disp "`:var la `var''" 
cap	bysort code datatype (year): gen t_`var' = (`var'-`var'[_n-1])/PCN_extrapolationtime
	local varname1    `=lower(substr("`: var label `var''",1,1))'
	local varnamerest `=lower(substr("`: var label `var''",2,.))'
	lab var t_`var' "Change in `varname1'`varnamerest'"
}
}
}
rename t_*_l_* *_c_*


// Again dropping variable swith too many missings (some of the change variables might have too many missings)
foreach var of varlist WEO_g_* WEO_c_* WDI_g_* WDI_c_*  RS_c_* RS_g_* {
	// Dropping variables with more than 90% missing in nowcasting year
	qui mdesc `var' if year==2020
	if `r(percent)'>90 {
		drop `var' 
	}
}
foreach var of varlist WEO_g_* WEO_c_* WDI_g_* WDI_c_* RS_c_* RS_g_* {
	// Dropping variables with more than 90% missing in years of poverty data
	qui mdesc `var' if !missing(Y_l_head)
	if `r(percent)'>90 {
		drop `var' 
	}
}

// Dropping levels in LCU variables.
// Couldn't do this before, as they might still have useful information for the growth variables
// In WEO     
foreach var in WEO_l_BCA WEO_l_FLIBOR6 WEO_l_GGSB_NPGDP WEO_l_GGXCNL WEO_l_GGXWDG_NGDP WEO_l_NGDP_FY WEO_l_NGDP_RPCH WEO_l_NID_NGDP WEO_l_PCPIE WEO_l_PCPIPCH WEO_l_PPPGDP WEO_l_PPPSH WEO_l_TM_RPCH {
cap drop `var'
}

// From national accounts file
foreach var in NA_l_*lcu {
cap drop `var'
}
// From WDI
foreach type in "LCU" "lcu" {
foreach var of varlist WDI_l_* {
if strpos("`:var la `var''","`type'") {
disp "`:var la `var''" 
drop `var'
}
}
}
lab var year "Year"

// Condense non-GDP/HFCE/GNI NA growth variables
foreach type in WDI_g_NECONTOTLK WDI_g_NEDABTOTLK WDI_g_NYGDYTOTLK WDI_g_NEGDITOTLK WDI_g_NECONGOVTK ///
				WDI_g_FMASTDOMSC WDI_g_FMASTNFRGC WDI_g_FMLBLBMNYC WDI_g_NEEXPGNFSK WDI_g_NEGDIFPRVC ///
				WDI_g_NEGDIFTOTK WDI_g_NEGDISTKBK WDI_g_NEIMPGNFSK WDI_g_NERSBGNFSK WDI_g_NVAGRTOTLK ///
				WDI_g_NVINDMANFK WDI_g_NVINDTOTLK WDI_g_NVSRVTOTLK WDI_g_NYEXPCAPMK WDI_g_NYGDPDISCK ///
				WDI_g_NYGDPFCSTK WDI_g_NYGNSICTRC WDI_g_NYGSRNFCYC WDI_g_NYTAXNINDK WDI_g_NYTRFNCTRC ///
				WDI_g_NYTTFGNFSK WDI_c_NERSBGNFSK {
cap gen `type'D = .
cap gen `type'N = .
replace `type'D = `type'N if missing(`type'D)
if "`: var label `type'D'"=="" {
lab var `type'D "`: var label `type'N'"
}

drop `type'N
rename `type'D `type'
}
foreach type in WDI_g_NECONTOTLK WDI_g_NEDABTOTLK WDI_g_NYGDYTOTLK WDI_g_NEGDITOTLK WDI_g_NECONGOVTK ///
				WDI_g_FMASTDOMSC WDI_g_FMASTNFRGC WDI_g_FMLBLBMNYC WDI_g_NEEXPGNFSK WDI_g_NEGDIFPRVC ///
				WDI_g_NEGDIFTOTK WDI_g_NEGDISTKBK WDI_g_NEIMPGNFSK WDI_g_NERSBGNFSK WDI_g_NVAGRTOTLK ///
				WDI_g_NVINDMANFK WDI_g_NVINDTOTLK WDI_g_NVSRVTOTLK WDI_g_NYEXPCAPMK WDI_g_NYGDPDISCK ///
				WDI_g_NYGDPFCSTK WDI_g_NYGNSICTRC WDI_g_NYGSRNFCYC WDI_g_NYTAXNINDK WDI_g_NYTRFNCTRC ///
				WDI_g_NYTTFGNFSK WDI_c_NERSBGNFSK {
local varlabnew : variable label `type'
local varlabnew: subinstr  local varlabnew "(constant 2010 us$)" ""
local varlabnew: subinstr  local varlabnew "(constant 2010 US$)" ""
local varlabnew: subinstr  local varlabnew "(constant lcu)" ""
local varlabnew: subinstr  local varlabnew "(current us$)" "(current prices)"
label var `type' "`varlabnew'"
}

foreach var of varlist  CLASS* PCN* NA* WEO* WDI* RS*{
local varlabnew : variable label `var'
local varlabnew: subinstr  local varlabnew "gdp" "GDP"
local varlabnew: subinstr  local varlabnew "lcu" "LCU"
local varlabnew: subinstr  local varlabnew "percent " "% "
local varlabnew: subinstr  local varlabnew "(constant 2017 PPP $) " "in 2017 PPP"
label var `var' "`varlabnew'"
}

lab var WDI_g_SHSGRIRSKZS "Growth in % of people in risk of impoverishing expenditure for surgical care"
lab var WDI_c_SHSGRIRSKZS "Change in % of people in risk of impoverishing expenditure for surgical care"
lab var WDI_g_SHSGRCRSKZS "Growth in % of people in risk of catastrophic expenditure for surgical care"
lab var WDI_c_SHSGRCRSKZS "Change in % of people in risk of catastrophic expenditure for surgical care"


****************
*** FINALIZE ***
****************
order code economy year datatype sample* weight* Y* CLASS* PCN* NA* WEO* WDI* RS*
sort code datatype year
compress
save "02.inputdata\FinalInputData.dta", replace