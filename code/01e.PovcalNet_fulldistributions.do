********************
*** INTRODUCTION ***
********************
/*
This .do-file queries PovcalNet many times in order to obtain all distributions of all surveys
This can be used to calculate median/gini's for surveys where PovcalNet does not have median/gini's, 
and for exploring poverty nowcasts with changing ginis. 
It is a bit simpler (but a bit less precise) than looping over the GMD, which doesn't have all surveys anyway
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

******************************
*** POVERTY LINES TO QUERY ***
******************************
// We want to query povertylines such that the Gini coefficients from the recovered distributions are sufficiently precise.
// It is not quite clear how many poverty lines are necessary to query
// Here I choose increments of 0.01 until 2 and then increases of 0.05% until 1000
// This is not really based on much more than intutition that it will give sufficient information.
clear
set obs 2000
gen double povertyline = _n/100
replace povertyline = povertyline[_n-1]*1.005 if povertyline>2
drop if povertyline>1000
replace povertyline = round(povertyline,0.01)
tostring povertyline, replace force
gen povertyline5 = povertyline + " " + povertyline[_n+1] + " " + povertyline[_n+2] + " " + povertyline[_n+3] + " " + povertyline[_n+4]
keep if mod(_n-1,5)==0
drop povertyline


**************************
*** QUERYING POVCALNET ***
**************************
/*
qui levelsof povertyline5
foreach lvl in `r(levels)' {
disp as error "`lvl'"
qui povcalnet, country(all) year(all) povline(`lvl') clear
// Only keeping the national estimates (with Argentina/Suriname being the exception)
qui keep if inlist(coveragetype,3,4) | inlist(countrycode,"ARG","SUR")
// Only keep relevant variables
keep countrycode datatype datayear povertyline headcount
// This line won't work for the first part of the loop but it will afterwards
cap append using "02.inputdata\PovcalNet\FullDistributions_raw.dta"
qui save         "02.inputdata\PovcalNet\FullDistributions_raw.dta", replace 
}
*/

*****************************************
*** TURNING HEADCOUNT RATES INTO PDFs ***
*****************************************
use "02.inputdata\PovcalNet\FullDistributions_raw.dta", clear
sort countrycode datayear datatype povertyline
rename countrycode code

// Adding a 0 poverty line
sum povertyline
expand 2 if abs(povertyline-`r(min)')<0.001
sort code datayear datatype povertyline
bysort code datayear datatype (povertyline): replace povertyline = 0 if _n==1
bysort code datayear datatype (povertyline): replace headcount   = 0 if _n==1 

// Sometimes the returned headcount rate is -1. This should be 1
*br if headcount==-1 | headcount[_n-1]==-1 | headcount[_n+1]==-1
sum headcount,d
replace headcount = 1 if headcount==-1

// Checking that the headcount rate always is an increasing function of the poverty rate
bysort code datayear datatype (povertyline): gen weight = headcount-headcount[_n-1]
*br if weight<-0.0001
replace weight = 0 if weight<0

// There is an error with IRN2014. 
br if weight>0.99 & !missing(weight)
replace weight = 0 if weight>0.99 & !missing(weight)

bysort code datayear datatype (povertyline): gen welfare = povertyline[_n-1]+(povertyline-povertyline[_n-1])/2
drop if weight==0
*br if missing(weight)
drop if missing(weight) // First row for each country
drop povertyline headcount
lab var code "Country code"
lab var datayear "Survey year"
lab var datatype "Datatype (1=Consumption, 2=Income)"
lab var weight "Weight"
lab var welfare "Welfare (Daily, 2011 PPP USD)"
compress

save "02.inputdata\PovcalNet\FullDistributions_processed.dta", replace

************************************************
*** COLLAPSE TO 1000 OBSERVATIONS PER SURVEY ***
************************************************
use "02.inputdata\PovcalNet\FullDistributions_processed.dta", clear

// Povsim has as error when using weights. Hence below I collapse each survey to 1000 quantiles such that I can proceed without weights.
egen survey = group(code datayear datatype)
levelsof survey
/*foreach srv in `r(levels)'*/ forvalues srv=714/1957 {
disp in red "`srv'"
pctile welf`srv'=welfare [aw=weight] if survey==`srv', nq(1001) gen(obs)
// Using the distribution above would slightly underestimate the mean and the gini, since the top 1000th quantile is ingored. Below I replace the max observation with an even higher observation, to try to deal with this. I experimented with included the max welfare by country as a separate quantile, but that overesitmates the mean and the gini. 
qui sum welfare [aw=weight] if survey==`srv', d
pctile welftemp`srv'=welfare [aw=weight] if survey==`srv' & welfare>`r(p90)', nq(1001) gen(obs_temp)
qui replace welf`srv'=welftemp`srv'[1000] if _n==1000
drop obs obs_temp welftemp*
}
 
preserve
keep survey code datayear datatype
duplicates drop
tempfile surveys
save `surveys'
restore

keep if _n<=1000
drop weight welfare survey code datayear datatype
gen obs = _n
reshape long welf, i(obs) j(survey) 
merge m:1 survey using `surveys', nogen
drop survey obs
sort code datayear datatype welf
compress
save "02.inputdata\PovcalNet\FullDistributions_collapsed.dta", replace

*****************************
*** COMPARE GINIs/MEDIANS ***
*****************************
use "02.inputdata\PovcalNet\FullDistributions_collapsed.dta", clear

/*
keep if code=="IRN"
gen logwelf = ln(welf)
bysort datayear: gen pctile = _n/10
twoway scatter logwelf pctile, by(datayear)
*/

// Calculate Gini's
egen survey = group(code datayear datatype)
gen gini_recovered = .
lab var gini_recovered "Gini index (recovered by querying full distributions)"
levelsof survey
local surveys = `r(r)'
qui ineqdec0 welf, by(survey)
forvalues srv=1/`surveys' {
disp as error "`srv'"
qui replace gini_recovered = `r(gini_`srv')' if survey==`srv'
}
// Calculate medians
bysort survey: egen median_recovered = median(welf)
// Convert median into monthly (which is what the PovcalNet ado reports)
replace median_recovered = median_recovered*365/12
lab var median_recovered "Median (recovered by querying full distributions)"

keep code datayear datatype gini_recovered median_recovered
duplicates drop

// Get PovcalNet Gini's / medians
preserve
povcalnet, country(all) year(all) clear
keep if inlist(coveragetype,3,4) | inlist(countrycode,"ARG","SUR")
keep countrycode datayear datatype gini median
rename countrycode code
rename gini gini_pcn
lab var gini_pcn "Gini index (from PovcalNet)"
rename median median_pcn
lab var median_pcn "Median (from PovcalNet)"
tempfile pcn
save `pcn'
restore

// Merge
merge 1:1 code datayear datatype using `pcn', nogen

// Compare
gen ginidif = abs(gini_recovered-gini_pcn)
gen mediandif = abs(median_recovered-median_pcn)/median_pcn*100
format gini* *dif %3.2f
format median* %3.0f
sum ginidif,d f
sum mediandif,d f
gsort -ginidif
*br
drop ginidif mediandif *_pcn
rename datayear year
order code year datatype
sort code year datatype

save "02.inputdata\PovcalNet\Ginis.dta", replace
