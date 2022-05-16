********************
*** INTRODUCTION ***
********************
/*
This .do-file creates the dataset with the poverty estimates, weights, and sample distinction
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

**********************************
***LOADING ALL SURVEY ESTIMATES***
**********************************
// Loading all PovcalNet data
*povcalnet, country(all) year(all) clear 
// Saving the dataset such that the code can be run without being online
*save "02.inputdata\PovcalNet\Survey_estimates_raw.dta", replace
use  "02.inputdata\PovcalNet\Survey_estimates_raw.dta", clear

// Retaining and renaming relevant variales
keep   countrycode datayear datatype coveragetype mean headcount gini median
rename coveragetype coverage
rename countrycode  code
rename datayear     year
rename headcount    head
rename median       medi

// Only keeping estimates from 1981
drop if year<1981

// Only keeping national estimates (Argentina/Suriname exception)
keep if inlist(coverage,3,4) | inlist(code,"ARG","SUR")
drop coverage

*************************************************************************
*** MERGE ON GINIS/MEDIANS RECOVERED FROM QUERYING FULL DISTRIBUTIONS ***
*************************************************************************
// In 7 cases Gini's are missing and in 50+ cases medians are missing
// In these cases, I have calculated the gini and medians by querying many poverty lines in PovcalNet,
// and thereby recovering the full distributions.
// This is done in "02.dofile\01d.PovcalNet_fulldistributions.do"
// Yet even the other Gini's I replace with the (less precise) recovered Gini's since the recovered ones are the ones we will be working with in order to implement predicted changes in the Gini's.
mdesc gini medi

// 2019 not in Ginis file
merge 1:1 code year datatype using "02.inputdata\PovcalNet\Ginis.dta", nogen keep(1 3) // _merge==2 are data points before 1981
replace gini = gini_recovered
replace medi = median_recovered if missing(medi)
drop *recovered
mdesc

// FOR SLIDES
/*
keep if inlist(code,"MLI","TGO")
keep code year mean headcount GDP
format mean %3.1f
replace headcount=100*headcount
format headcount %2.1f
replace year = round(year)
gen country = "Mali" if code=="MLI"
replace country = "Togo" if code=="TGO"
drop code
order country year
*/

***************************************
*** ADDING ON COMPARABIILITY COLUMN ***
***************************************
preserve
datalibweb, country(Support) year(2005) type(GMDRAW) surveyid(Support_2005_CPI_v05_M) filename(Survey_price_framework.dta)
keep if inlist(survey_coverage,"N") | inlist(code,"ARG","SUR")
ren datatype datatype_old
gen     datatype = 1 if inlist(datatype_old,"C","c")
replace datatype = 2 if inlist(datatype_old,"I","i")
replace rep_year=2017 if rep_year==2018 & code=="TZA"
keep code rep_year comparability datatype
rename rep_year flooryear
drop if flooryear<1981
tempfile comparability
save    `comparability'
restore

gen flooryear = floor(year)

merge 1:1 code flooryear datatype using `comparability', nogen keep(1 3)
// _merge==2 are surveys not in PovcalNet 
drop flooryear
// Filling out missings with the public version of the comparability database
*br if inlist(code,"BOL","HTI","NIC","PHL","URY")
replace comp = 0 if code=="BOL" & year==1992
replace comp = 1 if code=="HTI" & year==2012
replace comp = 0 if code=="NIC" & year<=2001
replace comp = 1 if code=="NIC" & year==2005
replace comp = 2 if code=="PHL" & year==2000
replace comp = 3 if code=="PHL" & year>=2003
replace comp = 2 if code=="URY" & year<=2005

// Generating dummy variable of comparability
// This ignores the 13 cases where comparability is not a weakly increasing function over time (when one survey is incomparable with the last survey, but comparable with a survey before that). This makes our life a lot easier at the cost of reducing the sample by 13...

bysort code datatype (year): gen comparable = (comparability == comparability[_n-1]) if _n!=1 
drop comparability
label var comparable "Spell comparable, 1=Yes, 0=No"

******************************
*** ADDING NOWCASTING ROWS ***
******************************
// Finding latest year by country
bysort code: egen lastyear = max(year)
// Adding rows from latest survey until present year (but only going back to 2000, won't care about the 90s)
preserve
// First I create a dataset with country-year observations from 2000 to the present
keep code
duplicates drop
expand 22
bysort code: gen year = _n+1999
gen sample_now = 1
tempfile nowcastadditions
save    `nowcastadditions'
restore
append using `nowcastadditions'
// Only keep observations later than last survey
gsort code lastyear
bysort code: replace lastyear = lastyear[_n-1] if missing(lastyear)
sort code year
drop if sample_now==1 & year<=lastyear
drop lastyear
// Use latest survey type going forward. If both income and consumption, use consumption
gsort code year -datatype
bysort code (year): replace datatype = datatype[_n-1] if missing(datatype)
replace sample_now = 0 if missing(sample_now)
lab var sample_now "Nowcasting sample"

****************************************************************************
*** CREATING SAMPLE INDICATOR AND WEIGHTS NOT FACTORING IN COMPARABILITY ***
****************************************************************************
                             gen sample_al_l = !missing(mean)
bysort code datatype (year): gen sample_al_g = !missing(mean) & _n!=1
lab var sample_al_l  "Full sample for level analysis"
lab var sample_al_g  "Full sample for growth analysis"
// Weights
bysort code sample_al_l: gen weight_al_l   = 1/_N if sample_al_l==1
bysort code sample_al_g: gen weight_al_g   = 1/_N if sample_al_g==1
lab var weight_al_l  "Weight for full sample for level analysis"
lab var weight_al_g  "Weight for full sample for growth analysis"
sort code datatype year

************************************************************************
*** CREATING SAMPLE INDICATOR AND WEIGHTS FACTORING IN COMPARABILITY ***
************************************************************************
                             gen sample_co_l  = !missing(mean) & comp==1
bysort code datatype (year): gen sample_co_g  = !missing(mean) & comp==1 & comp[_n-1]==1
lab var sample_co_l  "Full sample of comparable spells for level analysis"
lab var sample_co_g  "Full sample of comparable spells for growth analysis"

// Weights
bysort code sample_co_l: gen weight_co_l = 1/_N if sample_co_l==1
bysort code sample_co_g: gen weight_co_g = 1/_N if sample_co_g==1
lab var weight_co_l  "Weight for full sample of comparable spells for level analysis"
lab var weight_co_g  "Weight for full sample of comparable spells for growth analysis"

sort code datatype year

***************************************************************************************
*** CREATING SAMPLE INDICATOR AND WEIGHTS FACTORING FOR HIGH/LOW POVERTY SEPARETELY ***
***************************************************************************************
// Median poverty rate is around 5%, use that to divide sample into two
sum head [aw=weight_al_l], d
// Rich sample
                             gen sample_ri_l = !missing(mean) & head<0.05
bysort code datatype (year): gen sample_ri_g = !missing(mean) & head<0.05  & !missing(head[_n-1])
lab var sample_ri_l  "Rich sample for level analysis"
lab var sample_ri_g  "Rich sample for growth analysis"
// Rich weights
bysort code sample_ri_l: gen weight_ri_l   = 1/_N if sample_ri_l==1
bysort code sample_ri_g: gen weight_ri_g   = 1/_N if sample_ri_g==1
lab var weight_ri_l  "Weight for rich sample for level analysis"
lab var weight_ri_g  "Weight for rich sample for growth analysis"
// Poor sample
                             gen sample_po_l = !missing(mean) & head>0.05
bysort code datatype (year): gen sample_po_g = !missing(mean) & head>0.05 & !missing(head[_n-1])
lab var sample_po_l  "Poor sample for level analysis"
lab var sample_po_g  "Poor sample for growth analysis"
// Poor weights
bysort code sample_po_l: gen weight_po_l   = 1/_N if sample_po_l==1
bysort code sample_po_g: gen weight_po_g   = 1/_N if sample_po_g==1
lab var weight_po_l  "Weight for poor sample for level analysis"
lab var weight_po_g  "Weight for poor sample for growth analysis"
sort code datatype year

****************
*** FINALIZE ***
****************

replace gini = 100*gini
replace head = 100*head
format mean medi  %4.0f
format weight*    %4.2f
format gini* head %4.1f
compress

foreach var of varlist mean head medi gini {
rename `var' Y_l_`var'
}

order code year datatype Y* sample* weight*

save "02.inputdata\PovcalNet\Survey_estimates_processed.dta", replace