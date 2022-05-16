/*==================================================
project:       programs of mi simulation
Author:        R.Andres Castaneda 
E-email:       acastanedaa@worldbank.org
url:           
Dependencies:  The World Bank
----------------------------------------------------
Creation Date:     8 Jul 2021 - 20:04:41
Modification Date:   
Do-file version:    01
References:          
Output:             
==================================================*/

/*==================================================
0: Program set up
==================================================*/
version 16.1

discard
/*==================================================
1: Sorted table with missing values count 
==================================================*/
cap program drop misscount
program define misscount, rclass
syntax, [ Old(string) New(string)]

qui {
	
	if ("`old'" == "") {
		local old = "default"
	}
	
	if ("`new'" == "") {
		local new = "misscount"
	}
	
	
	cap frame drop `new'
	frame copy `old' `new'
	frame `new' {
		local varlist: char _dta[impvars]
		local N = _N
		mi unset
		collapse (count) `varlist'
		rename * _l*
		gen n = 1
		reshape long _l, i(n) j(var) string
		drop n 
		replace _l = (`N'-_l)/`N'
		sort _l // start with var with less missing values
		local Nvars = _N
    char _dta[Nvars]   "`Nvars'" 
	}
	
	return local Nvars = `Nvars'
	
}

end 


/*==================================================
2: MI one by one 
==================================================*/
cap program drop mi1by1
program define mi1by1, rclass
syntax , ///
[                 ///
Origin(string)    ///
MCount(string)    ///
m(integer 5)      ///
Nvars(numlist)    ///
INframe(string)   ///
OUTframe(string)  ///
method(string)    ///
noi               ///
]

qui {
	set seed 89098
  cap frame drop `outframe' // drop output frame in case it exists
  
  // first variable to impute
	local varsin = ""
  
  // if nvars is stored in frame use that instead
  frame `mcount': local Nvarschar: char _dta[Nvars]
  if ("`Nvarschar'" != "") {
    local nvars = `Nvarschar'
	}
  
  // display dots
	noi _dots 0, title(Multiple Imputation one by one) reps(`nvars')
	
  forval i = 1/`nvars' {
		
		local newvar = _frval(`mcount', var, `i') // 2nd impute var
		frame copy `origin' `inframe', replace // in frame
    
		// working on in frame
    cap frame `inframe'  {
      local callregvars  ""
/*
      
			local regvars: char _dta[regvars]
			
			if (`"`regvars'"' != `""') {
				local callregvars = " = `regvars'"
			}
			else {
				local callregvars  ""
			}
			
*/
			local impvars: char _dta[impvars]
			local mispecification "mi impute `method' `varsin'  `newvar' `callregvars', add(`m') force"
			cap `noi' `mispecification'
      
      if (_rc != 0) {        
        if (_rc == 498) { // problem with the imputed variable
          noi _dots `i' 1
          continue
				}
        else {
          error _rc
				}
			}
      
		}  // end of frame 
		
		if (_rc != 0) {
			global iob    "`i'"
			global mispecification "`mispecification'"
      noi _dots `i' 1
      noi disp _n
      continue, break
		}
		
		frame copy `inframe' `outframe', replace
		local varsin "`varsin' `newvar'"
		noi _dots `i' 0
	} // end of loop
  
	frame `outframe' {
		
   // add chars and save
		char _dta[varsin]   "`varsin'"
		char _dta[regvars]  "`regvars'"
		char _dta[impvars]  "`impvars'"
		save "${input}/mi/mi_`outframe'.dta", replace
	}
	return local varsin = "`varsin'"
  return local mispecification = "`mispecification'"
}

end

/*==================================================
3: Clean MI data for second phase 
==================================================*/
cap program drop cleanprepare
program define cleanprepare, rclass
syntax , ///
OLDframe(string)                ///
NEWframe(string)                

qui {
	frame copy `oldframe' `newframe', replace
	frame  `newframe' {
	
    set seed 89098
    
    // convert from mi to regular 
    qui mi unset
    drop mi_miss

    // find all variables that were imputed and how many
    qui ds *_?_, has(type numeric)
    global varsimp "`r(varlist)'"
    global nobs: word count ${varsimp}
    numlist "1/${nobs}"
    global ns = "`r(numlist)'"


    // New frame to create variable with name of variables imputed that 
    // will be called one by one
    cap frame drop vartreat
    frame create vartreat
    qui frame vartreat {
      set obs ${nobs}
      gen var = ""
      foreach n of global ns {
        replace var = "`: word `n' of ${varsimp}'" in `n'
      }
      
      gen m = regexs(2) if regexm(var, "(.*)_([1-9])_$")
      replace var = regexs(1) if regexm(var, "(.*)_([1-9])_$")
      contract var
      gsort -var  // to avoid break ahead
      global nvars = _N
    }
		
		// Drop original incomplete variable, replace it with the average of the 
    // imputed variables and drop the imputed variables. 
    global errs ""
    qui forvalues n = 1(1)$nvars {
      cap {
        local var = _frval(vartreat, var,  `n')
        drop `var'
        egen `var' = rowmean(`var'*)
        drop `var'_*
      }
      if (_rc) global errs "${errs}  `n'"
     }

    if ("${errs}" != "") {
      noi disp in red "check the following variables in frame vartreat" _n ///
      in green "${errs}"
      return local errs =  "${errs}"
    } 
     
    /*
    frame copy cleanprep sharemiss, replace
    frame sharemiss: sharemiss 1
    */
      
    // If there is one imputed variable with coverage higher than 99% we will 
    // take as a new regular variable so it won't be taken as a variable to be 
    // imputed in the next iteration. 
      
    // find complete variables
    sharemiss .99, preserve
    local compvars = "`r(varlist)'"
    *disp "`compvars'"

    // new regular vars
    local regvars: char _dta[regvars] // Regular vars (right hand vars on mi)
    local regvars: list  regvars | compvars
    char _dta[regvars] `regvars'
    register
    return add
    
    
	} //end of frame
	
} // end of qui

end

//========================================================
// register and save main frame
//========================================================


cap program drop register
program define register, rclass
qui {
	
	ds Y_l_* PCN_l_* NA_l_* WEO_l_* WDI_l_* RS_l_*, has(type numeric)
	local impvars = "`r(varlist)'" // level variables
	
	ds Y_* PCN_* NA_* WEO_* WDI_* RS_*, has(type numeric)
	local intvars = "`r(varlist)'"  // interest variables
	
	local firstdrop: list intvars - impvars
  if ("`firstdrop'" != "") drop `firstdrop'
  
  local regvars: char _dta[regvars]
  if ("`regvars'" != "") {
  	local impvars: list  impvars - regvars
  }
	
	// get regular variables
	ds, has(type numeric)
	local allvars = "`r(varlist)'"  
	
	global regvars: list allvars - impvars
	global impvars = "`impvars'"
	
	mi set wide
	mi xtset id year
	mi register imputed ${impvars}
	mi register regular ${regvars}
	
	char _dta[regvars] ${regvars}
	char _dta[impvars] ${impvars} /// vars to impute
  
  return local regvars = "${regvars}"
  return local impvars = "${impvars}"
	
}

end 

//========================================================
// find the share of missing values in each variable
//========================================================

cap program drop sharemiss
program define sharemiss, rclass
  syntax [anything(name=share)] , [preserve]
  
  qui {
  	
    `preserve'

    local N = _N
    qui ds *, has(type numeric)
    collapse (count) `r(varlist)'
    rename * nm* // no_miss
    gen nn = 1
    reshape long nm, i(nn) j(var) string
    drop nn
    rename nm no_miss
    gen share = no_miss/`N'
    sort share

    if ("`share'" != "") {
      keep if share >= `share'
    }

    levelsof var, local(vars) clean

    return local varlist = "`vars'"
  }
  
  
end 




exit
/* End of do-file */

><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

Notes:
1.
2.
3.


Version Control:


