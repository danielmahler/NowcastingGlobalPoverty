/*==================================================
project:       Impute missing values (MATA functions)
Author:        Andres Castaneda 
Dependencies:  The World Bank
----------------------------------------------------
Creation Date:    20 Nov 2018 - 10:05:07
Modification Date:   
Do-file version:    01
==================================================*/

version 14

mata:
mata drop mi_*()

//---------------------------------------------
// test variable names length for imputation
//---------------------------------------------

void mi_checkName(real scalar varl) {
	
	string vector longName
	string vector nameLength
	
	V          = tokens(st_strscalar("vars"))
	longName   = select(V, strlen(V):>varl)'
	nameLength = strofreal(
										select(strlen(V), strlen(V):>varl)')
	
	if (length(longName) > 0) {
		ltitle      = strlen("Variable names longer than varl characters")
		minl        = 3                          // min space between columns
		LlongName   = max(strlen(longName))      // length of stop words
		LnameLength = strlen("Length")        // length of stop words
		
		if (LlongName + LnameLength <= ltitle) {
			n1 = abs(ltitle + minl - (LlongName + LnameLength))
			h1 = minl
		}
		else {
			n1 = minl
			h1 = abs(ltitle + minl - (LlongName + LnameLength))
		}
		
		prt = sprintf("{res}{space 6}%%-%fuds%%%fuds\n", LlongName+n1, LnameLength)
		
		printf(sprintf("{text}{center %f:{ul:Variable names longer than %f characters}}\n", 
			h1+ltitle+12, varl))  // title 
		printf(prt, "Var Name", "Length")  // subtitle 
		printf(sprintf("{text}{space 6}{hline %f}\n", h1+ltitle)) // line 
		for (i = 1; i <= rows(longName); i++) {
			printf(prt, longName[i], nameLength[i])   // content
		}
		printf(sprintf("{text}{space 6}{hline %f}\n", h1+ltitle)) // final line
	}
	else {
		printf(sprintf("{text}No variable has more than %f characters\n", varl))
	}
}


//---------------------------------------------
// Split WDI codes into sections
//---------------------------------------------
string matrix mi_split_wdi(string rowvector Vnames) { 

	string rowvector SVnames

	r = cols(Vnames)
	SVnames = sort(Vnames,1)
	for (i = 1; i<=r; i++ ) {
		T = tokens(SVnames[i], "_")  // parse by _
		C = select(T, (T :!= "_"))   // get rid of _
		
		if (i == 1) {
				Coding = C
		}
		else {
			diffcols   = cols(Coding) - cols(C)
			
			if (diffcols > 0) {
				C = C, J(1, diffcols, "")
			}
			if (diffcols < 0) {
				diffcols = abs(diffcols)
				Coding = Coding, J(rows(Coding), diffcols, "")
			}
			Coding = Coding \ C
		}
		
	}
	return(Coding[|1,2\.,.|])
}

//---------------------------------------------
// Index with minimum number of missings
//---------------------------------------------
struct mi_MinIndOut {
	real vector i     // vector with index
	real matrix w     // matrix with sequence of minimums
	real scalar m     // counter
	real vector Nmiss // Number of missing values per variable
}

struct mi_MinIndOut scalar mi_minmat(real matrix Vars, 
                                   | real scalar h ) { // Matrix with index of minimums
	
	struct mi_MinIndOut scalar s
	
	s.Nmiss    = colmissing(Vars)' 
	minindex(s.Nmiss, rows(s.Nmiss), s.i, s.w)
	if (args()==1) h = 0
	s.m = h
	return(s)
}


void mi_nimextract(struct mi_MinIndOut scalar s, 
                   string vector Vnames) {	// extract minimum m
	
	s.m = ++s.m
	n = s.w[s.m,1]+s.w[s.m,2]-1	
	st_global("lmiss", invtokens(Vnames[s.i[n..s.m]]'))
}

void mi_nomiss(struct mi_MinIndOut scalar s, 
                   string vector Vnames) {	// extract minimum m
	
	B = strofreal(s.Nmiss), Vnames'
	cvar = select(B, (B[.,1] :== "0"))[,2]
	cvar = invtokens(cvar')
	st_local("cvar", cvar)
}


string matrix mi_ttest_results(string matrix T, 
	string scalar mvar,
	string scalar tvar,
	string scalar p) {

	real scalar i, n, stepcut, step, counter

	n = strtoreal(st_local("n"))
	i = strtoreal(st_local("i"))
	
	if (rows(T) == 0) {
		stepcut = step = 1  // one percent
		T = mvar, tvar, p
	} 
	else {
		T = T \ (mvar, tvar, p)
	}

	counter=(i/n)*100

	if (counter > stepcut) {

		printf("\t{res}%3.0f%%...\n",  round(counter))

		displayflush()
		
		stepcut = stepcut + step
	}


	return(T)
}

void mi_maxmiss(real matrix Vars, 
	              string rowvector Vnames,
	              | real scalar m) {	// extract minimum m
	
	real scalar i, w

	E = colmissing(Vars)'
	
	if (args()==1) m = rows(E)   // three vars with highest No. of missings
	
	maxindex(E, m, i, w)  // get max indix

	B = strofreal(E), Vnames'  // attach variable nanes
	maxvar = B[i,2]
	maxvar = invtokens(maxvar')
	st_local("maxvar", maxvar)
}


//---------------------------------------------
//---------------------------------------------
//---------------------------------------------
//---------------------------------------------
//---------------------------------------------






end

exit




