/*====================================================================
project:       Create bare repository
Author:        Andres Castaneda 
Dependencies:  The World Bank
----------------------------------------------------------------------
Creation Date:    15 Nov 2018
Modification Date:   
Do-file version:    01
References:          
Output:             
====================================================================*/

/*====================================================================
0: Program set up
====================================================================*/


local dir "\\wbgfscifs01\gtsd\05.projects_requ\04.Global_Poverty_Nowcasting"
local pkg "04.Global_Poverty_Nowcasting_FY19-20"
local pause pause 

local dir:   subinstr local dir "\" "/", all
local path   "`dir'/`pkg'"
local path2: subinstr local path "/" "\", all
disp "`path'"
disp "`path2'"


if ("`pause'" == "pause") local pause "& pause"

mata: st_local("direx", strofreal(direxists("`path'.git")))

if ("`direx'" != "1"){
	shell git init "`path'"                       /* 
	*/ & git clone --bare -l "`path'" "`path'.git" /* 
	*/ & attrib +s +h "`path2'.git" `pause'
}



exit 



