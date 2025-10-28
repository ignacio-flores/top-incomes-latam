
///////////////////////////////////////////////////////////////////////////////
//																			 //
//																			 //
//  MORE UNEQUAL OR NOT AS RICH? The Missing Half of Latin American Income	 //
//			          	De Rosa, Flores & Morgan (2022)						 //
//				    Goal: Runs every dofile in the project					 //
//																		     //
///////////////////////////////////////////////////////////////////////////////

//general settings 
macro drop _all 
clear all 

*ssc install gtools mipolate quandl renvars xls2dta sgini // search dm88_1 
*ineqdeco  wid  kountry genstack egenmore

//list codes 
***********************************************************************
global do_codes1 " "01a" "01b" "01c" " 
global do_codes2 " "02b" "02c" " //"02a"
global last_code = 2 
***********************************************************************

//report and save start time 
local start_t "$S_DATE at $S_TIME"
di as result "Started running everything working `start_t'"

//prepare list of do-files 
forvalues n = 1/$last_code {

	//get do-files' name 
	foreach docode in ${do_codes`n'} { 
			
		local do_name : dir "code/Stata/." files "`docode'*.do" 
		local do_name = subinstr(`do_name', char(34), "", .)
		global doname_`docode' "`do_name'"
	}
}	

//loop over all files  
forvalues n = 1/$last_code {
	foreach docode in ${do_codes`n'} {
		
		*********************
		do code/Stata/${doname_`docode'}
		*********************
		
		//record time
		global do_endtime_`docode' " - ended $S_DATE at $S_TIME"
		
		//remember work plan
		di as result "{hline 70}" 
		di as result "list of files to run, started `start_t'"
		di as result "{hline 70}"
		forvalues x = 1/$last_code {
			di as result "Stage nº`x'"
			foreach docode2 in ${do_codes`x'} {
				di as text "  * " "${doname_`docode2'}" _continue
				di as text " ${do_endtime_`docode2'}"
			}
			if `x' == ${last_code} di as result "{hline 70}"	
		}
	}
}

