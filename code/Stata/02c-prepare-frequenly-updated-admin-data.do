//clean admin data with yearly updates 
run "code/Stata/00a-preamble.do"
local list_noquotes : subinstr global all_countries `"""' "" , all

//CHL 
di as txt "Using R to clean chilean admin data..."
if strpos("`list_noquotes'", "CHL") > 0 {
	rcall: source("code/R/01a_clean_admin_chl.R")
} 

//BRA
di as txt "Using R to clean brazilian admin data..."
if strpos("`list_noquotes'", "BRA") > 0 {
	rcall: source("code/R/01b_clean_admin_bra.R")
}

//COL
di as txt "Using Stata to clean colombian admin data..."
if strpos("`list_noquotes'", "COL") > 0 {
	do "code/Stata/admin_tabulated_data/COL-diverse.do"
}	
