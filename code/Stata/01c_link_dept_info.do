//setup 
clear all 

// 0. General settings
qui do "code/Stata/functions/aux_general.do"

//load agreste data 
qui import delimited "data/FDS_DEVELOPPE_2/clean/dep_subset.csv", clear 
qui replace dep = "101" if dep == "2A"
qui replace dep = "102" if dep == "2B"
qui destring dep, replace 
qui rename dep dep_code 
tempfile tf_agr 
qui save `tf_agr' 	

//load dep reference codes 
run "code/Stata/functions/load_dep_codes.do"
tempfile tf_ref 
qui save `tf_ref'

//load multipanel 
qui use "data/FR/multi_panel_foods_pa28.dta", clear 

qui rename dept dep_name 

*merge reference codes to multipanel 
qui merge m:1 dep_name using `tf_ref' //, nogen 
qui drop if _merge == 2 & inlist(dep_name, "Hauts-de-Seine", "Paris")
assert _merge == 3 
qui drop _merge 

*merge rendement agreste to multipanel 
merge m:1 dep_code year using `tf_agr', keep(1 3)
tab year _merge 

qui save "data/FR/multi_panel_foods_pa28.dta", replace 
