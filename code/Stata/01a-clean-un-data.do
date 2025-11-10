//0. PRELIMINARY ------------------------------------------------------------//

//General 
clear all
run "code/Stata/00a-preamble.do"




//Table names
local TOT 		"Table 4.1- Total Economy (S.1)"
local RoW 		"Table 4.2- Rest of the world (S.2)"
local NFC 		"Table 4.3- Non-financial Corporations (S.11)"
local FC 		"Table 4.4- Financial Corporations (S.12)"
local GG 		"Table 4.5- General Government (S.13)"
local HH 		"Table 4.6- Households (S.14)"
local NPISH 	"Table 4.7- Non-profit institutions serving households (S.15)"
local corps 	" Non-Financial and Financial Corporations (S.11 + S.12)"
local CORPS 	"Table 4.8- Combined Sectors-`corps'"
local HH_NPISH 	"Table 4.9- Combined Sectors- Households and NPISH (S.14 + S.15)"
local all_IS 	"TOT HH NPISH HH_NPISH" //"TOT RoW NFC FC GG HH NPISH CORPS HH_NPISH"

//1. PREPARE AND CLEAN DATA -------------------------------------------------//

//1 UNDATA ----------------------------------------------------------------// 

local iter = 1
tempfile tf_merge1
foreach IS in `all_IS' {
	tempvar auxi1 auxi2
	qui use "input_data/sna_UNDATA/_clean/``IS''.dta", clear

	//Items & codes
	qui rename sna93_item_code i_code
	qui replace i_code = subinstr(i_code, ".", "",.) 
	qui replace i_code = subinstr(i_code, "*", "",.) 
	qui split sub_group, parse(-)
	qui gen sg2 = substr(sub_group2,2,1) 
	qui replace sg2 = "L" if strpos(sub_group2, "liabilities")
	qui replace sg2 = "A" if strpos(sub_group2, "assets")
	qui egen `auxi1' = concat(i_code sg2), punct(_)

	//Check for items with same code
	qui sort iso year series `auxi1'
	qui by iso year series `auxi1':  gen dup = cond(_N==1,0,_n)
	qui egen `auxi2' = concat(`auxi1' dup) if dup > 0
	qui replace `auxi2' = `auxi1' if dup == 0
	qui replace `auxi2' = subinstr(`auxi2', "-", "",.) 
	qui levelsof `auxi2', local(vars)
	
	//Get labels
	qui egen item_lab = concat(sub_group2 item), punct(", `IS' (UN-DATA): ")
	qui levelsof `auxi2', local(lab_items)
	foreach i in `lab_items' {
		qui levelsof item_lab if `auxi2' == "`i'", local(lab_item_`i') clean 
	}

	//Reshape
	qui keep iso year series `auxi2' value
	qui reshape wide value, i(iso year series) j(`auxi2') string
	qui rename value* `IS'_*
	foreach i in `lab_items' {
		qui label var `IS'_`i' "`lab_item_`i''"
	}
	
	//Save and merge
	if (`iter' == 0) {
		qui mer 1:m iso year series using "`tf_merge1'", nogenerate 
	}
	local iter = 0
	qui save `tf_merge1', replace 
}

qui sort iso series year
qui kountry iso, from(iso2c) to(iso3c) geo(undet)

//Get net balance of primary incomes 
foreach IS in  "TOT" {
	local s1 "1"
	local s2 ""
	if ("`IS'" == "TOT") {
		local s1 "U"
		local s2 "U"
	}
	qui gen `IS'_B5n_`s1' = `IS'_B5g_`s1' - `IS'_K1_`s2'
}

//Keep only LATAM	
qui keep if inlist(GEO, "Caribbean", "South America", "Central America")
qui egen ctry_srs = concat(iso series)
qui encode ctry_srs, gen(ctry_srs_n)
qui sort iso series year
qui xtset ctry_srs_n year 

//Fill missing values and special cases
foreach cod in "D4" "B2g" "B5g" "D5" {
	foreach x in "U" "R" {
		cap qui replace HH_`cod'_`x' ///
			= HH_NPISH_`cod'_`x' ///
			if missing(HH_`cod'_`x') & ///
			!missing(HH_NPISH_`cod'_`x')
		local x "1"	
		cap qui replace CORPS_`cod'_`x' ///
			= NFC_`cod'_`x' + FC_`cod'_`x' ///
			if missing(CORPS_`cod'_`x') & ///
			!missing(NFC_`cod'_`x', FC_`cod'_`x')
	}
}

//Save
tempfile tf_main 
qui save "intermediary_data/national_accounts/sna-un.dta", replace 

