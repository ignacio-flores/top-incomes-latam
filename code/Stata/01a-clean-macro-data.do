//0. PRELIMINARY ------------------------------------------------------------//

//General 
clear all
run "code/Stata/00a-preamble.do"

//Table names
local TOT 		"Table 4.1 Total Economy (S.1)"
local RoW 		"Table 4.2 Rest of the world (S.2)"
local NFC 		"Table 4.3 Non-financial Corporations (S.11)"
local FC 		"Table 4.4 Financial Corporations (S.12)"
local GG 		"Table 4.5 General Government (S.13)"
local HH 		"Table 4.6 Households (S.14)"
local NPISH 	"Table 4.7 Non-profit institutions serving households (S.15)"
local corps 	" Non-Financial and Financial Corporations (S.11 + S.12)"
local CORPS 	"Table 4.8 Combined Sectors`corps'"
local HH_NPISH 	"Table 4.9 Combined Sectors Households and NPISH (S.14 + S.15)"
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
qui save `tf_main', replace 

//2. wid.world data -------------------------------------------------------// 

global areas_wid_latam  ///
		" "AR" "BR" "CL" "CO" "CR" "DO" "EC" "MX" "PE" "SV" "UY" "

//define varlist
global widvars mnninc mgdpro mnnfin mptfrr mptfrp inyixx ///
	mccshn mccmhn mcfcco mconfc mptfhr mgsmhn mgsrhn mgmxhn mprgco agninc
clear //npopul 

// Download net national income figures (constant local currency) 
qui wid, indicators(${widvars}) areas(${areas_wid_latam}) ages(999 992) clear
qui rename country iso 

//rename 
qui kountry iso, from(iso2c) to(iso3c) geo(undet)
drop if missing(_ISO3C_)
qui keep iso _ISO3C_ year variable value 
qui rename _ISO3C_ country 
qui order country year 	
	
//reshape 
reshape wide value, i(country year) j(variable) string	

//rename main variables 
qui rename value* *
qui rename (agninc992i agninc999i) (agninc_adults agninc_totpop)
qui rename *999i * //these are all macro variables (defined as 999 = total pop)
*qui rename *992i *_adults
qui rename (mnninc mconfc inyixx) (TOT_B5n_wid TOT_K1_wid priceindex)
*qui drop *99?f *99?m

//Rename other variables
qui rename (mgdpro mnnfin mptfrr mptfrp mccshn mccmhn mcfcco ///
	mptfhr mgsmhn mgsrhn mgmxhn mprgco) ///
	(gdp_wid nfi re_portf_inv_rec re_portf_inv_paid cfc_hh_surplus ///
	cfc_hh_mixed cfc_corp y_cap_tax_havens y_gos_gmix_hh ///
	y_gos_hh y_gmix_hh bpi_corp_wid)		
		
//compute current gross national income
qui gen TOT_B5g_wid = TOT_B5n_wid + TOT_K1_wid
foreach v in TOT_B5g_wid TOT_B5n_wid TOT_K1_wid gdp_wid {
	qui replace `v' = `v' * priceindex
}
qui egen cfc_hh = rowtotal(cfc_hh_surplus cfc_hh_mixed)
qui gen foreign_up_corp = re_portf_inv_rec - re_portf_inv_paid

//label variables 
qui label var gdp_wid "gross domestic product"
qui label var nfi "net foreign income"
qui label var TOT_B5g_wid "gross national income"
qui label var cfc_hh_mixed "personal depreciation on mixed income"
qui label var cfc_hh "consumption of fixed capital of households"
qui label var cfc_corp "consumption of fixed capital of corporations"
qui label var TOT_K1_wid "consumption of fixed capital of the total economy"
qui label var y_cap_tax_havens "capital income received from tax havens"
qui label var y_gos_hh "gross operating surplus of households"
qui label var y_gmix_hh "gross mixed income of households"
qui label var bpi_corp_wid "balance of primary incomes of corporations (wid)"
qui label var re_portf_inv_rec ///
	"reinvested earnings on foreign portfolio investment (received)"
qui label var re_portf_inv_paid ///
	"reinvested earnings on foreign portfolio investment (paid)"	
qui label var foreign_up_corp ///
	"net foreign reinvested earnings on portfolio investment"	
qui label var y_gos_gmix_hh ///
	"gross operating surplus and mixed income of households"
qui label var cfc_hh_surplus ///
	"personal depreciation on operating surplus"
	
//Express as shares of target total
qui gen sh_cfc_hh_surplus = cfc_hh_surplus / y_gos_hh
qui la var sh_cfc_hh_surplus ///
	"Depreciation on operating surplus, HH (% of gross value)"
qui gen sh_cfc_hh_mixed = cfc_hh_mixed / y_gmix_hh
qui la var sh_cfc_hh_mixed ///
	"Depreciation on mixed income (% of gross value)"
qui gen sh_cfc_hh = cfc_hh / y_gos_gmix_hh
qui la var sh_cfc_hh ///
	"Consumption of fixed capital of households (% of MI + OS_HH)"
qui gen sh_cfc_total = TOT_K1_wid / TOT_B5g_wid
qui la var sh_cfc_total ///
	"Total Consumption of fixed capital (% of Gross National Income)"		

qui merge 1:m iso year using `tf_main', nogenerate
qui save `tf_main', replace

//harmonize country names
qui rename (_ISO3C_ GEO) (iso3c geo)
qui kountry iso, from(iso2c) to(iso3c) geo(undet)

//Save
require_dir, path("intermediary_data")
require_dir, path("intermediary_data/national_accounts")

//Save 
tempfile last 
qui save `last'

// Harmonize country-names --------------------------------------------///	
qui import delimited using  ///
	"input_data/sna_UNDATA/iso/iso_fullnames.csv" ///
	, encoding(ISO-8859-1) clear varnames(1)	
split name, parse(",") gen(stub)
qui rename (code stub1) (iso iso_long)
drop stub2 name
qui merge 1:m iso using `last', keep(match) nogenerate

//cosmetics and save 	
order iso_long iso series year
sort iso series year 	
qui save "intermediary_data/national_accounts/sna-un-wid.dta", replace 	

