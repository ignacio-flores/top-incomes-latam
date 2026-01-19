
//2. wid.world data -------------------------------------------------------// 

global areas_wid_latam  ///
		" "AR" "BR" "CL" "CO" "CR" "DO" "EC" "MX" "PE" "SV" "UY" "

//define varlist

global widvars mprgho mssbgo mssbfc msscho inyixx ///
	agninc mnninc mconfc mgdpro mnnfin mptfrr mptfrp mccshn mccmhn mcfcco ///
	mptfhr mgsmhn mgsrhn mgmxhn mprgco 

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

//harmonize country names
qui kountry iso, from(iso2c) to(iso3c) geo(undet)

//Save
require_dir, path("intermediary_data")
require_dir, path("intermediary_data/national_accounts")


//cosmetics and save 	
order country year 
sort country year 	
qui save "output/national_accounts/sna-wid.dta", replace 	
