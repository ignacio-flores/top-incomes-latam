/*=============================================================================*
Goal: Import and prepare Dominican tax data for combination with Survey
*=============================================================================*/

run "code/Stata/00a-preamble.do"

//General----------------------------------------------------------------------- 
forvalues year = 2012/2020 {
	
	qui import excel ///
		"input_data/admin_data/DOM/IR-1 consolidado (2012-2020).xlsx", /// 
		sheet("Año `year'") cellrange("C4:X32") clear
	
	//rename variables
	quietly rename (C D E F G H I J K L M N O P Q R S T U V W X) ///
		(declar thr totdec wage divs interest rent otherinc expens exemp ///
		taxdue sscont housasset agroasset stockasset cashasset liab ///
		men women capitalinc indepinc dependinc)
	
	*original tables contain an error, where n = 19 equals the sum of 
	*following brackets 
	qui drop in 19	
		
	//gen variables of interest
	quietly gen bracketavg = totdec/declar
	
	qui gen year=`year' in 1
	qui egen suminc=total(totdec)
	qui egen totaldeclar=total(declar)
	qui gen average=suminc/totaldeclar
	
	foreach inc in wage divs interest rent otherinc expens exemp ///
		taxdue sscont capitalinc indepinc dependinc ///
		housasset agroasset stockasset cashasset liab {
		qui gen sh_`inc' =  `inc' / totdec 
	}
	
	*qui gen auxchk = -sh_expens
	*qui egen check1 = rowtotal(sh_capitalinc sh_indepinc sh_dependinc auxchk) 
	
	//keep variables of interest
	qui order year average suminc declar thr bracketavg sh_*
	qui keep year average suminc declar thr bracketavg sh_*
	
	tempfile tab_`year'
	quietly save `tab_`year'', replace
	
	qui use "input_data/wid_population/pops.dta", clear	
		
	qui keep if iso == "DO" 	
	quietly sum npopul if year == `year' 
	local totalpop = r(mean)
	
	qui use `tab_`year'' , clear
	tempvar freq cumfreq 
	
	//Obtaining population totals, frequencies and cumulative frequencies
	quietly gen totalpop=`totalpop'
	qui gsort - bracketavg
	quietly gen `freq'=declar/totalpop
	quietly	gen `cumfreq' = sum(`freq')
	
	//percentiles
	quietly gen p = 1 - `cumfreq'
	qui sort bracketavg
	qui sort p
	
	qui gen country="DOM" in 1
	qui gen component = "pretax" in 1 
	qui replace average = suminc / totalpop
	qui replace totalpop = . if _n != 1 
	qui replace average = . if _n != 1 
	local lister year country component totalpop average p thr bracketavg
	qui keep `lister' sh_tax*
	qui order `lister'
	
	require_dir, path("input_data/admin_data/DOM/_clean")
	cap export excel `lister' using ///
		"input_data/admin_data/DOM/_clean/total-pre-DOM.xlsx", ///
		sheet("`year'", replace) firstrow(variables) keepcellfmt 	
		
	qui replace bracketavg = bracketavg * (1 - sh_taxdue)	
	qui replace thr = thr * (1 - sh_taxdue)	
	qui replace component = "postax" in 1 
	
	cap export excel `lister' using ///
		"input_data/admin_data/DOM/_clean/total-pos-DOM.xlsx", ///
		sheet("`year'", replace) firstrow(variables) keepcellfmt 
		
	di as text "DOM - `year' done"	
}
