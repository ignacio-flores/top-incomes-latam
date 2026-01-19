
forvalues t = 2009/2014 { // define the range of years you have

	//
	di as result "crunching MEX-`t' admin data (total income)..." _continue

	//get total population 
	qui use "input_data/wid_population/pops.dta", clear 
	qui sum npopul_adults if country == "MEX" & year == `t'
	local totalpop = r(mean)
	
	local taxfile ///
		"input_data/admin_data/MEX/Database_taxfiles/Mexico`t'.dta" 
	qui use `taxfile', clear

	// Define income variable 
	//(post-tax pre-deduction income, except for deductions of expenses needed to incur income)
	qui include "code/Stata/auxiliar/build_tot_inc_mex.do"
			
	local y "income_3"
	qui replace `y'=0 if `y'<=0

	qui rename 	de_isr_causado	tax
	qui gen eff_tax_rate = tax / `y'
	qui replace eff_tax_rate = 0 if eff_tax_rate == .
	qui replace eff_tax_rate = 0.3 if eff_tax_rate > 0.3

	
	tempvar weight ftile freq F fy cumfy L d_eq bckt_size cum_weight wy	
	qui gen `weight' = 1
	qui gen poptot = `totalpop'
	
	// Get total average
	quietly sum `y'
	local inc_tot = r(sum)	
	qui gsort -`y'
	quietly	gen `freq' = `weight'/poptot
	quietly	gen `F' = 1- sum(`freq')
	qui sort `y'
	
	// Classify obs in g-percentiles
	quietly egen `ftile' = ///
		cut(`F'), at(0.98(0.01)0.99 0.991(0.001)0.999 ///
		0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)

	// Top average 
	qui gsort -`F'
	quietly gen `wy' = `y'*`weight'
	quietly gen topavg = sum(`wy')/sum(`weight')
	qui sort `F'
		
	// Collapse 
	quietly collapse ///
		(mean) eff_tax_rate (min) poptot (min) thr = `y' ///
		(mean) bckt_avg = `y' (min) topavg [w=`weight'], ///
		by (`ftile')
	qui sort `ftile'
	quietly gen ftile = `ftile'
		
	// Generate top shares  
	quietly replace ftile = round(ftile, 0.00001)  	

	// Inverted beta coefficient
	quietly gen b = topavg/thr		
		
	// Fractile
	quietly rename ftile p
		
	// Year
	quietly gen year = `t' in 1
		
	// Order and save	
	qui rename bckt_avg bracketavg
	qui rename  poptot totalpop
	qui order year p thr bracketavg topavg b totalpop eff_tax_rate
	qui keep year p thr bracketavg topavg  b totalpop eff_tax_rate
	
	qui drop if thr == 0 
	
	qui export excel using ///
		"input_data/admin_data/MEX/_clean/total-pre-MEX-`t'.xlsx", ///
		firstrow(variables) keepcellfmt replace 

	di as text " done"	
}	
	
