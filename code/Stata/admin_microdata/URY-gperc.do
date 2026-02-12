clear

di as result "crunching Uruguayan adminisitrative microdata..."
global data "input_data/admin_data/URY"

//get total population 
qui use "input_data/wid_population/pops.dta", clear 
forvalues x = 2009/2016{
	local x2 = substr("`x'", 3, 4)
	qui sum npopul_adults if country == "URY" & year == `x'
	scalar pob_`x2' = r(mean)
	di as result "year `x' scalar pob_`x2': " pob_`x2'
}

foreach year in "09" "10" "11" "12" "13" "14" "15" "16" { //  

	di as result "Crunching URY `year'..." _continue
	use "$data/microdata/Mega20`year'_paracuadros_alt3", clear  

	*-------------------------------------------------------------------------------
	*PART I: MAIN VARIABLES
	*-------------------------------------------------------------------------------
	
	qui do "code/Stata/auxiliar/aux_URY_incomevar"
	
	
	*Add indivs so that database accounts for the entire population
	*local new_pob 	=pob_`year' - _N
	*local new = _N + `new_pob'
	*qui set obs `new'

	*Missings recoded as "0"
	qui replace tot_inc=0 if tot_inc==.

	*Shares
	qui gen sh_cap=cap_inc/tot_inc
	qui gen sh_lab=lab_inc/tot_inc
	qui gen sh_pen=pen_inc/tot_inc
	qui gen sh_mix=mix_inc/tot_inc

	*Sex
	qui gen male=0
	qui replace male=1 if sex==1
	qui label var male "Male"

	*Age groups
	qui gen age_1=0
	qui replace age_1=1 if age<40 & age!=.
	qui gen age_2=0
	qui replace age_2=1 if age>39 & age<60  & age!=.
	qui gen age_3=0
	qui replace age_3=1 if age>59  & age!=.
	qui gen age_4=0
	qui replace age_4=1 if age==. // important because there are many missings in top incomes


	tempvar poptot agg_pop freq F  
	qui gsort -tot_inc
	qui gen `poptot' = pob_`year'
	qui gen `freq' 	= 1 / `poptot'
	qui gen `F' 		= 1-sum(`freq')
	
	// Classify obs in g-percentiles
	cap drop ftile
	qui gsort -`F'
	qui sort `F'
	qui egen ftile = cut(`F'), at(0(0.01)0.99 0.991(0.001)0.999 ///
		0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)

	*-------------------------------------------------------------------------------
	*PART II: OUTPUT MATRIX
	*-------------------------------------------------------------------------------

	*Main output matrix
	mat out_mat_`year'	=J(127,17,.)
	mat tax_`year'		=J(127,2,.)

	cap drop aux
	qui gen aux = round(ftile*100000)
	qui gen p = 0 
	qui replace p = aux/100000

	cap drop countp
	qui gen countp = 0
	local x = 0
	forvalues i = 0(1000)99000 {
		local x = `x' + 1
		qui replace countp = `x' if aux == `i'
	}

	local x = 100
	forvalues i = 99100(100)99900 {
		local x = `x' + 1
		qui replace countp = `x' if aux == `i'
	}

	local x = 109
	forvalues i = 99910(10)99990 {
		local x = `x' + 1
		qui replace countp = `x' if aux == `i'
	}

	local x = 118
	forvalues i = 99991(1)100000 {
		local x = `x' + 1
		qui replace countp = `x' if aux == `i'
	}
	

	local x=0
	forvalues cent=1/127 {
		local x=`x'+1
		
		qui sum 	tot_inc, d
		local   pob_tot=r(N)
		local   average=r(mean)
		mat out_mat_`year'[`x',16]=`pob_tot'
		mat out_mat_`year'[`x',17]=`average'
		
		qui sum 	tot_inc if countp==`cent', d
		local 	aver=r(mean)
		local 	thres=r(min) + `x' // to meke sure they are ascending if equal ()
		local   pob=r(N)
		mat out_mat_`year'[`x',1]=`pob'
		mat out_mat_`year'[`x',2]=`thres'
		mat out_mat_`year'[`x',3]=`aver'

		qui sum 	tot_inc if countp >= `cent', d
		mat out_mat_`year'[`x',14]=r(mean)

		qui sum 	p if countp == `cent', d
		mat out_mat_`year'[`x',15]=r(max)

		qui sum 	male if countp==`cent', d
		local	aver_male=r(mean)
		local 	aver_female=1-r(mean)
		mat out_mat_`year'[`x',4]=`aver_male'
		mat out_mat_`year'[`x',5]=`aver_female'
		
		qui sum 	e_tax_rate if countp==`cent', d
		local	tax_rate=r(mean)
		mat tax_`year'[`x',1]=`tax_rate'
		qui sum 	e_ss_rate if countp==`cent', d
		local	ss_rate=r(mean)
		mat tax_`year'[`x',2]=`ss_rate'

		local z=5
		foreach group in "age_1" "age_2" "age_3" "age_4" {
			local z=`z'+1
			qui sum 	`group' if countp==`cent', d
			local	aver_`group'=r(mean)
			mat out_mat_`year'[`x',`z']=`aver_`group''
		}


		local z=9
		foreach group in "lab_inc" "mix_inc" "pen_inc" "cap_inc" {
			local z=`z'+1
			qui sum 	`group' if countp==`cent', d
			local	sum_`group'=r(sum)
			qui sum 	tot_inc if countp==`cent', d
			local	sum_tot_inc=r(sum)
			local 	aver_`group'=`sum_`group''/`sum_tot_inc'
			mat out_mat_`year'[`x',`z']=`aver_`group''
		}

	}


	*export the matrix--------------------------------------------------------------
	mat colnames out_mat_`year'=N thr bracketavg male female _40 _60 _ Miss_age lab_inc mix_inc pen_inc cap_inc topavg p totalpop average
	
	// Create directory if it doesnt exist 
	local dirpath "input_data/admin_data/URY/_clean"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}	
	
	clear
	svmat double out_mat_`year', names(col)
	
	qui gen country = "URY" 
	qui gen year = 20`year'	
	qui drop if missing(p)
	
	
	qui export excel ///
			"input_data/admin_data/URY/_clean/total-pre-URY-adults.xlsx", ///
			sheet("20`year'", replace) firstrow(variables) keepcellfmt 
	
}

