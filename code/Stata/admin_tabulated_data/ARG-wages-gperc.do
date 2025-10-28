*----------------------------------------------------------------------------*
* Code to organise administrative wage data                              
* Authors: Alvaredo, De Rosa, Flores, Morgan
* Project: DINA LATAM
* October 2019
*----------------------------------------------------------------------------*

clear

//Import total population data into tabulations
qui use "input_data/wid_population/pops.dta", clear
qui keep if country == "ARG" & year >= 1996
qui rename (npopul npopul_adults) (totalpop adultpop)

mkmat year totalpop adultpop, matrix(_mat_sum)

scalar totalpop1996=_mat_sum[1, 2]
scalar totalpop1997=_mat_sum[2, 2]
scalar totalpop1998=_mat_sum[3, 2]
scalar totalpop1999=_mat_sum[4, 2]
scalar totalpop2000=_mat_sum[5, 2]
scalar totalpop2001=_mat_sum[6, 2]
scalar totalpop2002=_mat_sum[7, 2]	
scalar totalpop2003=_mat_sum[8, 2]
scalar totalpop2004=_mat_sum[9, 2]
scalar totalpop2005=_mat_sum[10, 2]
scalar totalpop2006=_mat_sum[11, 2]
scalar totalpop2007=_mat_sum[12, 2]
scalar totalpop2008=_mat_sum[13, 2]
scalar totalpop2009=_mat_sum[14, 2]
scalar totalpop2010=_mat_sum[15, 2]
scalar totalpop2011=_mat_sum[16, 2]
scalar totalpop2012=_mat_sum[17, 2]
scalar totalpop2013=_mat_sum[18, 2]
scalar totalpop2014=_mat_sum[19, 2]
scalar totalpop2015=_mat_sum[20, 2]
scalar totalpop2016=_mat_sum[21, 2]
scalar totalpop2017=_mat_sum[22, 2]
scalar totalpop2018=_mat_sum[23, 2]
scalar totalpop2019=_mat_sum[24, 2]

// Estimate Distribution and Export

forvalues t = 1996/2015 {
	
	cap use "input_data/admin_data/ARG/Muestra-salarios/Muestra_remuneracion_`t'.dta", clear
	
	local weight "pondera"
	local income "rtot"

	replace `income'=0 if `income'==.

	tempvar ftile freq F fy cumfy L d_eq bckt_size cum_weight wy

	local poptot = totalpop`t'
	
	// Total average
	quietly sum `income' [w=`weight']
	local inc_tot = r(sum)	
	local inc_avg = `inc_tot'/`poptot'
	gsort -`income'
	quietly	gen `freq' = `weight'/`poptot'
	quietly	gen `F' = 1- sum(`freq')
	qui sort `income'

		
	// Classify obs in g-percentiles
	quietly egen `ftile' = cut(`F'), ///
		at(0.60(0.01)0.99 0.991(0.001)0.999 ///
		0.9991(0.0001)0.9999 0.99991(0.00001)0.99999 1)
				
	// Top average 
	qui gsort -`F'
	quietly gen `wy' = `income'*`weight'
	quietly gen topavg = sum(`wy')/sum(`weight')
	qui sort `F'
		
	// Interval thresholds
	quietly collapse (min) thr = `income' (mean) bckt_avg = `income' ///
		(min) topavg [w=`weight'], by (`ftile')
	qui sort `ftile'
	quietly gen ftile = `ftile'
		
	// Generate 127 percentiles from scratch
	tempfile collapsed_sum
	quietly save "`collapsed_sum'"
	clear
	quietly set obs 67
	quietly gen ftile = (60 + (_n - 1))/100 in 1/40
	quietly replace ftile = (99 + (_n - 40)/10)/100 in 41/49
	quietly replace ftile = (99.9 + (_n - 49)/100)/100 in 50/58
	quietly replace ftile = (99.99 + (_n - 58)/1000)/100 in 59/67
	quietly merge n:1 ftile using "`collapsed_sum'"
		
	// Interpolate missing info
	quietly ipolate bckt_avg ftile, gen(bckt_avg2)      
	quietly ipolate thr ftile, gen(thr2)
	quietly ipolate topavg ftile, gen(topavg2)
		
	// Fill last cases if blank
	qui sort ftile
	qui drop bckt_avg thr topavg
	quietly rename bckt_avg2 bckt_avg
	quietly rename thr2 thr
	quietly rename topavg2 topavg
	quietly sum bckt_avg, meanonly
	quietly replace bckt_avg = r(max) if missing(bckt_avg)
	quietly sum thr, meanonly
	quietly replace thr = r(max) if missing(thr) 
	quietly sum topavg, meanonly
	quietly replace topavg = r(max) if missing(topavg)
	
	qui rename bckt_avg bracketavg
		
	// Top shares  
	quietly replace ftile = round(ftile, 0.00001)
	quietly gen topshare = (topavg/`inc_avg')*(1 - ftile)  	
		
	// Total average  
	quietly gen average = .
	quietly replace average = `inc_avg' in 1		
		
	// Inverted beta coefficient
	quietly gen b = topavg/thr		
		
	// Fractile
	quietly rename ftile p
		
	// Year
	quietly gen year = `t' in 1
	
	// Write Population
	quietly gen poptot = `poptot' in 1
	
	// Write total wages
	quietly gen totinc = poptot*average in 1
	qui rename poptot totalpop

	// Order and save	
	order year totalpop totinc average p thr bracketavg topavg topshare b 
	keep year totalpop totinc average p thr bracketavg topavg topshare b	
	
	*if `t' == 2000 exit 1
	
	//ensure thresholds are always increasing...
	quietly count if thr[_n] >= thr[_n + 1] 
	while (r(N) > 0){
		tempvar bracket newbracket queue weight nweight
		quietly generate `queue' = sum(thr[_n] >= thr[_n + 1])
		quietly generate `bracket' = _n
		//We group the bracket with the one just above
		quietly gen `newbracket' = `bracket'[_n + 1 ] ///
			if (thr[_n] >= thr[_n + 1])
		quietly replace `bracket' = `newbracket' ///
			if (`queue' == 1) & (thr[_n] >= thr[_n + 1])
		//weight brackets before collapsing
		quietly gen `weight' = p[_n + 1] - p
		quietly replace `weight' = 1 - p if missing(`weight')
		quietly gen `nweight' = `poptot' * `weight'
		//collapse
		quietly collapse  (min) p thr totalpop average (mean) bracketavg (mean) topavg ///
			[w=`nweight'], by(`bracket')
		quietly count if (thr[_n] >= thr[_n + 1])
	}
	
	export excel using "input_data/admin_data/ARG/wage_ARG_`t'.xlsx", /// 
		firstrow(variables) keepcellfmt replace
}


