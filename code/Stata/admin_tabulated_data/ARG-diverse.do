/*=============================================================================*
Tablas: 2.2.2.1.1 = incomes | 2.2.2.1.2 = deductions | 2.2.2.1.11 = tax
Ingresos: 1cat = renta del suelo | 2cat = renta de capitales | 3cat = ingreso
mixto (beneficios de empresas + renta comercial) | 4cat = renta de trabajo

Totales de ingreso en miles de pesos corrientes
*=============================================================================*/

local rawfd input_data/admin_data/ARG/all

//General----------------------------------------------------------------------- 
clear all

display as text "Crunching ARG diverse income tables ..."

forvalues year = 2001/2002 {
	
	display as text "`y'"

	foreach tab in "3.2.3.2.1" "3.2.3.2.2" {
	
		//define range
		if ("`tab'" == "3.2.3.2.1") & `year'==2001 local cellr "B17:L34" 
		else if ("`tab'" == "3.2.3.2.2") & `year'==2001 local cellr "B17:P34" 
		if ("`tab'" == "3.2.3.2.1") & `year'==2002 local cellr "B15:P21"
		else if ("`tab'" == "3.2.3.2.2") & `year'==2002 local cellr "B15:Q21"
		
		//import
		import excel ///
			"`rawfd'/EstadisticasTributarias`year'/AFIP/`tab'.xls", cellrange(`cellr') clear
			
		if ("`tab'" == "3.2.3.2.1" & `year'==2001)  { 	
			//Change names	
			drop G-J
			quietly rename (B C D E F K L) ///
				(desde hasta declarantes totinc tot_1_4cat importe_part_emp_qb ///
				importeded_3cat)
			drop hasta
		}	
		else if ("`tab'" == "3.2.3.2.2" & `year'==2001) { 	
			//Change names	
			drop E-O
			quietly rename (B C D P) ///
				(desde hasta declarantes impuesto)
			drop hasta
		}		

		if ("`tab'" == "3.2.3.2.1" & `year'==2002) { 	
			//Change names	
			quietly rename (B C D E F G H I J K L M N O P) ///
				(desde hasta declarantes totinc tot_1_4cat ///
				casos_1cat importe_1cat casos_2cat importe_2cat ///
				casos_3cat importe_3cat casos_4cat importe_4cat ///
				casos_part_emp importe_part_emp)
			drop hasta
		}
		
		else if ("`tab'" == "3.2.3.2.2" & `year'==2002) { 	
			//Change names
			quietly rename (B C D E F G H I J K L M N O P Q) ///
			(desde hasta declarantes_ing declarantes_ded totded totded_1_4cat ///
			casosded_1cat importeded_1cat casosded_2cat importeded_2cat ///
			casosded_3cat importeded_3cat casosded_4cat importeded_4cat ///
			casos_part_emp_qb importe_part_emp_qb)		
			drop hasta
		}	

		quietly save ///
		"`rawfd'/EstadisticasTributarias`year'/`tab'.dta", ///
		replace 
		
		di as text "`rawfd'/EstadisticasTributarias`year'/`tab'.dta saved" 
	}
}

import excel ///
"`rawfd'/EstadisticasTributarias2002/AFIP/3.2.3.2.10.xls", /// 
cellrange(B16:O22) clear
	
//Change names	
drop E-N
quietly rename (B C D O) (desde hasta declarantes impuesto)
drop hasta

quietly save ///
"`rawfd'/EstadisticasTributarias2002/3.2.3.2.10.dta", replace 


local time "2003 2004 2005 2006 2007 2008 2015 2016 2017 2018 2019 2020"

foreach year in `time' {
	
	di as result "`year'" 
	
	foreach tab in "2.2.2.1.1" "2.2.2.1.2" "2.2.2.1.11" {
		
		//define range
		if ("`tab'" == "2.2.2.1.1") & inrange(`year',2003,2006) {
			local cellr "B16:P33" 
		} 
		else if ("`tab'" == "2.2.2.1.2") & inrange(`year',2003,2006) {
			local cellr "B17:Q34"
		}  
		else if inrange(`year',2003,2006) local cellr "B17:H34"
		if ("`tab'" == "2.2.2.1.1") & inrange(`year',2007,2018) {
			local cellr "B14:P31"
		} 
		else if ("`tab'" == "2.2.2.1.2") & inrange(`year',2007,2018) {
			local cellr "B15:Q32"
		} 
		else if inrange(`year',2007,2019) {
			local cellr "B15:H32"
		} 
		if ("`tab'" == "2.2.2.1.1") & `year'== 2019 local cellr "B13:P30"
		else if ("`tab'" == "2.2.2.1.2") & `year'== 2019 local cellr "B14:Q31"
		if ("`tab'" == "2.2.2.1.1") & `year'== 2020 local cellr "B14:R43"
		else if ("`tab'" == "2.2.2.1.2") & `year'== 2020 local cellr "B15:S44"
		else if `year'== 2020 local cellr "B15:H44"
		
		//import
		cap import excel ///
			"`rawfd'/EstadisticasTributarias`year'/AFIP/`tab'.xls", /// 
				cellrange(`cellr') clear	
		if _rc != 0 {
			cap import excel ///
			"`rawfd'/EstadisticasTributarias`year'/`tab'.xlsx", /// 
				cellrange(`cellr') clear	
		}
			
		if ("`tab'" == "2.2.2.1.1") & inrange(`year',2003,2019) { 	
			//Change names	
			quietly rename (B C D E F G H I J K L M N O P) ///
				(desde hasta declarantes totinc tot_1_4cat ///
				casos_1cat importe_1cat casos_2cat importe_2cat ///
				casos_3cat importe_3cat casos_4cat importe_4cat ///
				casos_part_emp importe_part_emp)
			cap drop Q 
			drop hasta
		}
		else if ("`tab'" == "2.2.2.1.1") & `year'==2020 { 	
			//Change names	
			quietly rename (B C D E F G H I J K L M N O P Q R) ///
				(desde hasta declarantes totinc tot_1_4cat ///
				casos_1cat importe_1cat casos_2cat importe_2cat ///
				casos_3cat importe_3cat casos_4cat importe_4cat ///
				casos_div importe_div casos_part_emp importe_part_emp)
			cap drop Q 
			drop hasta
		}
		
		else if ("`tab'" == "2.2.2.1.2") & inrange(`year',2003,2019) { 	
			//Change names
			quietly rename (B C D E F G H I J K L M N O P Q) ///
			(desde hasta declarantes_ing declarantes_ded totded totded_1_4cat ///
			casosded_1cat importeded_1cat casosded_2cat importeded_2cat ///
			casosded_3cat importeded_3cat casosded_4cat importeded_4cat ///
			casos_part_emp_qb importe_part_emp_qb)		
			drop hasta
		}
	
		else if ("`tab'" == "2.2.2.1.2") & `year'==2020 { 	
			//Change names
			quietly rename (B C D E F G H I J K L M N O P Q R S) ///
			(desde hasta declarantes_ing declarantes_ded totded totded_1_4cat ///
			casosded_1cat importeded_1cat casosded_2cat importeded_2cat ///
			casosded_3cat importeded_3cat casosded_4cat importeded_4cat ///
			casos_div importe_div casos_part_emp_qb importe_part_emp_qb)		
			drop hasta
		}
		
		else {
			quietly rename (B C D E F G H) ///
				(desde hasta declarantes_tax totinc_tax casos_gan ///
				importe_gan impuesto)
			drop hasta
		}
	
		quietly save "`rawfd'/EstadisticasTributarias`year'/`tab'.dta", ///
			replace 
		
		di as text "`rawfd'/EstadisticasTributarias`year'/`tab'.dta saved"
	}
		
}

local iter = 1 
local years "2001 2002 2003 2004 2005 2006 2007 2008  2015 2016 2017 2018 2019 2020"
local time "2000 2001 2002 2003 2004 2005 2006 2007 2014 2015 2016 2017 2018 2019"

foreach year in `years' {
	
	di as result "`year'"
	local t: word `iter' of `time'

	if `year'==2001 {
		
		use "`rawfd'/EstadisticasTributarias`year'/3.2.3.2.1.dta", clear
		
		// merge tabulations
		merge m:m desde using ///
			"`rawfd'/EstadisticasTributarias`year'/3.2.3.2.2.dta", nogenerate
		
		// change names and keep variables of interest
		rename (desde totinc) (thr total)
		gen year=`t' in 1
		gen total_net = total - importeded_3cat - importe_part_emp_qb - impuesto
		egen totalnetinc=total(total_net)
		replace totalnetinc=totalnetinc*1000
		egen totalpop=total(declarantes)
		gen average=(totalnetinc)/totalpop
		gen bracketavg = (total_net*1000)/declarantes
		
		order year average totalnetinc declarantes thr bracketavg 
		keep year average totalnetinc declarantes thr bracketavg
	
	}
	
	if `year'==2002 {
		
		use "`rawfd'/EstadisticasTributarias`year'/3.2.3.2.1.dta", clear
		
		// merge tabulations
		qui merge m:m desde using ///
			"`rawfd'/EstadisticasTributarias`year'/3.2.3.2.2.dta", nogenerate
		qui merge m:m desde using ///
			"`rawfd'/EstadisticasTributarias`year'/3.2.3.2.10.dta", nogenerate

		// change names and keep variables of interest
		rename (desde totinc) (thr total)
		gen year=`t' in 1
		gen total_net = total - importeded_3cat - importe_part_emp_qb - impuesto
		egen totalnetinc=total(total_net)
		replace totalnetinc=totalnetinc*1000
		egen totalpop=total(declarantes)
		gen average=(totalnetinc)/totalpop
		gen bracketavg = (total_net*1000)/declarantes
		
		order year average totalnetinc declarantes thr bracketavg 
		keep year average totalnetinc declarantes thr bracketavg
	}
	
	if inrange(`year',2003,2020) {
		
		use "`rawfd'/EstadisticasTributarias`year'/2.2.2.1.1.dta", clear
			
		// merge tabulations
		merge m:m desde using ///
			"`rawfd'/EstadisticasTributarias`year'/2.2.2.1.2.dta", nogenerate
		merge m:m desde using ///
			"`rawfd'/EstadisticasTributarias`year'/2.2.2.1.11.dta", nogenerate
		
		// change names and keep variables of interest
		rename (desde totinc importe_1cat importe_2cat ///
			importe_3cat importe_4cat importe_part_emp) ///
			(thr total rent capital mixed labour business)
		gen year=`t' in 1
		keep year thr declarantes total rent capital mixed labour business ///
			importeded_3cat importe_part_emp_qb impuesto
		gen total_net = total - importeded_3cat - importe_part_emp_qb - impuesto
		egen totalnetinc=total(total_net)
		egen totalpop=total(declarantes)
		
		if inrange(`year',2003,2012) {
			replace totalnetinc=totalnetinc*1000
			gen average=(totalnetinc)/totalpop
			gen bracketavg = (total_net*1000)/declarantes 
		}
		
		if inrange(`year',2013,2020) {
			replace totalnetinc=totalnetinc*1000000
			gen average=(totalnetinc)/totalpop
			gen bracketavg = (total_net*1000000)/declarantes 
		}
		
		foreach inc in "rent" "capital" "mixed" "labour" "business" {
			gen sh_`inc' =  `inc' / total
		}
		
		order year average totalnetinc declarantes thr bracketavg sh_rent sh_capital sh_mixed sh_labour sh_business
		keep year average totalnetinc declarantes thr bracketavg sh_rent sh_capital sh_mixed sh_labour sh_business

	}	

	local iter = `iter' + 1
	
	quietly save ///
	"`rawfd'/EstadisticasTributarias`year'/tabulation_`t'.dta", replace 
	
	di as result "saved `rawfd'/EstadisticasTributarias`year'/tabulation_`t'.dta"

}	

local iter = 1 
local years "2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020"
local time "2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019"

//get total population 
qui use "input_data/wid_population/pops.dta", clear 
forvalues ano = 2000/2020 {
	qui sum npopul_adults if country == "ARG" & year == `ano'
	local totalpop`ano' = r(mean)
	di as result "totalpop`ano': `ano'"
}


foreach year in `years' {
	local t: word `iter' of `time'
	
	use "`rawfd'/EstadisticasTributarias`year'/tabulation_`t'.dta", clear
	cap drop totalpop
	gen totalpop = `totalpop`t''
	tempvar freq cumfreq 
	//bracket frequency
	cap gsort -thr
	cap confirm variable p, exact 
	if _rc != 0 {
		quietly gen `freq'=declarantes/totalpop
		//cumulative frequency
		quietly	gen `cumfreq' = sum(`freq')
		//percentiles
		quietly gen p = 1 - `cumfreq'
	}
	cap sort thr
	keep average totalpop p
	tempfile p_`t'
	quietly save "`p_`t''"
	
	use "`rawfd'/EstadisticasTributarias`year'/tabulation_`t'.dta", ///
	clear
	
	merge m:m average using "`p_`t''", nogenerate
	
	cap gen country = "ARG" in 1
	
	cap confirm variable average, exact 
	if _rc != 0 {
		gen totalinc = totalnetinc 
		replace average = totalinc / totalpop
	}
	
	
	if inrange(`year',2003,2020) {
		keep year country totalpop average p bracketavg sh_rent sh_capital ///
			sh_mixed sh_labour sh_business
		order year country totalpop average p bracketavg sh_rent sh_capital ///
			sh_mixed sh_labour sh_business
	}
	
	else {
		keep year country totalpop average p bracketavg
		order year country totalpop average p bracketavg
	}		

	local iter = `iter' + 1
	
	// Create directory if it doesnt exist 
	local dirpath "input_data/admin_data/ARG/_clean"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}
	
	qui export excel ///
			"input_data/admin_data/ARG/_clean/total-pre-ARG-adults.xlsx", ///
			sheet("`t'", replace) firstrow(variables) keepcellfmt 
			
}

			
