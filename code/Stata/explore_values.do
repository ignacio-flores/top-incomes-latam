
//setup 
clear all  

// 0. General settings
global absorber i.canton i.t 
*global absorber i.canton##i.month
qui do "code/Stata/functions/aux_general.do" 

local ext w
local shk s
 
//loop over types 
foreach type in foods /*$tps*/ {
	
	*loop over shock-types
	foreach c in the_shock_`shk' {
		
		foreach ver in /*v1 v2*/ v3 {
				
				foreach depvar in gpp_`ext' agpp_`ext'  {
							
				*open database 
				qui use "data/FR/multi_panel_foods_${`ver'}.dta", replace 

				*define canton 
				cap drop canton 
				qui encode adm_can, gen(canton)
				qui so canton t 
				
				*define log 
				qui gen agpp_`ext' = asinh(gpp_`ext')
				
				*define asinh if necessary 
				if inlist("`depvar'", "acum_gpp_`ext'", "cum_gpp_`ext'") {
					qui byso canton year: gen cum_gpp_`ext' = ///
						sum(gpp_`ext') if inrange(p, 10, 32) 		
				}
				sort canton year date 
				
				//effects by quantile
				foreach v in $mainvars {
					
					qui so canton year 
					
					*build quintiles 
					cap drop quant_`v'
					cap drop q_`v'*
					qui xtile quant_`v' = `v' if year == 2015, nq(${q})
					bysort canton: ereplace quant_`v' = max(quant_`v') 
					qui tab quant_`v', gen(q_`v')
					qui tab1 q_`v'*
					
					*define treatment by quintile 
					foreach var of var q_`v'* {
						cap drop t`c'_`var'
						qui gen t`c'_`var' = `c' * `var'
					}
				} 
				
				br adm_can quant_gini_can gini_can area_agri seminat fsize name_4_farms percent_overlap if ra_code == 317 & p == 1 & year == 2021
				exit 1
				
			}
		}	
	}
}
