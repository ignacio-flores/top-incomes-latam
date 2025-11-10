
run "code/Stata/00a-preamble.do"

// Download net national income figures (constant local currency) 
qui wid, indicators(npopul) ///
	areas(AR BR CL CO CR DO EC MX PE SV UY) ages(999 992) clear
qui rename country iso 

//rename 
qui kountry iso, from(iso2c) to(iso3c) geo(undet)
drop if missing(_ISO3C_)
qui keep iso _ISO3C_ year variable value 
qui rename _ISO3C_ country 
qui order country year 	
	
//reshape 
reshape wide value, i(country year) j(variable) string	
qui rename value* * 
qui rename *992i *_adults
qui rename *999i * 
qui drop *99?f *99?m

//save 
require_dir, path("input_data/wid_population")
qui save "input_data/wid_population/pops.dta", replace 
