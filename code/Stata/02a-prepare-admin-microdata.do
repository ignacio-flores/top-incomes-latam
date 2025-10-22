///////////////////////////////////////////////////////////////////////////////
//				    Goal: Calls dofiles preparing admin microdata	         //
/////////////////////////////////////////////////////////////////////////////// 

local list_noquotes : subinstr global all_countries `"""' "" , all

foreach dofile in "MEX-wages" "URY-gperc" { 
	local sub = substr("`dofile'", 1, 3)
	if strpos("`list_noquotes'", "`sub'") > 0 {
		//run
		di as result "(02a) Doing `dofile'.do at ($S_TIME)"
		quietly do "code/Stata/admin_microdata/`dofile'.do"
	}
}
