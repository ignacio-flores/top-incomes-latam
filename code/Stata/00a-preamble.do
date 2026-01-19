//load homemade programs 
clear programs
qui sysdir set PERSONAL "code/Stata/ado/."

//country colors
global c_arg "eltblue"
global c_bol "eltgreen"
global c_bra "midgreen"
global c_chl "cranberry"
global c_col "gold"
global c_cri "purple"
global c_ecu "stone"
global c_dom "black"
global c_pry "lavender"
global c_per "gs7"
global c_mex "dkgreen"
global c_ury "ebblue"
global c_slv "maroon"
global c_ven "red"
global c_nic "orange"
global c_gtm "gs15"
global c_hnd "sienna"

//country long names 
global lname_arg "Argentina"
global lname_bol "Bolivia"
global lname_bra "Brazil" 
global lname_chl "Chile" 
global lname_col "Colombia" 
global lname_cri "Costa Rica"
global lname_ecu "Ecuador"
global lname_pry "Paraguay"
global lname_per "Perú"
global lname_mex "Mexico"
global lname_ury "Uruguay"
global lname_slv "El Salvador"
global lname_ven "Venezuela"
global lname_dom "República Dominicana"

//group labels 
global lname_t1  "Top 1%"
global lname_t10 "Top 10%"

//variable labels (english)
global labcom_cap_eng "Property income"
global labcom_mix_eng "Mixed income"
global labcom_kap_eng "Capital & GOS"
global labcom_imp_eng "Imputed Rents"
global labcom_wag_eng "Wages"
global labcom_ben_eng "Pensions & benefits"
global labcom_pen_eng "Pensions"
global labcom_mir_eng "Mixed & GOS"
global labcom_upr_eng "Und. profits"
global labcom_indg_eng "Taxes on prod."
global labcom_lef_eng "Other"
global labcom_mbe_eng "Monetary Benefits"
global labcom_wmbe_eng "Post-tax Market inc."
global labcom_hea_eng "Health gov. expenditure"
global labcom_edu_eng "Education gov. expenditure"
global labcom_oex_eng "Other gov. expenditure"

//variable labels (español)
global labcom_cap_esp "Capital"
global labcom_mix_esp "Mixto"
global labcom_imp_esp "Alquileres"
global labcom_wag_esp "Salarios"
global labcom_ben_esp "Transferencias"
global labcom_pen_esp "Pensiones"
global labcom_mir_esp "Mixto & alquil."
global labcom_upr_esp "Gan. retenidas"
global labcom_indg_esp "Impuestos indir."
global labcom_lef_esp "Otros"


//axis label options 
global ylab_opts labsize(medium) grid labels angle(horizontal)
global xlab_opts labsize(medium) grid labels angle(horizontal)
global xlab_opts labsize(medium) grid labels angle(45)

//axis label options 
global ylab_opts_white labsize(medium) angle(horizontal)
*global xlab_opts_white labsize(medium) angle(horizontal)
global xlab_opts_white labsize(medium) angle(45)

//last bit of a graph
global graph_scheme scheme(s1color) subtitle(,fcolor(white) ///
lcolor(bluishgray)) graphregion(color(white)) ///
plotregion(lcolor(bluishgray)) scale(1.2)
