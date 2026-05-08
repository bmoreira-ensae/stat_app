/************************************
Project:    A Signal to End Child Marriage: Theory and Experimental Evidence from Bangladesh
File Name:  7_regression_tables.do
Purpose: 	Regressions
************************************/

/* Notes:
	
	ITT Regressions, including Covariates. 
	
*/

clear
set more off

cap log close
log using "$logs\7_regression_tables_$date_string", replace	

********************************************************************************
* Main Outcomes
********************************************************************************

* Clear stored data
eststo clear	
local ipv_k "i_angry_cellphone i_fr_angry_other_men i_fr_permit i_fr_limit_contact i_fr_insist i_fr_humiliate i_fr_threat i_fr_insult i_fr_spite i_fr_force i_fr_perform i_fr_push i_fr_slap i_fr_kick i_fr_punch i_fr_twist i_angry_for_friend i_angry_for_food i_fr_shout i_fr_stomp i_fr_destroy"

local angry_k "i_angry_cellphone i_angry_for_food i_angry_for_friend"
local contract_k "i_dowry denmeher"

* Index components
local el_g_cons_k "husband_level i_allow_dress education_resources menstruationunable_act i_latest_marry"	

* Define variables that need to be recoded
local recode "allow_dress latest_marry angry_cellphone angry_for_food angry_for_friend dowry arranged_marriage"
	
* Define the outcome lists
local lists "mar el_mar edu mc ss_mar sc"

forval r=0/3{
	/// Robustness: r=0: main, r=1: excluding controls, r=2: including women married before program start, r=3: TOT

	* Define specification (ITT or TOT)
	if `r'!=3{
		local reg "reg"
		local var "anyemp anyoil oil_kk"
		local out "anyemp anyoil oil_kk"
		local loc "ITT"
	}
	if `r'==3{
		local reg "ivregress 2sls"
		local var "anyemp (anylist list_kk=anyoil oil_kk)"
		local out "anyemp anylist list_kk"
		local loc "2SLS"
	}	
			
	* Loop through the lists
	foreach list in `lists' {
						
		forval w=1/4{
		/// w=1: wave III, w=2: wave II, w=3: wave III young women's survey; w=4: wave III
		
			* Define the waves
			if `w'==1 {
				local wave "endline"
				local data "$data\waveIII"
				local mar "under_18 under_16"
				local el_mar "ever_married"
				local edu ""
				local mc "dowry"
				local ss_mar ""
				local sc ""
			}

			if `w'==2{
				local wave "midline"
				local data "$data\waveII"
				local mar "ml_ever_married"
				local el_mar ""
				local edu "ml_still_in_school"
				local mc ""
				local ss_mar ""
				local sc ""
			}
				
			if `w'==3{
				local wave "endline"
				local data "$data\waveIII_young_women_sample"
				local mar ""
				if `r'==0 {					
					local el_mar "ever_married"										
				}
				if `r'!=0 {
					local el_mar ""
				}
				local edu ""
				local mc "denmeher"
				local ss_mar "under_18 under_16 ml_ever_married marriage_age ever_birth_20"
				local sc "el_g_cons_index"
			}
			if `w'==4{
				local wave "endline"
				local data "$data\waveIII"
				if `r'==0 {
					local mar "marriage_age ever_birth_20"
				}
				if `r'!=0 {
					local mar "ever_married marriage_age ever_birth_20"
				}
				local el_mar ""
				local edu "still_in_school education secondary_complete"
				local mc "husband_education husband_formal outside_union"
				local ss_mar ""
				local sc ""
			}
				
			* Define controls
			if `r'==1{
				local controls ""
				local miss ""
			}
			if `r'!=1{
				local controls "$controls"
				local miss "$miss"
				
				if `w'==3 & "`list'"!="sc"{ 
					local controls "older_sister bl_still_in_school bl_education_mother bl_HHsize bl_public_transit bl_age6 bl_age7 bl_age8 bl_age9 bl_age10 bl_age11 bl_age12 bl_age13 bl_age14 bl_age15 bl_age16 bl_age17 bl_bmi bl_stunted bl_income g_cons_index distance_hh_oil"
					local miss "older_sister_miss bl_still_in_school_miss bl_education_mother_miss bl_HHsize_miss bl_public_transit_miss bl_bmi_miss bl_stunted_miss bl_income_miss g_cons_index_miss distance_hh_oil_miss"
				}
				if `w'==3 & "`list'"=="sc"{ 
					local controls "older_sister bl_still_in_school bl_education_mother bl_HHsize bl_public_transit bl_age6 bl_age7 bl_age8 bl_age9 bl_age10 bl_age11 bl_age12 bl_age13 bl_age14 bl_age15 bl_age16 bl_age17 bl_bmi bl_stunted bl_income distance_hh_oil"
					local miss "older_sister_miss bl_still_in_school_miss bl_education_mother_miss bl_HHsize_miss bl_public_transit_miss bl_bmi_miss bl_stunted_miss bl_income_miss distance_hh_oil_miss"
				}
			}
					
			* Import data
			u "`data'", clear

			* Keep unmarried girls
			cap keep if bl_ever_married==0 
			
			* Keep girls in age range and define sub-group
			local age "bl_age_reported==14"
			if "`list'"!="sc"{
				keep if bl_age_reported>=14 & bl_age_reported<=16
				local age "bl_age_reported==14" 
			}
			if "`list'"=="sc"{
				local age "bl_age_reported>=14 & bl_age_reported<=16" 
			}
								
			* Keep only analysis data
			keep if `wave'==1 
			
			* Drop washedout data
			if `w'!=3 {
				keep if washedout==0
			}
			if `w'==3 {
				keep if girls_washedout==0
			}
			
			* Drop girls married before program start
			if `r'!=2{
				drop if before_miss==1
			}

			* Keep in school girls
			if "`list'"=="edu"{
				keep if bl_still_in_school==1
			}
								
			* Loop through outcomes
			foreach outcome in ``list''{
				preserve
				
				cap format dowry %5.1f
											
				* Create Kling mean effects
				if strpos("`outcome'","_index")>0 {
				   * Recode negative outcomes
					foreach v in `recode'{
						g i_`v'=-`v'
					}
					
					* Create index
					local index=subinstr("`outcome'","_index","", .)
					egen observations=rownonmiss(``index'_k')
					foreach component in ``index'_k'{
						bys treatment_type: egen `component'_mean=mean(`component')
						g `component'_k=`component'
						sum `component' if treatment_type==4
						g `component'_m=r(mean)
						g `component'_sd=r(sd)
						g `component'_std=(`component'_k-`component'_m)/`component'_sd
					}
					egen `index'_index=rowmean(*std)
					drop *_k *_m *_sd *_std *_mean observations
				}
					
				if "`list'"=="mar" | "`list'"=="el_mar" | "`list'"=="edu" | ("`list'"=="mc") | ("`list'"=="ss_mar" & `w'==3) | ("`list'"=="sc" & `w'==3)	{
					if "`outcome'"!="under_16" {
						eststo `list'_`outcome'`w'1: `reg' `outcome' `var' i.third i.unionID `controls' `miss', cluster(CLUSTER)
						sum `outcome' if treatment_type==4
						estadd scalar control_mean=r(mean)
						estadd local fe_var "Union"
						gen reg1_sample	= e(sample)

						* running regression for suest
						`reg' `outcome' `var' i.third i.unionID `controls' `miss'
						estimate store reg1
					}
					if "`list'"!="mc"{
						if strpos("`outcome'","_index")>0 {
							keep if `age'
							drop `index'_index
							egen observations=rownonmiss(``index'_k')
							foreach component in ``index'_k'{
								bys treatment_type: egen `component'_mean=mean(`component')
								g `component'_k=`component'
								sum `component' if treatment_type==4
								g `component'_m=r(mean)
								g `component'_sd=r(sd)
								g `component'_std=(`component'_k-`component'_m)/`component'_sd
							}
							egen `index'_index=rowmean(*_std)
						}
						eststo `list'_`outcome'`w'2: `reg' `outcome' `var' i.third i.unionID `controls' `miss' if `age', cluster(CLUSTER)
						sum `outcome' if treatment_type==4 & `age'
						estadd scalar control_mean=r(mean)
						estadd local fe_var "Union"
						gen reg2_sample	= e(sample)

						* running regression for suest
						`reg' `outcome' `var' i.third i.unionID `controls' `miss' if `age'
						estimate store reg2
						
						* Test whether effects for girls age 15 or age 15-17 are significantly different (ITT specifications)
						if "`list'"=="mar" | "`list'"=="el_mar" | "`list'"=="edu" | ("`list'"=="ss_mar" & `w'==3) {
							if "`outcome'"!="under_16" & `r'!=3{
								foreach regressor in anyemp anyoil oil_kk{
									suest reg1 reg2, vce(cluster CLUSTER)
									test [reg1_mean]`regressor' -  [reg2_mean]`regressor' = 0
									local scalar=`r(p)'
									est restore `list'_`outcome'`w'2
									estadd scalar sig_`regressor'=`scalar'
									estadd local empty_line " "
								}
							}
						}

						* Test whether effects for girls age 15 or age 15-17 are significantly different (TOT specifications)
						if `r'==3 & "`outcome'"!="under_16" {
							* changing frames to run a stacked specification
							cap frame drop frame_stacked
							frame copy default frame_stacked
							frame change frame_stacked

							* stacking the sample
							expand 2, gen(expanded_sample)
							drop if (reg1_sample==0 & expanded_sample==0) | (reg2_sample==0 & expanded_sample==1)

							* transform variables
							foreach v of varlist anyemp anylist list_kk anyoil oil_kk third unionID bl_age_reported $controls $miss {
								gen `v'_1 = `v'*(1-expanded_sample)
								gen `v'_2 = `v'*expanded_sample
							}

							* generating cluster var for stacked specification
							gen cluster_var = girlID + string(CLUSTER)

							local var_stacked "anyemp_1 anyemp_2 (anylist_1 anylist_2 list_kk_1 list_kk_2=anyoil_1 anyoil_2 oil_kk_1 oil_kk_2)"

							local controls1	"older_sister_1 bl_still_in_school_1 bl_education_mother_1 bl_HHsize_1 bl_public_transit_1"
							local miss1 		"older_sister_miss_1 bl_still_in_school_miss_1 bl_education_mother_miss_1 bl_HHsize_miss_1 bl_public_transit_miss_1"

							local controls2	"older_sister_2 bl_still_in_school_2 bl_education_mother_2 bl_HHsize_2 bl_public_transit_2"
							local miss2 		"older_sister_miss_2 bl_still_in_school_miss_2 bl_education_mother_miss_2 bl_HHsize_miss_2 bl_public_transit_miss_2"

							* run stacked regression
							eststo stacked: qui `reg' `outcome' `var_stacked' i.third_1 i.third_2 i.unionID_1 i.unionID_2 i.bl_age_reported_1 i.bl_age_reported_2 `controls1' `controls2' `miss1' `miss2' expanded_sample, cluster(cluster_var)
							frame change default
							frame drop frame_stacked

							foreach regressor in anyemp anylist list_kk {
								estimates restore stacked
								lincom `regressor'_1 - `regressor'_2
								local scalar=`r(p)'
								est restore `list'_`outcome'`w'2
								estadd scalar sig_`regressor'=`scalar'
								estadd local empty_line " "
							}
						}
					}
				}
				restore
			}
		}
			
		* Output the data
		if "`list'"=="mc" {
			estout `list'_* ///
			using "$tables\Reg_`loc'\reg_`list'_robust`r'.tex", label replace cells(b(fmt(3) ) se(par fmt(3))) style(tex)  ///
			mlabels(, none) collabels(, none) varlabels(N Observations) ///
			eqlabels(, none) ///
			stats(control_mean N fe_var, fmt(3 %12.0gc 3) ///
			labels("\hline Control Mean" "Observations" "FE")) /// 
			noomitted keep(`out') order(`out')   
			eststo clear
		}
		if `r'==3 & "`list'"!="mc" {
			estout `list'_* ///
			using "$tables\Reg_`loc'\reg_`list'_robust`r'.tex", label replace cells(b(fmt(3) ) se(par fmt(3))) style(tex)  ///
			mlabels(, none) collabels(, none) varlabels(N Observations) ///
			eqlabels(, none) ///
			stats(control_mean N fe_var empty_line sig_anyemp sig_anylist sig_list_kk, fmt(3 %12.0gc 3 3 3 3 3) ///
			labels("\hline Control Mean" "Observations" "FE" "\hline \underline{Age 15-17 vs 15:}" "Empowerment" "Incentive" "Incen.*Empow.")) /// 
			noomitted keep(`out') order(`out')   
			eststo clear
		}
		if `r'!=3 & "`list'"=="sc" {
			estout `list'_* ///
			using "$tables\Reg_`loc'\reg_`list'_robust`r'.tex" , label replace cells(b(fmt(3) ) se(par fmt(3))) style(tex)  ///
			mlabels(, none) collabels(, none) varlabels(N Observations) ///
			eqlabels(, none) ///
			stats(control_mean N fe_var, fmt(3 %12.0gc 3 3 3 3 3) ///
			labels("\hline Control Mean" "Observations" "FE")) /// 
			noomitted keep(`out') order(`out')
			eststo clear
		}
		if `r'!=3  & ("`list'"=="mar" | "`list'"=="el_mar" | "`list'"=="edu" | "`list'"=="ss_mar") {
			estout `list'_* ///
			using "$tables\Reg_`loc'\reg_`list'_robust`r'.tex", label replace cells(b(fmt(3) ) se(par fmt(3))) style(tex)  ///
			mlabels(, none) collabels(, none) varlabels(N Observations) ///
			eqlabels(, none) ///
			stats(control_mean N fe_var empty_line sig_anyemp sig_anyoil sig_oil_kk, fmt(3 %12.0gc 3 3 3 3 3) ///
			labels("\hline Control Mean" "Observations" "FE" "\hline \underline{Age 15-17 vs 15:}" "Empowerment" "Incentive" "Incen.*Empow.")) /// 
			noomitted keep(`out') order(`out')
			eststo clear
		}
	}
}

********************************************************************************
* Intra-cluster Correlation and Checking full compliance for girls age 15 
********************************************************************************

* Import data
u "$data\waveIII", clear

* Keep only analysis data
keep if endline==1 & washedout==0 & bl_age_reported>=14 & bl_age_reported<=16 & before_miss==0 & bl_ever_married==0

* Checking full compliance for girls age 15
keep if treatment_type==4
keep if bl_age_reported==14
g age_change=18-marriage_age if marriage_age<18
replace age_change=0 if marriage_age>18
replace age_change=. if marriage_age==.
g x=1
collapse (count) x, by(age_change)
egen all=sum(x)
g percent=x/all
replace age_change=age_change*12
g girl_change=percent*age_change
egen total_change=sum(girl_change)
di total_change

********************************************************************************
* Spillovers and heterogeneity by type (Treatment effect and Spillovers)
********************************************************************************
estimates dir
estimates clear
estimates dir

forval a=0/4{
	
	* Import data
	if `a'==0 | `a'>=3{
		u "$data\waveIII", clear
		keep if washedout==0 & bl_ever_married==0
		local weight ""
	}
	if `a'==1 | `a'==2{
		u "$data\waveIII_young_women_sample", clear
		keep if girls_washedout==0
		local weight " "
	}
	
	* Define controls
	if `a'==1{
		foreach var of varlist $controls distance_hh_ss distance_hh_vc distance_hh_nvc{
			cap g `var'_miss=`var'==.
			cap replace `var'=0 if `var'==.
		}
	}
	
	* Define heterogeneity groups: by age, social conservatism, BL schooling status, and mother's schooled status
	if `a'==0{
		local keep0 "bl_age_reported>=14"
		local keep1 "bl_age_reported<=13"
	}
	if `a'>=1{
		local keep: word `a' of high_g_cons high_g_cons bl_still_in_school bl_mother_schooled
		cap replace `keep'=. if `keep'_miss==1
		forval i=0/1{
			local keep`i' "`keep'==`i'"
		}
	}

	* Keep analysis data and define regressors
	keep if endline==1 & before_miss==0
	if `a'==0 | `a'==1{
	    keep if anyoil==0
		local regress "vill_radius_500_oil"
		la var vill_radius_500_oil "Close to incentive village"
		local control_mean "vill_radius_500_oil==0"
		local table=1
		local outcomes "under_18"
	}
	if `a'>=1{
	    keep if bl_age_reported>=14 & bl_age_reported<=16
	}
	if `a'>=2{
		local regress "anyemp anyoil oil_kk"
		local control_mean "treatment_type==4"
		local table=`a'
		if `a'==2{
		    local outcomes "under_18"
		}
		if `a'>=3{
		    local outcomes "under_18 still_in_school education dowry denmeher"
		}
	}

	* Regressions
	forval t=0/1{
	    foreach outcome in `outcomes'{
			preserve			
			if "`outcome'"=="denmeher"{
				u "$data\waveIII_young_women_sample", clear
				keep if girls_washedout==0
				local weight ""
				local control "older_sister bl_still_in_school bl_education_mother bl_HHsize bl_public_transit bl_age6 bl_age7 bl_age8 bl_age9 bl_age10 bl_age11 bl_age12 bl_age13 bl_age14 bl_age15 bl_age16 bl_age17 bl_bmi bl_stunted bl_income distance_hh_oil older_sister_miss bl_still_in_school_miss bl_education_mother_miss bl_HHsize_miss bl_public_transit_miss bl_bmi_miss bl_stunted_miss bl_income_miss distance_hh_oil_miss"
			}

			* defining control variables
			if `a'==0{
				local control "$controls $miss village_distance distance_vc_ss"
			}	
			if `a'==1{
				local control "older_sister bl_still_in_school bl_education_mother bl_HHsize bl_public_transit older_sister_miss bl_still_in_school_miss bl_education_mother_miss bl_HHsize_miss bl_public_transit_miss bl_age6 bl_age7 bl_age8 bl_age9 bl_age10 bl_age11 bl_age12 bl_age13 bl_age14 bl_age15 bl_age16 bl_age17 bl_bmi bl_stunted bl_income bl_bmi_miss bl_stunted_miss bl_income_miss village_distance distance_hh_vc distance_hh_vc_miss distance_vc_ss distance_hh_ss distance_hh_ss_miss"	
			}
			if `a'==2{
				local control "older_sister bl_still_in_school bl_education_mother bl_HHsize bl_public_transit older_sister_miss bl_still_in_school_miss bl_education_mother_miss bl_HHsize_miss bl_public_transit_miss bl_age6 bl_age7 bl_age8 bl_age9 bl_age10 bl_age11 bl_age12 bl_age13 bl_age14 bl_age15 bl_age16 bl_age17 bl_bmi bl_stunted bl_income bl_bmi_miss bl_stunted_miss bl_income_miss"
			}
			if `a'>=3{
				local control "$controls $miss"
			}
			
			if ("`outcome'"=="still_in_school" | "`outcome'"=="education") & `a'!=3{
			    keep if bl_still_in_school==1
			}
			keep if `keep`t''

			eststo reg`table'`outcome'`a'`t': reg `outcome' `regress' i.third i.unionID `control', cluster(CLUSTER)
			sum `outcome' if `control_mean'
			estadd scalar control_mean=r(mean)
			estadd local fe_var "Union"

			if `table'==2 {
				eststo reg`table'2`outcome'`a'`t': reg `outcome' `regress' i.third i.unionID `control' if bl_age_reported==14, cluster(CLUSTER)
				sum `outcome' if `control_mean' & bl_age_reported==14
				estadd scalar control_mean=r(mean)
				estadd local fe_var "Union"
			}
			restore
		}
	}

    estout reg`table'* ///
    using "$tables\reg`table'.tex", label replace cells(b(fmt(3)) se(par fmt(3))) style(tex) ///
    collabels(, none) varlabels(N Observations) ///
    mlabels(, none) eqlabels(, none) ///
    stats(control_mean N fe_var, fmt(3 %12.0gc) ///
    labels("\hline Control Mean" "Observations" "FE")) /// 
    noomitted keep(`regress') order(`regress')
}

