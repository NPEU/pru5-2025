/* PREAMBLE *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
Creation date: 	13/Nov/2024, Last Update: 6/June/2025
Purpose:    Create variable from HES APC data cut to use for Maternal Medicine Network 
				project (PRU5 project 2024) - prep 2
Author:     Victoria Soriano (CI Claire Carson)
			based on some code by Miranda Geddes-Barton (11/2024)
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
Requirements:	* Stata Licence (IC or higher) 
References:		n/a
Queries: *****
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

version 18.0
capture log close 
clear all 
set more off

*============================================================*
* set home directory 
*============================================================*
cd  [file location]

log using cr_hes_mmn2024_prep2.log, replace

use hes2024_mmn_prep1.dta //created with cr_hes2024_mmn_prep1.do

/*============================================================*
* TABLE OF CONTENTS * (CTRL+F the section)
	All variables that need to be created with whole dataset, including other admissions (2003-2023)

1. Mode of Delivery (mat tail only)
2. Ethnicity of patient, mode
3. Key/core medical conditions
4. Deaths and stillbirths

* Make birth cohort: only includes birth superspell

*============================================================*/

capture noisily drop __00000?

*============================================================*
* 1. Mode of Delivery (mat tail only)
*============================================================*
 * see next do file for other codes included
clonevar delivmethod_t=delmeth_1
	replace delivmethod_t = ".u" if delmeth_1=="X"
destring delivmethod_t, replace

bysort enc admidate_superspell del_d:egen delivmethod=max(delivmethod_t) //maximum not including missing, allows for most severe deliv to be selected
	lab var delivmethod "Method of birth, numerical (mat tail only)"

*============================================================*
* 2. Ethnicity of patient, mode
*============================================================*
	*Note: gen ethcode based on mode (excl where mode is 'unknown') and prioritising Other and BAME if 2+ modes
gen ethcode = .
	replace ethcode = 1 if ethnos == "A" //british white
	replace ethcode = 2 if ethnos == "B" //irish white
	replace ethcode = 3 if ethnos == "C" //any oth white
	replace ethcode = 4 if ethnos == "D" //w&b carrib
	replace ethcode = 5 if ethnos == "E" //w&b african
	replace ethcode = 6 if ethnos == "F" //w& asian
	replace ethcode = 7 if ethnos == "G" //any oth mixed
	replace ethcode = 8 if ethnos == "H" //indian
	replace ethcode = 9 if ethnos == "J" //pakist
	replace ethcode = 10 if ethnos == "K" //banglad
	replace ethcode = 11 if ethnos == "L" //any oth asian
	replace ethcode = 12 if ethnos == "M" //carrib
	replace ethcode = 13 if ethnos == "N" //african
	replace ethcode = 14 if ethnos == "P" //any oth black
	replace ethcode = 15 if ethnos == "R" //chinese
	replace ethcode = 16 if ethnos == "S" //any oth group
	replace ethcode = . if ethnos == "Z" //not stated
	replace ethcode = . if ethnos == "99" //not known >=2013
	replace ethcode = . if ethnos == "X" //not known <2013

gen ethvalid = 0 //check ethnicity code is valid
	replace ethvalid = 1 if ethcode >= 1 & ethcode <= 16
bysort enc: egen ethvalidmax = max(ethvalid)


bysort enc: egen ethnicity = mode(ethcode), maxmode //was ethmodemax, also changed ethfinal to ethnicity
bysort enc: egen ethmodemin = mode(ethcode), minmode

	replace ethnicity = . if ethvalidmax == 0
	lab var ethnicity "Ethnicity, 16 categories"
	
label define ethnic16_lbl 1 "british white" 2 "irish white" 3 "any oth white" 4 "w&b carrib" 5 "w&b african" 6 "w& asian" 7 "any oth mixed" 8 "indian" 9 "pakistan" 10 "bangladesh" 11 "any oth asian" 12 "carrib" 13 "african" 14 "any oth black" 15 "chinese" 16 "any oth group" 
	label values ethnicity ethnic16_lbl
	
	tab ethnicity, m

*Ethnicity in 4, 8, 10 categories
gen ethnic10=.
	replace ethnic10=0 if ethnicity==1 | ethnicity==2 | ethnicity==3 //any white
	replace ethnic10=1 if ethnicity==12 //black carrib
	replace ethnic10=2 if ethnicity==13 //black african
	replace ethnic10=3 if ethnicity==14 //any oth black
	replace ethnic10=4 if ethnicity==8 //indian
	replace ethnic10=5 if ethnicity==9 //pakist
	replace ethnic10=6 if ethnicity==10 //banglad
	replace ethnic10=7 if ethnicity==15 //chinese
	replace ethnic10=8 if ethnicity==16 //any oth group
	replace ethnic10=9 if ethnicity==4 | ethnicity==5 | ethnicity==6 |ethnicity==7 //any mixed
	replace ethnic10=10 if ethnicity==11 //any oth asian

	lab var ethnic10 "Ethnicity, 10 categories"

label define ethnic10_lbl 0"White" 1"Black or Black British - Caribbean" 2"Black or Black British - African" 3"Black - Other" 4"Indian" 5"Pakastani" 6" Bangladeshi" 7 "Chinese" 8 "Any other ethnic group" 9 "Mixed" 10"Other Asian" 
	label values ethnic10 ethnic10_lbl
	tab ethnic10, m

gen ethnic8=.
	lab var ethnic8 "Ethnicity, 8 categories"
replace ethnic8=0 if ethnic10==0
replace ethnic8=1 if ethnic10==1
replace ethnic8=2 if ethnic10==2
replace ethnic8=3 if ethnic10==4
replace ethnic8=4 if ethnic10==5
replace ethnic8=5 if ethnic10==6
replace ethnic8=6 if ethnic10==9
replace ethnic8=7 if ethnic10==3 | ethnic10==7 | ethnic10== 8 | ethnic10==10

label define ethnic8_lbl 0"White" 1"Black or Black British - Caribbean" 2"Black or Black British - African" 3"Asian or Asian British - Indian" 4"Asian or Asian British - Pakistani" 5"Asian or Asian British - Bangladeshi" 6 "Mixed" 7"Other" 
	label values ethnic8 ethnic8_lbl

gen ethnic4=.
	lab var ethnic4 "Ethnicity, 4 categories"
replace ethnic4=0 if ethnic10==0
replace ethnic4=1 if ethnic10==1|ethnic10==2|ethnic10==3
replace ethnic4=2 if ethnic10==4|ethnic10==5|ethnic10==6
replace ethnic4=3 if ethnic10==9
replace ethnic4=4 if ethnic10==7 | ethnic10==8 | ethnic10==10
replace ethnic4=. if ethnic10==.

label define ethnic4_lbl 0"White" 1"Black" 2 "Asian" 3"Mixed" 4"Other including chinese"  
label values ethnic4 ethnic4_lbl
	tab ethnic4, mi
	 
	 *Drop cleaning variables
drop ethcode ethvalid ethvalidmax ethmodemin 



*============================================================*
* 3. Core medical conditions
*============================================================* 

*Create variables
foreach x of numlist 1 4/5 8 10 12/13 17/18 24/35 37 50/53 58 63 67/68 {
	gen condition_`x'_t=0
}
	gen condition_31a_t=.
	gen condition_31b_t=.
	gen condition_27a_t=.
	gen condition_27b_t=.
	gen condition_27c_t=.
	
*Diagnostic codes
	foreach var of varlist diag_01-diag_20 {
		replace condition_1_t=1 if (inlist(`var', "I270", "I272")) //pulmonary hypertension
		replace condition_4_t=1 if (inlist(`var',"Q204", "Q224", "Q226", "Q234")) //heart surgery- fontan
		replace condition_5_t=1 if (inlist (substr(`var',1,3), "I42", "I43")) | (inlist(`var',"O903")) //Previous peripartum cardiomyopathy- in the 6m prior-5month post
		replace condition_8_t=1 if (inlist (substr(`var',1,3), "I20", "I21", "I22", "I23", "I24", "I25")) | (inlist(`var', "I200", "I201", "I208", "I209", "I210", "I211")) | (inlist(`var', "I212", "I213", "I214", "I219", "I220", "I221")) | (inlist(`var', "I228", "I229", "I230", "I231", "I232", "I233")) | (inlist(`var', "I234", "I235", "I236", "I238", "I240", "I241")) | (inlist(`var', "I248", "I249", "I250", "I251", "I252", "I255")) | (inlist(`var', "I256", "I258", "I259", "T822", "Z955")) //Ischaemic heart disease
		
		replace condition_10_t=1 if (inlist(`var',"Q251")) //Heart surgery: Repaired aortic coarctation // icd10 Q25.1  plus OPCS tbc
		replace condition_12_t=1 if (inlist (substr(`var',1,3), "J60", "J61", "J62", "J63", "J64", "J65")) | (inlist (substr(`var',1,3), "J66", "J67", "J84", "D86")) | (inlist(`var',"J841", "J701", "J703", "J704", "G532", "M633")) //Restrictive lung disease
		replace condition_13_t=1 if (inlist (substr(`var',1,3), "E84")) //Cystic fibrosis 
		replace condition_17_t=1 if (inlist (substr(`var',1,3), "C15", "C16", "C17", "C18", "C19", "C20")) | (inlist (substr(`var',1,3), "C21", "C22", "C23", "C24", "C25", "C26")) | (inlist (substr(`var',1,3), "C34")) | (inlist(`var', "C340", "C341", "C342", "C343", "C348", "C349")) | (inlist (substr(`var',1,3), "C81", "C82", "C83", "C84", "C85", "C86")) | (inlist (substr(`var',1,3), "C87", "C88", "C89", "C90", "C91", "C92")) | (inlist (substr(`var',1,3), "C93", "C94", "C95", "C96")) | (inlist(`var', "C795")) // GI: C15-C26, lung: C34, heam: C81–C96, C79.5
		replace condition_18_t=1 if (inlist(`var',"K766")) //Portal hypertension

		replace condition_24_t=1 if (inlist(`var', "K703", "K717", "K744", "K745", "K746")) //Cirrhosis
		replace condition_26_t=1 if (inlist(`var', "K754")) //Autoimmune hepatitis
		replace condition_27a_t=1 if (inlist(`var', "E240", "E243", "E248")) //Cushing's syndrome
		replace condition_27b_t=1 if (inlist(`var', "C740", "C749", "D350", "D352")) 
		replace condition_27c_t=1 if (inlist(`var', "E249"))

		replace condition_28_t=1 if (inlist(`var', "E220")) //Acromegaly
		replace condition_29_t=1 if (inlist(`var', "E210", "E211", "E212", "E213")) //Hyperparathyroidism
		replace condition_30_t=1 if (inlist(`var', "E102", "E112", "E122", "E132", "E142", "N083")) //Diabetic nephropathy (T1 T2)
		replace condition_31a_t=1 if (inlist(`var', "D352")) //Macroprolactinoma
		replace condition_31b_t=1 if (inlist(`var', "E221"))

		replace condition_32_t=1 if (inlist(`var', "E250")) //Congenital adrenal hyperplasia
		replace condition_33_t=1 if (inlist(`var', "E271", "E272")) //Addison's disease
		replace condition_34_t=1 if (inlist (substr(`var',1,3), "L93" "M32")) //Lupus nephritis
		replace condition_35_t=1 if (inlist(`var', "N165", "N185", "T824", "T861", "Y602", "Y612")) | (inlist(`var', "Y841", "Z491", "Z492", "Z940", "Z992")) //Pre-pregnancy CKD, stage 5
		replace condition_50_t=1 if (inlist (substr(`var',1,3), "C70", "C71", "C72")) | (inlist(`var',"C793")) //Cancer: Brain tumour

		replace condition_51_t=1 if (inlist (substr(`var',1,3), "I61", "I63", "I64")) //Acute stroke
		replace condition_52_t=1 if (inlist(`var', "G610")) //Guillain-Barre
		replace condition_53_t=1 if (inlist(`var', "G700")) //myasthenia gravis
		replace condition_58_t=1 if (inlist(`var', "D561", "D562")) //Beta thalassaemia major
		replace condition_63_t=1 if (inlist(`var',"D681", "D682")) //Clotting factor XI deficiency

		replace condition_67_t=1 if (inlist (substr(`var',1,3), "D66", "D67")) //Carriers of haemophilia -no gender specification due to expert input Apr 2025
		replace condition_68_t=1 if (inlist(`var',"D680")) // Von-Willebrand disease 

	}

*Procedural codes
	foreach var of varlist opertn_01-opertn_24 {
		replace condition_4_t=1 if (inlist(`var',"K182", "K192")) //Heart surgery Fontan
		replace condition_25_t=1 if (inlist (substr(`var',1,3), "K01")) | (inlist(`var', "E531", "E532", "E533", "E538", "E539", "J011")) | (inlist(`var', "J012", "J013", "J014", "J015", "J018", "J019")) | (inlist(`var', "M012", "M013", "M014", "M015", "M018", "M019")) | (inlist(`var', "T861", "T863", "T864")) //Transplant (lung/liver/kidney)
		replace condition_35_t=1 if (inlist(`var', "L746", "M011", "M012", "M013", "M014", "M015")) | (inlist(`var', "M018", "M019", "M026", "M027", "M084", "M172")) | (inlist(`var', "M174", "M178", "M179", "X401", "X402", "X403")) | (inlist(`var', "X405", "X406", "X411", "X412", "X421")) //Pre-pregnancy CKD, stage 5
		replace condition_37_t=1 if  (inlist (substr(`var',1,3), "X40")) | (inlist(`var',"X411", "X421")) | (inlist(`var', "X401", "X402", "X403", "X404", "X405"))| (inlist(`var', "X406","X407","X408", "X409")) //dialysis- EMMOI DArcy code
	}

*Merge variables that required multiple codes
		replace condition_27_t=1 if condition_27a_t==1 | (condition_27b_t==1 & condition_27c_t==1) //Cushing's syndrome
		replace condition_31_t=1 if condition_31a_t==1 & condition_31b_t==1 //Macroprolactinoma

*Loop dates and final variables
	//Create date of event and generate start of final variable
foreach x of numlist 1 4/5 8 10 12/13 17/18 24/35 37 50/53 58 63 67/68 {
	gen condition_`x'_date=admidate if condition_`x'_t==1
		bysort enc:egen condition_`x'_date1=min(condition_`x'_date)
		format condition_`x'_date condition_`x'_date1 %td

	gen condition_`x'=0 //Never
		replace condition_`x'=1 if condition_`x'_date1<. //occurred ever for woman
}

	drop *_t //to make space for more variables

	//Create events that must occur before the start of pregnancy: (VS checked - variables match)
foreach x of numlist 8 35 37 51 {
	di _newline "*Generate condition_`x' < start preg___________"
	notes condition_`x': Event first ocurred before start of pregnancy.

	forval n=1/15 {
		gen cond_`x'_d`n'_t =1 if condition_`x'_date1<=start_preg_d`n' & start_preg_d`n' <. //per delivery, check if the first date of event happened before each pregnancy event. 
			replace cond_`x'_d`n'_t = .m if start_preg_d`n' ==. //make variable missing if no pregnancy dates (could not have ocurred within a delivery)
		bysort enc:egen cond_`x'_d`n'=max(cond_`x'_d`n'_t)  //copy event by pregnancy through all admissions
			lab var cond_`x'_d`n' "Event condition_`x' occurred within pregnancy window #`n'"
	
	replace condition_`x' =2 if del_d ==`n' & cond_`x'_d`n'==1 //after event in copied into all admissions, replace here with 2 if occurred within current pregnancy line (del_d ensures it is a delivery data row)
		}
	}

	
	//Create events that must occur before post-pregnancy: 
foreach x of numlist 1 4 10 12 13 18 25/34 50 58 63 68 {
	di _newline "*Generate condition_`x', <6w post preg___________"
	notes condition_`x': Event first ocurred before 6w post of pregnancy.

	forval n=1/15 {
		gen cond_`x'_d`n'_t =1 if condition_`x'_date1<=post_preg_d`n' & post_preg_d`n' <. //per delivery, check if the first date of event happened before 6w post-pregnancy event. 
			replace cond_`x'_d`n'_t = .m if post_preg_d`n' ==. //make variable missing if no pregnancy dates (could not have ocurred within a delivery)
		bysort enc:egen cond_`x'_d`n'=max(cond_`x'_d`n'_t)  //copy event by pregnancy through all admissions
			lab var cond_`x'_d`n' "Event condition_`x' occurred before 6w post pregnancy window #`n'"
	
	replace condition_`x' =2 if del_d ==`n' & cond_`x'_d`n'==1 //after event is copied into all admissions, replace here with 2 if occurred within current pregnancy line (del_d ensures it is a delivery data row)
		}
	}

	drop *_t //to make space for more variables
	
	*Event within current pregnancy, before birth to postpartum... start_preg_d`n' - post_preg_d`n'
foreach x of numlist 5 17 24 52 53 { 
	di _newline "*Generate condition_`x', start to <6w post preg___________"
	notes condition_`x': Event ocurring in current pregnancy (start to post pregnancy)
	
	forval n=1/15 {
		gen cond_`x'_d`n'_t = inrange(condition_`x'_date, start_preg_d`n', post_preg_d`n') & start_preg_d`n'<. & post_preg_d`n' <. //per delivery, check if the date of event happened within a pregnancy event. _preg_d variables are already copied into every event line, so this will match the event to see if it ocurred within any preg window. 
			replace cond_`x'_d`n'_t = .m if start_preg_d`n'==. | post_preg_d`n' ==. //make variable missing if no pregnancy dates (could not have ocurred within a delivery)
		bysort enc:egen cond_`x'_d`n'=max(cond_`x'_d`n'_t) if cond_`x'_d`n'_t < . //copy event by pregnancy through all admissions
			lab var cond_`x'_d`n' "Event condition_`x' occurred within pregnancy window #`n'"
	
		replace condition_`x' =2 if del_d ==`n' & cond_`x'_d`n'==1 //after event is copied into all admissions, replace here with 2 if occurred within current pregnancy line (del_d ensures it is a delivery data row)
		}
}

	//Create Previous peripartum cardiomyopath (if cardiomyopathy happened within a pregious pregnancy window)
clonevar condition_5_current  = condition_5
	lab var condition_5_current "Current peripartum cardiomyopathy, excluded condition"
	replace condition_5 =1 if condition_5==2 //remove all "current", to add back if there was a previous peripartum event

	replace condition_5 = 2 if (cond_5_d1==1) & del_d>=2 & del_d<.
	replace condition_5 = 2 if (cond_5_d2==1) & del_d>=3 & del_d<.
	replace condition_5 = 2 if (cond_5_d3==1) & del_d>=4 & del_d<.
	replace condition_5 = 2 if (cond_5_d4==1) & del_d>=5 & del_d<.
	replace condition_5 = 2 if (cond_5_d5==1) & del_d>=6 & del_d<.
	replace condition_5 = 2 if (cond_5_d6==1) & del_d>=7 & del_d<.
	replace condition_5 = 2 if (cond_5_d7==1) & del_d>=8 & del_d<.
	replace condition_5 = 2 if (cond_5_d8==1) & del_d>=9 & del_d<.
	replace condition_5 = 2 if (cond_5_d9==1) & del_d>=10 & del_d<.
	replace condition_5 = 2 if (cond_5_d10==1) & del_d>=11 & del_d<.
	replace condition_5 = 2 if (cond_5_d11==1) & del_d>=12 & del_d<.
	replace condition_5 = 2 if (cond_5_d12==1) & del_d>=13 & del_d<.
	replace condition_5 = 2 if (cond_5_d13==1) & del_d>=14 & del_d<.
	replace condition_5 = 2 if (cond_5_d14==1) & del_d>=15 & del_d<.

*Any of MMN conditions within appropriate time frames? 
gen condition_any=0
	lab var condition_any "Any MMN condition, n=40"
	lab val condition_any yesno

foreach x of numlist 1 4/5 8 10 12/13 17/18 24/35 37 50/53 58 63 67/68 {
	replace condition_any=1 if condition_`x'==2
}
	replace condition_any=1 if condition_67==1

	
*Label variables
lab var condition_1 "Pulmonary hypertension"
lab var condition_4 "Heart surgery: Fontan"
lab var condition_5 "Previous peripartum cardiomyopathy"
lab var condition_8 "Ischaemic heart disease"

lab var condition_10 "Heart surgery: Repaired aortic coarctation" // icd10 only
lab var condition_12 "Restrictive lung disease"
lab var condition_13 "Cystic fibrosis"
lab var condition_17 "Cancer: lung, haem, GI"
lab var condition_18 "Portal hypertension"

lab var condition_24 "Cirrhosis"
lab var condition_25 "Transplant (lung/liver/kidney)"
lab var condition_26 "Autoimmune hepatitis"
lab var condition_27 "Cushing's syndrome"
lab var condition_28 "Acromegaly"

lab var condition_29 "Hyperparathyroidism"
lab var condition_30 "Diabetic nephropathy (T1 T2)"
lab var condition_31 "Macroprolactinoma"
lab var condition_32 "Congenital adrenal hyperplasia"
lab var condition_33 "Addison's disease"

lab var condition_34 "Lupus nephritis"
lab var condition_35 "Pre-pregnancy CKD, stage 5"
lab var condition_37 "Dialysis"	
lab var condition_50 "Cancer: Brain tumour"
lab var condition_51 "Acute stroke"

lab var condition_52 "New-onset Guillain-Barre syndrome"
lab var condition_53 "New diagnosis or flare of myasthenia gravis"
lab var condition_58 "Beta thalassaemia major"
lab var condition_63 "Clotting factor XI deficiency" 
lab var condition_67 "Carriers of haemophilia" // expert input advises: no gender restriction

lab var condition_68 "Von-Willebrand disease (I,II,III)" // (note: non-normal type 1 included on expert advice)


*Label values
lab define preg_lbl 0 "Never" 1 "After conception" 2 "Before conception"
	lab val condition_8 condition_35 condition_37 condition_51 preg_lbl
lab define preg2_lbl 0 "Never" 1 "Not peripartum" 2 "Peripartum", replace
	lab val condition_5_current condition_17 condition_24 condition_52 condition_53  preg2_lbl 
lab define preg3_lbl 0 "Never" 1 "After 6w pp" 2 "Before 6w pp", replace
foreach x of numlist 1 4 10 12 13 18 25/34 50 58 63 68 {
	lab val condition_`x' preg3_lbl 
}
lab define preg4_lbl 0 "Never" 1 "Not prev peripartum" 2 "Previous peripartum"
	lab val condition_5 preg4_lbl 
lab define ever_lbl 0 "Never" 1 "Ever", replace
	lab val condition_67 ever_lbl

foreach x of numlist 1 4/5 8 10 12/13 17/18 24/35 37 50/53 58 63 67/68 {
	tab condition_`x'
}
	drop *_t
*============================================================*
* 4. Death and stillbirths
*============================================================*
lab var deatheng "Death in England"
lab var deathdat "Date of death"
lab var deathreg "Date of registration of death"
lab var agedeath "Age at mother death, year"

gen stillbirth=1 if dismeth==5
	lab var stillbirth "Stillbirth"

gen motherdeath=0 if dismeth==4 | deatheng<=1 
	lab var motherdeath "Mother's death, birth to 6 wks postpartum"
	lab val motherdeath yesno

	*Event within current pregnancy, before birth... end_preg_d`n' - post_preg_d`n'
foreach var of varlist motherdeath { 
	di _newline "*Generate `var'___________"
	notes `var': Event ocurring in current pregnancy (end to 6w post pregnancy)
	
	forval n=1/15 {
		gen `var'_d`n'_t = inrange(deathdat, end_preg_d`n', post_preg_d`n') & end_preg_d`n'<. & post_preg_d`n' <. //per delivery, check if the date of event happened within a pregnancy event. 
			replace `var'_d`n'_t = .m if end_preg_d`n'==. | post_preg_d`n' ==. //make variable missing if no pregnancy dates (could not have ocurred within a delivery)
		bysort enc:egen `var'_d`n'=max(`var'_d`n'_t) if `var'_d`n'_t < . //copy event by pregnancy through all admissions
			lab var `var'_d`n' "Event `var' occurred within pregnancy window #`n'"
	
	replace `var' =1 if del_d ==`n' & `var'_d`n'==1 //after event is copied into all admissions
		}
}
	replace motherdeath=0 if motherdeath==. 
		tab motherdeath, m
/* Mother's death variables: deatheng deathdat deathreg agedeath ucause
deathdat != admidate_superspell where death is registered. DO NOT USE admidate to supplement date of death. Use deathdat

DISMETH
	1 = Patient discharged on clinical advice or with clinical consent
	2 = Patient discharged him/herself or was discharged by a relative or advocate (retired in 2022-23)
	3 = Patient discharged by a mental health review tribunal, Home Secretary or court
	4 = Patient died
	5 = Stillbirth
	6 = Patient discharged himself/herself (available from 2022-23)
	7 = Patient discharged by a relative or advocate (available from 2022-23)
	8 = Not applicable - Hospital Provider Spell not finished at episode end (i.e. not discharged) or current episode unfinished
	9 = Method of Discharge Not Known
*/

*------------------------------------------------------------*

*============================================================*
 * Make birth cohort: only includes birth superspell
*============================================================*
* Drop temporary variables
	drop *_date *_t	//temp variables which end in  "_d`n'_t" in the code
	drop cond_*_d? cond_*_d?? //key condition event by pregnancy number (individual variable)- not needed in final dta
	drop motherdeath_d? motherdeath_d??


* n records
count
	keep if deliv==1 & del_d!=. 
count

*============================================================*
save hes2024_mmn_prep2.dta, replace

log close

exit
 