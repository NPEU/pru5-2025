/* PREAMBLE *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
Creation date: 	13/Nov/2024, Last update 03/06/2025
Purpose:    Make birth cohort from HES APC data cut to use for Maternal Medicine Network 
				project (PRU5 project 2024)
Author:     Victoria Soriano (CI Claire Carson)
			some parts based on code by Miranda Geddes-Barton (11/2024)
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
Requirements:	* Stata Licence (IC or higher) 
References:		n/a
Queries: *****
Postfix notes: _t & _date indicate a temporary variable that will be deleted
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

version 18.0
capture log close 
clear all 
set more off

*============================================================*
* set home directory 
*============================================================*
cd [file location] 

log using cr_mmn2024.log, replace

use hes2024_mmn_prep2.dta //created wih cr_hes2024_mmn_prep2.do

/*============================================================*
* TABLE OF CONTENTS * (CTRL+F the section)
 1. Cohort
 2. Parity
 3. Preterm
 4. Maternal age
	4.1 Maternal age groups
 5. Mode of delivery/birth (MOD) - OPCS codes
	5.1 Delivery number, categorical
	5.2 Delivery date
	5.3 Drop duplicate idsuperspells/twins
 6. Merge ONS (census)
 7. Cleaning of census variables:
	7.1 Urban / Rural variable
	7.2 Index of multiple deprivation (IMD)
	7.3 Hospital trusts
 8. Region, extra variables
	8.1 Merge in regional and hospital postcode variables
	8.2 Merge in LSOA11 regions 
	8.3 Extra region variables
 9. Maternity tail
	9.1 Extra mode of delivery variables
...
 
 20. EMMOI composite (Rhiannon D'Arcy)
 
 21. MODSMO composite (MMN outcome) - n=21.5 events - Apr 2025

 99. Label variables 

*============================================================*/


*============================================================*
///Clean and create new variables ///
*============================================================*
capture noisily drop __00000?

*============================================================*
* 1. Cohort - further selection
*============================================================*
	* Select by year, gestation, birthweight
	
 	keep if year >=2013 & year <=2024 //years 2013-24
	
	* Select gestation and birthweight to include:
count if gestation >=. 
count if gestation<20 
	drop if gestation<20 

* drop if birthweight<75g -4SD for 22 weeks (see previous do file for references)
count if birthweight<200 & gestation==. //used cleaned birthweight variable, not birweit_1
count if birthweight<75 & gestation==. 
	drop if birthweight<75 & gestation==. //birthweight less than the minimum possible for 22 weeks

*Duplicates removed after section 5
	
*============================================================*
* 2. Parity
*============================================================*
*mutiparous if second delivery in the dataset, have obs code in the last 18 years after cleaning

gen primip=.
lab var primip "Parity"
	replace primip=0 if (del_d>1 & del_d <20) | pastobshxyn==1 
	replace primip=1 if primip!=0 | (del_d==1 & pastobshxyn!=1) //first delivery recorded, no past obstetric history. n... recorded as delivery number 1 but multiparous, likely due to pregnancies outside of UK

label define parity_lbl 0"Multiparous" 1"Primiparous"
	label val primip parity_lbl
 
 
 *2.1. Multiple pregnancy 
*============================================================*
gen multiplepreg_t =. 
	foreach var of varlist diag_01-diag_20 {
	replace multiplepreg_t=1 if (inlist (substr(`var',1,3), "O30")) | (inlist (`var',"Z376", "Z373", "Z375", "Z372", "Z374", "Z377")) | (inlist(`var', "Z383", "Z384", "Z385", "Z386", "Z387", "Z388")) | (inlist(`var', "O632", "O661", "Q894", "P015", "P503", "P505")) 
	}
	
	replace multiplepreg_t=0 if multiplepreg_t !=1 //Assumed single pregnancy if no diagnostic code 
	
bysort enc idsuperspell:egen multiplepreg=max(multiplepreg_t)
	label var multiplepreg "Multiple pregnancy"
	notes multiplepreg: Assumed single pregnancy if no diagnostic code

label define multiplepreg_lbl 0 "Single" 1 "Multiple" 
	label val multiplepreg multiplepreg_lbl
 
tab multiplepreg,m 

*============================================================*
* 3. Preterm - VS reordered code completely 2024 
*============================================================*

gen preterm_t=.  
	foreach var of varlist diag_01-diag_20 {
	replace preterm_t=1 if (inlist (substr(`var',1,3), "O60")) 
	replace preterm_t=1 if (inlist (`var',"P072", "P073"))  
	}

	replace preterm_t=.m if preterm_t==. & gestation==. & birthweight <.
 	replace preterm_t=1 if gestation<37 
	replace preterm_t=0 if gestation>=37 & gestation<.

	bysort enc admidate_superspell del_d:egen preterm=max(preterm_t)  //to know if this pregnancy is preterm, not others

label var preterm "Preterm, defined gestation then ICD-10"
	notes preterm: Gestation < 37wks is preterm.

label define preterm_lbl 0"Term" 1"Preterm" 
	label val preterm preterm_lbl

	*make new variable for gestationage to code missing data
gen gestationage=.
	lab var gestationage "Gestational age, missing imputed (note1)"
	replace gestationage=gestation if gestation!=.
	replace gestationage=35 if gestation==. & preterm==1 //assume 35 months if preterm code plus missing gestational age
	replace gestationage=39 if gestation==. & preterm==0  //assume 39 months if no preterm code plus missing gestational age
	notes gestationage: Missing are imputed from preterm: preterm=35; not preterm=39.

*============================================================*
* 4. Maternal age
*============================================================*
tab startage, mi
lab var startage "Maternal Age at admission, yr (HES)"

* Clean maternal age
*------------------------------------------------------------*
	*Drop over 55, under 10
gen age50 = 0 if startage <.
	lab var age50 "Mother's age >50y"
	replace age50 = 1 if startage > 50 
gen age55 = 0 if startage <.
	lab var age55 "Mother's age >55y"
	replace age55 = 1 if startage > 55
	drop if age55 == 1

gen age10 = 0 if startage <.
	lab var age10 "Mother's age <10y"
	replace age10 = 1 if startage <10 // for those with multiple pregnancies, what is age at other pregnancy (ie, >10 and <55?). If yes, make startage missing but keep pregnancy
		lab val age5* age10 yesno
	
	* save list of idsuperspell if age10 = 1
	levelsof enc if age10==1, local(agelist)
	gen agecheck=.
		foreach i of local agelist {
		replace agecheck=1 if enc =="`i'"
		}
	* Create variable about replicated 'enc' to indicate mother identified more than once. 
	sort enc
	by enc:  gen dup_enc = cond(_N==1,0,_n)
	* delete idsuperspell if age10==1 & more than 1 enc (ie, mother recorded more than once)
	drop if age10==1 & dup_enc ==0 // (unique case and age <10) 
	* keep record but make age missing if more than 1 record of the enc.
	replace startage=. if age10==1 //all IDs have age >20 & <50 in a different record, and multiple births recorded
	list enc if startage==. //VS checked all startages of 11-12y and none have other births recorded to check. Assume correct ages. 

* 4.1 Maternal age groups
*------------------------------------------------------------*
gen agegroup=.a if startage<.
	lab var agegroup "Maternal age, group"
	replace agegroup=1 if startage<20
	replace agegroup=2 if startage>=20 & startage<25
	replace agegroup=3 if startage>=25 & startage<30
	replace agegroup=4 if startage>=30 & startage<35
	replace agegroup=5 if startage>=35 & startage<40
	replace agegroup=6 if startage>=40 & startage<.

	lab define age6_lbl 1 "<20" 2"20-25" 3"25-30" 4"30-35" 5"35-40" 6">40"
		lab values agegroup age6_lbl
	
	recode agegroup (3=1) (2=3) (1=2), gen(agegroup2)
		lab var agegroup2 "Maternal age, 25-30 base"
	lab define age6_lbl2  1"25-30" 2 "<20" 3"20-25" 4"30-35" 5"35-40" 6">40"
	lab val agegroup2 age6_lbl2
		
		tab agegroup*, m

		drop agecheck dup_enc

*============================================================*
* 5. Mode of delivery/birth (MOD)
*============================================================*

	*Birth method procedural codes:
gen breechextract=.
gen breechnormal=. 
gen othermethods=.
gen forceps=.
gen lowforceps=.
gen otherforceps=.
gen othervaginal=.
gen ventose=.
gen emerg=.
gen elective=.
gen svd=.
gen chysterectomy=.

foreach var of varlist opertn_01-opertn_24 {
 	
	replace breechextract=1 if (inlist (substr(`var',1,3), "R19")) 
 	replace breechnormal=1 if (inlist (substr(`var',1,3), "R20")) 	// R20.1 Spontaneous breech delivery
 	replace othermethods=1 if (inlist(`var',"R258","R259")) //other 
 	replace chysterectomy=1 if (inlist(`var',"R251","R252")) //C section hysterectomy or descruptive operation 
 	replace othervaginal=1 if (inlist (substr(`var',1,3), "R23")) 

	replace forceps=1 if (inlist (substr(`var',1,3), "R21")) //to do checks (same as below added)
	replace lowforceps=1 if (inlist(`var',"R215"))
	replace otherforceps=1 if (inlist(`var',"R211", "R212", "R213", "R214", "R218", "R219")) //Other forceps "R218" "R219" added by VS
 	replace ventose=1 if (inlist (substr(`var',1,3), "R22"))
 	replace emerg=1 if (inlist (substr(`var',1,3), "R18")) 
 	
	replace elective=1 if (inlist (substr(`var',1,3), "R17")) 
 	replace svd=1 if (inlist (substr(`var',1,3), "R24")) | (inlist(`var',"R249"))
}

gen mod1=. // if multiple methods recorded, preference in below order
	lab var mod1 "Mode of delivery, procedural codes"
	replace mod1=9 if othermethods==1 & mod1==.
	
	replace mod1=0 if svd==1
	replace mod1=1 if othervaginal==1
	replace mod1=2 if lowforceps==1
	replace mod1=3 if otherforceps==1
	replace mod1=4 if ventose==1
	replace mod1=5 if breechextract==1
	replace mod1=6 if breechnormal==1
	replace mod1=7 if elective==1
	replace mod1=8 if emerg==1
	replace mod1=10 if chysterectomy==1
 
label define mod1_lbl 0"Normal (Spontaneous vertex)" 1 "Spontaneous other" 2 "Low forceps cephalic" 3 "Other forceps" 4"Ventouse (Vacuum)" 5"Breech (including extraction)" 6"Breech" 7"Elective caesarean" 8"Emergency caesarean" 9"Other methods" 10"Caesarean hysterectomy"
	lab val mod1 mod1_lbl
	
/*	Must match HES delmeth_1 values:
0 = Spontaneous vertex (normal vaginal delivery, occipitoanterior)  
1 = Spontaneous other cephalic (cephalic vaginal delivery with abnormal presentation of head at delivery, without instruments...)  
2 = Low forceps, not breech, including forceps delivery not otherwise specified (forceps, low application, without manipulation)  
3 = Other forceps, not breech, including high forceps and mid forceps (forceps with manipulation)  
4 = Ventouse, vacuum extraction  
5 = Breech, including partial breech extraction (spontaneous delivery assisted or unspecified)  
6 = Breech
7 = Elective caesarean section
8 = Emergency caesarean section
9 = Other 
X = Not known // */
	
  
	*destring delivery method in previous do-file creating : delivmethod
	tab1 delmeth_1 delivmethod, m
  
	*cleaning deliv method - change to missing if trust is poorly coded///
///explore delivery method from maternity tail (knight paper)///
	replace delivmethod=. if delivmethod==9 & (procode=="RWG" | procode=="RWF" | procode=="RV8" | procode=="RTP" | procode=="RTK" | procode=="RTH" | procode=="RQQ" | procode=="RQ3" | procode=="RNS"| procode=="RNJ" | procode=="RMP" | procode=="RMZ" | procode=="RJZ" | procode=="RJF" | procode=="RJ7" | procode=="RHU" | procode=="RH8" | procode=="RHJ")

	* Mode of birth, if maternity tail for delivery method is missing then replace with procedure codes like Knight papepr)
gen mob_t=.
replace mob_t=delivmethod //prioritise maternity tail data
replace mob_t=mod1 if delivmethod==. | delivmethod==9 //then replace according to procedural codes if missing (or unknown in mat tail VS 02/2025)
replace mob_t=. if mod1==9 & (delivmethod==9 | delivmethod==.)

bysort idsuperspell : egen mob=max(mob_t) //copy through idsuperspells in case 1 is missing.
	lab var mob "Mode of birth, 9 categories"

	lab val mob mod1_lbl

gen mob5=. 
	lab var mob5 "Mode of birth, 5 categories"
	replace mob5=0 if mob==0 | mob==1 //Spontaneous vertex birth / vaginal
	replace mob5=1 if mob==2 | mob==3 | mob==4 //Instrumental birth
	replace mob5=2 if mob==5 | mob==6 //Breech
	replace mob5=3 if mob==7 //Elective caesarean delivery
	replace mob5=4 if mob==8 | mob==10 //Emergency caesarean or hysterectomy
	replace mob5=. if mob==9 //other

gen mob4=.
	lab var mob4 "Mode of birth, 4 categories"
	replace mob4=0 if mob==0 | mob==1 //Spontaneous vertex birth / vaginal
	replace mob4=1 if mob>=2 & mob<=6 //Instrumental birth or Breech
	replace mob4=3 if mob==7 //Elective caesarean delivery
	replace mob4=4 if mob==8 | mob==10 //Emergency caesarean or hysterectomy
	replace mob4=. if mob==9 //other
	notes mob4: Emergency CS includes CS hysterectomy&destructive ops
	
label define mob5_lbl 0"Spontaneous" 1 "Instrumental or breech" 2 "Breech" 3 "Elective caesarean" 4"Emergency caesarean" 5"Other methods"
	label val mob5 mob5_lbl
	
label copy mob5_lbl mob4_lbl
label define mob4_lbl 2 "" 5 "", modify
	label val mob4 mob4_lbl

//Investigate deliv methods vs procedural codes//
gen delivmethsame=.
	replace delivmethsame=0 if mod1==0 & delivmethod==0
	replace delivmethsame=0 if mod1==0 & delivmethod==1
	replace delivmethsame=0 if mod1==1 & delivmethod==0
	replace delivmethsame=0 if mod1==1 & delivmethod==1

	replace delivmethsame=1 if mod1==2 & delivmethod==2
	replace delivmethsame=1 if mod1==2 & delivmethod==3
	replace delivmethsame=1 if mod1==2 & delivmethod==4
	replace delivmethsame=1 if mod1==3 & delivmethod==2
	replace delivmethsame=1 if mod1==3 & delivmethod==3
	replace delivmethsame=1 if mod1==3 & delivmethod==4
	replace delivmethsame=1 if mod1==4 & delivmethod==2
	replace delivmethsame=1 if mod1==4 & delivmethod==3
	replace delivmethsame=1 if mod1==4 & delivmethod==4

	replace delivmethsame=2 if mod1==5 & delivmethod==5
	replace delivmethsame=2 if mod1==5 & delivmethod==6
	replace delivmethsame=2 if mod1==6 & delivmethod==5
	replace delivmethsame=2 if mod1==6 & delivmethod==6

	replace delivmethsame=3 if mod1==7 & delivmethod==7 
	replace delivmethsame=4 if mod1==8 & delivmethod==8 
	replace delivmethsame=5 if mod1==9 & delivmethod==9

lab define deliverymethod5 0"Spontaneous vertex" 1 "Instrumental" 2"Breech" 3"Elective caesarean" 4"Emergency caesarean" 5"Other methods"
	lab val delivmethsame deliverymethod5 

tab delivmethsame, mi 
 
	* 5.1 Delivery number, categorical
clonevar del8=del_d
	replace del8 = 8 if del_d >=8 & del_d <.
	
	lab define cat8_lbl 8 "8+"
	lab val del8 cat8_lbl
	
	* 5.2 Delivery date, months
gen month=month(admidate_superspell)
	lab var month "Month of admission"
gen monthyear=ym(year,month)
	lab var monthyear "Year-Month of admission"
	format monthyear %tm
replace monthyear=. if monthyear>= tm(2024m11) 

drop svd othervaginal forceps lowforceps otherforceps ventose breechextract breechnormal elective emerg othermethods chysterectomy //composite mod variables


* 5.3 Drop duplicate idsuperspells/twins - not already dropped after coping info over... mat age, BW, etc
*------------------------------------------------------------*
sort idsuperspell birthweight //important to have the same order for nvals and bvals
by idsuperspell, sort:gen nvals = _n ==1 
by idsuperspell birthweight, sort:gen bvals = _n ==1 //0 if duplicates. 1 if original birthweight values within that idsuperspell

tab multiplepreg if nvals !=1, m 
	drop if nvals ==0 //duplicates in cohort //VS checked with multiplepreg - drop if duplicate

*Remove twin duplicates not already removed. 
duplicates report enc admidate_superspell del_d //no duplicates
	duplicates tag enc admidate_superspell del_d, gen(dup_multi)
	duplicates drop enc admidate_superspell del_d, force
	
	drop  bvals nvals //drop variable after checks
*============================================================*
* 6. Merge ONS (census)
*============================================================*
merge m:1 lsoa11 using [census dataset], gen(gs_merge) force keepusing(IMDquintile20* UrbanRural urbanrural G G2015 LAD21NM NoEnglishrank* noenglish* ) 
	drop if gs_merge==2 //from census dataset only
	drop gs_merge

*============================================================*
* 7. Cleaning of census variables
*============================================================*


 * 7.1 Urban / Rural variable
*============================================================*
gen urbanrural_n=. 
	lab var urbanrural_n "Location, census"
	replace urbanrural_n=0 if urbanrural=="Urban"
	replace urbanrural_n=1 if urbanrural=="Rural"
	lab define urbanrural_lbl 0"Urban" 1"Rural"
	lab val urbanrural_n urbanrural_lbl
	
		drop urbanrural UrbanRural //census variables, to avoid confusion in coding

 * 7.2 Index of multiple deprivation (IMD)
*============================================================*

/*label define imdlabel 1 "Most deprived 10%" 2 "More deprived 10-20%" 3 "More deprived 20-30%" 4 "More deprived 30-40%" 5 "More deprived 40-50%" 6 "Less deprived 40-50%" 7 "Less deprived 30-40%" 8 "Less deprived 20-30%" 9 "Less deprived 10-20%" 10 "Least deprived 10%", replace
*/

gen imd_quint =. 
	lab var imd_quint "IMD quintile, 2015 & 2019"
replace imd_quint=IMDquintile2015 if year<2019 
replace imd_quint=IMDquintile2019 if year>=2019 & year<.
	label value imd_quint imdlabel2
	
	recode imd_quint (1 = 5) (2 = 4) (4 = 2) (5 = 1), gen(imd_quint2)
		lab var imd_quint2 "IMD quintile, 2015 & 2019, least to most"
			lab define imd5_lbl2 1 "Least deprived 80-100%" 2 "Less deprived 60-80%" 3 "Less deprived 40-60%"  4 "More deprived 20-40%" 5 "Most deprived 20%" 
			lab val imd_quint2 imd5_lbl2

gen imd_dec =.
	lab var imd_dec "IMD decile, both 2015 & 2019"
replace imd_dec=G2015 if year<2019 
replace imd_dec=G if year>=2019 & year>=2019 
	label value imd_dec imdlabel

 * 7.3 Hospital trusts -MGB/RG code. Not touched by VS
*============================================================*

 ///procode///
gen procode_d=. //changed from Procode to procode_d
	lab var procode_d "Hospital trusts, numerical"
replace procode_d= 0 if procode3=="R0A"
replace procode_d= 1 if procode3=="R0B"
replace procode_d= 2 if procode3=="R0D"
replace procode_d= 3 if procode3=="R1F"
replace procode_d= 4 if procode3=="R1F"
replace procode_d= 5 if procode3=="R1H"
replace procode_d= 6 if procode3=="R1K"
replace procode_d= 7 if procode3=="RA2"
replace procode_d= 8 if procode3=="RA3"
replace procode_d= 9 if procode3=="RA4"
replace procode_d= 10 if procode3=="RA7"
replace procode_d= 11 if procode3=="RA9"
replace procode_d= 12 if procode3=="RAE"
replace procode_d= 13 if procode3=="RAJ"
replace procode_d= 14 if procode3=="RAL"
replace procode_d= 15 if procode3=="RAP"
replace procode_d= 16 if procode3=="RAS"
replace procode_d= 17 if procode3=="RAX"
replace procode_d= 18 if procode3=="RBA"
replace procode_d= 19 if procode3=="RBD"
replace procode_d= 20 if procode3=="RBK"
replace procode_d= 21 if procode3=="RBL"
replace procode_d= 22 if procode3=="RBN"
replace procode_d= 23 if procode3=="RBT"
replace procode_d= 24 if procode3=="RBZ"
replace procode_d= 25 if procode3=="RC1"
replace procode_d= 26 if procode3=="RC3"
replace procode_d= 27 if procode3=="RC9"
replace procode_d= 28 if procode3=="RCB"
replace procode_d= 29 if procode3=="RCD"
replace procode_d= 30 if procode3=="RCF"
replace procode_d= 31 if procode3=="RCX"
replace procode_d= 32 if procode3=="RD1"
replace procode_d= 33 if procode3=="RD3"
replace procode_d= 34 if procode3=="RD8"
replace procode_d= 35 if procode3=="RDD"
replace procode_d= 36 if procode3=="RDE"
replace procode_d= 37 if procode3=="RDU"
replace procode_d= 38 if procode3=="RDZ"
replace procode_d= 39 if procode3=="RE9"
replace procode_d= 40 if procode3=="REF"
replace procode_d= 41 if procode3=="REP"
replace procode_d= 42 if procode3=="RF4"
replace procode_d= 43 if procode3=="RFF"
replace procode_d= 44 if procode3=="RFR"
replace procode_d= 45 if procode3=="RFS"
replace procode_d= 46 if procode3=="RFW"
replace procode_d= 47 if procode3=="RGN"
replace procode_d= 48 if procode3=="RGP"
replace procode_d= 49 if procode3=="RGQ"
replace procode_d= 50 if procode3=="RGR"
replace procode_d= 51 if procode3=="RGT"
replace procode_d= 52 if procode3=="RH5"
replace procode_d= 53 if procode3=="RH8"
replace procode_d= 54 if procode3=="RHM"
replace procode_d= 55 if procode3=="RHQ"
replace procode_d= 56 if procode3=="RHU"
replace procode_d= 57 if procode3=="RHW"
replace procode_d= 58 if procode3=="RJ1"
replace procode_d= 59 if procode3=="RJ2"
replace procode_d= 60 if procode3=="RJ6"
replace procode_d= 61 if procode3=="RJ7"
replace procode_d= 62 if procode3=="RJC"
replace procode_d= 63 if procode3=="RJD"
replace procode_d= 64 if procode3=="RJE"
replace procode_d= 65 if procode3=="RJF"
replace procode_d= 66 if procode3=="RJL"
replace procode_d= 67 if procode3=="RJN"
replace procode_d= 68 if procode3=="RJR"
replace procode_d= 69 if procode3=="RJZ"
replace procode_d= 70 if procode3=="RK5"
replace procode_d= 71 if procode3=="RK9"
replace procode_d= 72 if procode3=="RKB"
replace procode_d= 73 if procode3=="RKE"
replace procode_d= 74 if procode3=="RL4"
replace procode_d= 75 if procode3=="RLN"
replace procode_d= 76 if procode3=="RLQ"
replace procode_d= 77 if procode3=="RLT"
replace procode_d= 78 if procode3=="RLU"
replace procode_d= 79 if procode3=="RM1"
replace procode_d= 80 if procode3=="RM2"
replace procode_d= 81 if procode3=="RM3"
replace procode_d= 82 if procode3=="RMC"
replace procode_d= 83 if procode3=="RMP"
replace procode_d= 84 if procode3=="RN3"
replace procode_d= 85 if procode3=="RN5"
replace procode_d= 86 if procode3=="RN7"
replace procode_d= 87 if procode3=="RNA"
replace procode_d= 88 if procode3=="RNL"
replace procode_d= 89 if procode3=="RNQ"
replace procode_d= 90 if procode3=="RNS"
replace procode_d= 91 if procode3=="RNZ"
replace procode_d= 92 if procode3=="RP5"
replace procode_d= 93 if procode3=="RPA"
replace procode_d= 94 if procode3=="RQ3"
replace procode_d= 95 if procode3=="RQ8"
replace procode_d= 96 if procode3=="RQM"
replace procode_d= 97 if procode3=="RQQ"
replace procode_d= 98 if procode3=="RQW"
replace procode_d= 99 if procode3=="RQX"
replace procode_d= 100 if procode3=="RR1"
replace procode_d= 101 if procode3=="RR7"
replace procode_d= 102 if procode3=="RR8"
replace procode_d= 103 if procode3=="RRF"
replace procode_d= 104 if procode3=="RRK"
replace procode_d= 105 if procode3=="RRV"
replace procode_d= 106 if procode3=="RTD"
replace procode_d= 107 if procode3=="RTE"
replace procode_d= 108 if procode3=="RTF"
replace procode_d= 109 if procode3=="RTG"
replace procode_d= 110 if procode3=="RTH"
replace procode_d= 111 if procode3=="RTK"
replace procode_d= 112 if procode3=="RTP"
replace procode_d= 113 if procode3=="RTR"
replace procode_d= 114 if procode3=="RTX"
replace procode_d= 115 if procode3=="RV8"
replace procode_d= 116 if procode3=="RVJ"
replace procode_d= 117 if procode3=="RVL"
replace procode_d= 118 if procode3=="RVR"
replace procode_d= 119 if procode3=="RVV"
replace procode_d= 120 if procode3=="RVW"
replace procode_d= 121 if procode3=="RVY"
replace procode_d= 122 if procode3=="RW6"
replace procode_d= 123 if procode3=="RWA"
replace procode_d= 124 if procode3=="RWD"
replace procode_d= 125 if procode3=="RWE"
replace procode_d= 126 if procode3=="RWF"
replace procode_d= 127 if procode3=="RWG"
replace procode_d= 128 if procode3=="RWJ"
replace procode_d= 129 if procode3=="RWW"
replace procode_d= 130 if procode3=="RWY"
replace procode_d= 131 if procode3=="RX1"
replace procode_d= 132 if procode3=="RXC"
replace procode_d= 133 if procode3=="RXF"
replace procode_d= 134 if procode3=="RXH"
replace procode_d= 135 if procode3=="RXK"
replace procode_d= 136 if procode3=="RXL"
replace procode_d= 137 if procode3=="RXN"
replace procode_d= 138 if procode3=="RXP"
replace procode_d= 139 if procode3=="RXQ"
replace procode_d= 140 if procode3=="RXR"
replace procode_d= 141 if procode3=="RXW"
replace procode_d= 142 if procode3=="RYJ"
replace procode_d= 143 if procode3=="RYQ"
replace procode_d= 144 if procode3=="RYR"
replace procode_d= 145 if procode3=="RYV"
replace procode_d= 146 if procode3=="RW3"
replace procode_d= 147 if procode3=="RWH"
replace procode_d= 148 if procode3=="RWP"
replace procode_d= 149 if procode_d==.

  
*============================================================*
*8. Region, extra variables
*============================================================*
*rename resgor label 'region'
label copy region resgor_lbl , replace
rename region resgor_region
	lab val resgor_region resgor_lbl
label drop region


	*8.1 Merge in regional and hospital postcode variables
*============================================================*
merge m:1 procode3 using "mmn2024_boundary.dta", keepusing(region postcode) keep(1 3) //do not keep unmatched data from 'using'.
	drop _merge

	*8.2 Merge in LSOA11 regions
*============================================================*
merge m:m lsoa11 using "region_lsoa.dta",  keepusing(region_lsoa) keep (1 3) //do not keep unmatched lsoa11 values.
	lab var region_lsoa "Region (based on LSOA11)"

	*8.3 Extra region variables
*============================================================*
clonevar region12=region
	lab var region12 "Region of birth, 12 groups (from procode3)"
	replace region12 =1 if region>=1 & region<=5  //london

	note region12: London regions merged together. See 'region' for separated london regions.

clonevar region12_lsoa=region_lsoa
	lab var region12_lsoa "Region, 12 groups (from LSOA11)"
	replace region12_lsoa =1 if region_lsoa>=1 & region_lsoa<=5  //london
	note region12_lsoa: London regions merged together. See 'region_lsoa' for separated london regions.

	lab define region12_lbl  ///
		1 "London" /// region16_lbl=1-5
		6 "Cumbria & North East" ///
		7 "East Midlands" ///
		8 "East of England" /// 
		9 "Kent & SE London" /// 
		10 "North West" ///  
		11 "South West" ///  
		12 "Sussex & SE London" /// 
		13 "Thames Valley" ///
		14 "Wessex" /// 
		15 "West Midlands" ///
		16 "Yorkshire & Humber" 

	lab val region12 region12_lsoa region12_lbl
	
gen region_match=.
	lab var region_match "Regions from procode & LSOA11 match?"
	replace region_match=1 if region==region_lsoa
	replace region_match=0 if region!=region_lsoa
	replace region_match=.m if region>=. & region_lsoa>=.
	lab val region_match yesno


*============================================================*
* 9. Maternity tail
*============================================================*

 * label variables 
	lab var lsoa11 "LSOA11=Lower Super Output Area of Residence 2011, from postcode"
	lab var procode3 "PROCODE3=Organisation Identifier (Code of Provider to Oct2020), trust level"
	lab var numpreg "NUMPREG=Number of Previous Pregnancies, EPITYPE=2/5"
	lab var anasdate "First Antenatal Assessment Date"
	lab var resgor "RESGOR=Government Office Region of Residence"
/* lab list region //resgor_derived
region:
           1 A = North East
           2 B = North West
           3 D = Yorkshire and Humber
           4 E = East Midlands
           5 F = West Midlands
           6 G = East of England
           7 H = London
           8 J = South East
           9 K = South West
*/

* 9.1 Extra mode of delivery variables
*============================================================*

label define delchang1 1"Decision made during pregnancy because the patient's address changed" 2"Decision made during pregnancy for clinical reasons" 3 "Decision made during pregnancy for other reasons" 4 "Decision made during labour for clinical reasons" 5"Decision made during labour for other reasons" 6"Decision made during labour for other reasons" 8 "Not applicable: there was no change" 9"unknown"
	label val delchang delchang1
label define dellocation_lbl 0 "NHS hospital: delivery facilities associated with midwife ward" 1 "At a domestic address" 2 "In NHS hospital: delivery facilities associated with consultant ward" 3"In NHS hospital: delivery facilities associated with GMP ward" 4"In NHS hospital: delivery facilities associated with consultant and GMP ward" 5"In private hospital" 6"In other hospital or institution" 7 "In NHS hospital: ward or unit without delivery facilities" 8" Other than those above " 9 "Not known"
	lab val delplac_1 dellocation_lbl //delinten
label define delivery_lbl 1 "General anaesthetic: the administration by a doctor of an agent to produce unconsciousness" 2"Epidural or caudal anaesthetic: the injection of a local anaesthetic into the epidural space" 3"Spinal anaesthetic: the injection of a local anaesthetic agent into the subarachnoid space" 4 "General anaesthetic and epidural or caudal anaesthetic" 5"General anaesthetic and spinal anaesthetic" 6 "Epidural or caudal, and spinal anaesthetic" 7 "Other than 1 to 6" 8"Not applicable" 9"Not known" 
	label val delposan delprean delivery_lbl
label define delonset1 1"Spontaneous: the onset of regular contractions whether or not preceded by spontaneous rupture of the membranes" 2"Any caesarean section carried out immediately following the onset of labour, when the decision was made before labour" 3"Surgical induction by amniotomy" 4"Medical induction" 5"Combination of surgical induction and medical induction" 8"Not applicable" 9"Not known: validation error"
	label val delonset delonset1
label define delstat1 1"Hospital doctor" 2"General practitioner" 3"Midwife" 8"Other than above" 9"Not known: validation error"
	label val delstat_1 delstat1

clonevar delchang_d=delchang
	label var delchang_d "Delivery place change reason"
	replace delchang_d=. if delchang==9
 
clonevar delplac_d=delplac_1
	label var delplac_d "HES Delivery place actual"
	replace delplac_d=. if delplac_1==9

clonevar delposan_d=delposan
	label var delposan_d "HES Anaesthetic given post-labour or delivery"
	replace delposan_d=. if delposan==9 

clonevar delprean_d=delprean
	label var delprean_d "HES Anaesthetic given during labour or delivery"
	replace delprean_d=. if delprean==9 

clonevar delonset_d=delonset
	label var delonset_d "HES Delivery onset method"
	replace delonset_d=. if delonset==9 

clonevar delstat_d=delstat_1
	label var delstat_d "HES Status of person conducting delivery"
	replace delstat_d =. if delstat_1==9

	tab1 delchang_d delplac_d delposan_d delprean_d delonset_d delstat_d, mi


*============================================================*
* 20. EMMOI composite (Rhiannon D'Arcy)
*============================================================*
 
gen emmoi2024=.
	lab var emmoi2024 "Severe maternal morbidity (EMMOI D'Arcy 2024)"
	notes emmoi2024: From start of pregnancy up to 42 days after birth, or from end of pregnancy for some conditions (specified in individual condition variables em_*).
	lab define emmoi3_lbl 0 "No, outside preg" 1 "Yes" 99 "Never"
	lab val emmoi2024 emmoi3_lbl
	
foreach var of varlist em_acuteabdomen em_arf em_psychosis em_cardiacarrest em_coma em_dic em_cva em_anaesth em_embolism em_shock em_sepsis em_sscrisis em_asthma1 em_epilepsy1 em_rupture em_eclampsia em_cvt em_panc em_vent em_curr em_dialysis em_hysterectomy em_respcomp em_dka em_dissection em_collection em_vaghaem  em_interventionhaem em_cswound em_cystostomy em_intestine em_managementacs em_managementthrombo em_managementspleenhaem em_interventionstroke em_repairaorta em_managementpanc {
	replace emmoi2024=0 if `var'==0 //No, outside preg 
}

foreach var of varlist em_acuteabdomen em_arf em_psychosis em_cardiacarrest em_coma em_dic em_cva em_anaesth em_embolism em_shock em_sepsis em_sscrisis em_asthma1 em_epilepsy1 em_rupture em_eclampsia em_cvt em_panc em_vent em_curr em_dialysis em_hysterectomy em_respcomp em_dka em_dissection em_collection em_vaghaem  em_interventionhaem em_cswound em_cystostomy em_intestine em_managementacs em_managementthrombo em_managementspleenhaem em_interventionstroke em_repairaorta em_managementpanc {
	replace emmoi2024=1 if `var'==1
}
  replace emmoi2024=99 if emmoi2024==.

	*Binary
 recode emmoi2024 (99=0), gen(emmoi)
  lab var emmoi "Severe maternal morbidity, binary (EMMOI D'Arcy 2024)"
  lab val emmoi yesno
  
	*EMMOI without sepsis
gen emmoi_sep=0 
	lab var emmoi_sep "Severe maternal morbidity-no sepsis (EMMOI D'Arcy 2024)"
	notes emmoi_sep: From start of pregnancy up to 42 days after birth, or from end of pregnancy- sepsis removed (specified in individual condition variables em_*).
	lab val emmoi_sep yesno
	
foreach var of varlist em_acuteabdomen em_arf em_psychosis em_cardiacarrest em_coma em_dic em_cva em_anaesth em_embolism em_shock  em_sscrisis em_asthma1 em_epilepsy1 em_rupture em_eclampsia em_cvt em_panc em_vent em_curr em_dialysis em_hysterectomy em_respcomp em_dka em_dissection em_collection em_vaghaem  em_interventionhaem em_cswound em_cystostomy em_intestine em_managementacs em_managementthrombo em_managementspleenhaem em_interventionstroke em_repairaorta em_managementpanc {
	replace emmoi_sep=1 if `var'==1 //Yes, removed em_sepsis
}


  
*============================================================*
* 21. MODSMO composite (MMN outcome - further restriction post 11/03/2025)
*============================================================*
 
gen modsmo=0 //missing data assumed no
	lab var modsmo "MMN morbidity outcome, n=21.5"
	notes modsmo: From start of pregnancy up to 42 days after birth, or from end of pregnancy for some conditions (specified in individual condition variables em_*).
	lab val modsmo yesno
	
*Overall variables: em_acuteabdomen em_arf em_cardiacarrest em_coma em_dic em_cva em_embolism em_shock em_sepsis em_sscrisis em_asthma1 em_epilepsy1 em_rupture em_eclampsia em_cvt em_panc em_vent em_dialysis em_respcomp em_dka em_dissection  em_interventionhaem em_cswound em_cystostomy em_intestine em_managementacs em_managementthrombo em_interventionstroke em_repairaorta em_managementpanc

foreach var of varlist em_cardiacarrest em_panc em_respcomp em_coma em_cvt em_cva em_dka em_eclampsia em_embolism_ps em_dissection em_shock em_sscrisis em_asthma1 em_epilepsy1 em_dialysis em_interventionhaem em_managementthrombo em_interventionstroke em_managementacs em_repairaorta em_vent em_managementpanc {
	replace modsmo=1 if `var'==1
}

	
*============================================================*
* 99. Label variables
*============================================================*

label var birweit_1 "HES Birthweight"
label var del1date "Delivery date"

	*drop cleaning variables: 
drop G2015 G IMDquintile2019 IMDquintile2015 imdrank imdrankla LAD21NM NoEnglishrank1 NoEnglishrank opertn_* diag_* *t_preg_d* end_preg_d*
drop mod1 delivmethod delivmethsame date dup *_t //once checked, drop partial variables

count 

*------------------------------------------------------------*

save mmn2024.dta, replace

log close

exit
 
 
 