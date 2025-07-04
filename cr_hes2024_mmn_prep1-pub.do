/* PREAMBLE *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
Creation date: 	13/Nov/2024, Last Update: 6/June/2025
Purpose:    Create variable from HES APC data cut to use for Maternal Medicine Network 
				project (PRU5 project 2024) - prep 1
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
cd [file location]

log using cr_hes_mmn2024_prep1.log, replace

use ..\Raw_Data\vsoriano_2024nov_v1.dta

/*============================================================*
* TABLE OF CONTENTS * (CTRL+F the section)
	All variables that need to be created with whole dataset, including other admissions (2003-2024)

*opdate to delivery date- create and use this instead of deldate
1. Past obstetric history
2. Delivery
	2.1 Create delivery order
	2.2 Delivery number

3. Sex of baby
4. Gestational age and birthweight
	4.1 High / low birth weight 
	4.2 Gestation
	4.3 Birthweight

5. Pregnancy window

7. Individual EMMOI conditions (by morbid event)
8. EMMOI individual before/during birth (ref: Rhiannon D'Arcy 2024)
*============================================================*/


*============================================================*
 * 1. Past obstetric history, based on diagnostic and procedural codes. 
*============================================================*
///code in current pregnancy suggesting multiparous. coded up to and including the delivery episode. Ref: Sandell paper (pastobshx1) plus MGB (pastobshx). also codes previous delivery R17-R25 if coded 3 months prior to the delivery epsode ///

gen pastobshx=.
	lab var pastobshx "Past obstetric hx (excl uterine scar) ICD-10/OPSC"
foreach var of varlist diag_01-diag_20 {
 	replace pastobshx=1 if (inlist (`var', "O757", "Z354", "Z876", "Z875"))& epistart <= del1date
}
 
foreach var of varlist opertn_01-opertn_24 {
	replace pastobshx=1 if (inrange (substr(`var',1,3), "R17", "R25"))& epistart < del1date-90
}

	gen pastobshx_date=admidate if pastobshx==1
		 bysort enc:egen pastobshx_date_first=min(pastobshx_date)
		 
clonevar pastobshx1=pastobshx //same plus uterine scar
	lab var pastobshx1 "Past obstetric hx (incl uterine scar) ICD-10/OPSC"
foreach var of varlist diag_01-diag_20 {
 	replace pastobshx1=1 if (inlist (`var', "O342"))& epistart <= del1date //adds uterine scar
	}
 
gen pastobshx1_date=admidate if pastobshx1==1
	bysort enc:egen pastobshx1_date_first=min(pastobshx1_date)
	 
	format pastobshx_date_first pastobshx1_date_first %td 

gen pastobshxyn1=1 if pastobshx1_date_first<.

	
drop pastobshx_date pastobshx1_date


*============================================================*
* 2. Delivery
*============================================================*

* Link r17-25 delivery codes opertn_01 - opertn_24 to dates of delivery code opdate_01 opdate_24

foreach i of numlist 1/24 {
	if `i'<10 {
	local i= "0`i'" //adds a 0 onto these numbers
	}
	di "gen date_`i'_t: ----------------"
gen date_`i'_t = opdate_`i' if (inlist (substr(opertn_`i',1,3), "R17", "R18", "R19", "R20")) ///
 	| (inlist (substr(opertn_`i',1,3), "R21", "R22", "R23", "R24", "R25"))
		format date_`i'_t %td
	replace date_`i'_t=. if date_`i'_t < date("01jan2002","DMY") //earliest admission date jan2003
}

* Merge these 1-24 together to chose earliest date within this birth row:
egen date_t = rowmin(date_01_t date_02_t date_03_t date_04_t date_05_t date_06_t date_07_t date_08_t date_09_t date_10_t date_11_t date_12_t date_13_t date_14_t date_15_t date_16_t date_17_t date_18_t date_19_t date_20_t date_21_t date_22_t date_23_t date_24_t)

format date_t %td

bysort enc idsuperspell : egen date=min(date_t) //copy earliest date delivery episode started
	format date %td

	replace date = admidate_superspell if date ==. & deliv==1
	
* 2.1 Create delivery order
*------------------------------------------------------------*
	
preserve
	drop if deliv !=1
		*duplicates report enc epikey idsuperspell //good variables to match merge at end of this section
	sort enc date, stable
		keep enc epikey idsuperspell admidate_superspell date deliv
		duplicates tag enc idsuperspell, gen(dup_order)
		drop if dup_order >0 //drop any repeated idsuperspells
	bysort enc: gen order= _n
		tab order // maximum number of deliveries =15. Only 1 ID has order==15. VS checked delivery codes, gestation, etc and these all vary.
	keep enc epikey idsuperspell order
	
	tempfile deliv_order
	save `deliv_order'
restore
merge 1:1 enc epikey idsuperspell using `deliv_order'

drop _merge


* 2.2 Delivery/pregnancy number
*------------------------------------------------------------*
*find trusts with parity not 75/25% split. Change to numpreg = missing, if trust is poorly coded/// MGB selected poor trusts
replace numpreg=. if procode=="RWW" | procode=="RRV" | procode=="RPR" | procode=="RNQ" | procode=="RN3" | procode=="RMP" | procode=="RMC" | procode=="RMI" | procode=="RLN"| procode=="RJD" | procode=="RJI" | procode=="RHQ" | procode=="RGZ" | procode=="RGP" | procode=="REM" | procode=="RCD" | procode=="RBD" | procode=="RAJ" | procode=="RA3" | procode=="RIF" 
	replace numpreg=. if numpreg==99
tab numpreg, mi

*Delivery number (del1-del8 came created in dataset)
forval i=9/15 {
	clonevar del`i'date_t=date if order ==`i'
	bysort enc:egen del`i'date=min(del`i'date_t)
	format del`i'date %td
		drop del`i'date_t
}

gen del=. 
	lab var del "Delivery number, exact admisssion/disch"
	notes del: Indicates which del#date falls within the superspell period.

gen del_d=.
	lab var del_d "Delivery number, -30d/+3d window"
	notes del_d: Indicates which del#date falls within the superspell period -30d admission/+3 discharge.

forval i=1/15 {
	replace del=`i' if del`i'date >= admidate_superspell & del`i'date <= disdate_superspell
	replace del_d=`i' if del`i'date >= admidate_superspell-30 & del`i'date <= disdate_superspell+3 //+3 adds margin of error; -30d adds room for birth ocurring outside hospital with checkup after 
	}

	tab1 del del_d, m
	tab1 del del_d if deliv==1 & year >=2013, m
/* Check missing del_d. 03/06/2025

br enc admidate_superspell admidate epistart epiend disdate disdate_superspell deliv order del del_d del*date if del_d ==. & deliv==1 & year >=2013
gen checkdel=1 if del_d ==. & deliv==1 & year >=2013
bysort enc:egen checkdel_t=max(checkdel)
br enc admidate_superspell admidate epistart epiend disdate disdate_superspell deliv order del del_d del*date birweit_1 gestat_1 if checkdel_t==1

OUTCOME: Spot check of women with missing del_d shows that these are after the delivery event, likely a readmission post birth that have been mis-labeled in deliv as ==1. Those with deliv==1 & del_d==. will be dropped in the selection phase (end of do2)
*/

 * Tag duplicates admission dates within person and delivery
duplicates tag enc admidate_superspell del_d, gen(dup)
	lab var dup "Duplicate admission dates, person, and del_d"

*============================================================*
 * 3. Sex of baby
*============================================================*
 
gen sexbaby=.
	lab var sexbaby "Sex of baby, clean"
	replace sexbaby = 0 if sexbaby_1 =="2" | sexbaby_1 =="F"  //female
	replace sexbaby = 1 if sexbaby_1 =="1" | sexbaby_1 =="M"  //male

	replace sexbaby = 99 if sexbaby_1 =="3" | sexbaby_1 =="9" | sexbaby_1 =="I"  // indeterminate
	replace sexbaby = .m if sexbaby_1 =="0" | sexbaby_1 =="X" | sexbaby_1 =="U" //not known / unknown 

	lab define sex_lbl 0 "Female" 1 "Male" 99 "Indeterminate"
		lab val sexbaby sex_lbl
	
/*HES TOC data dictionary 24/02/2025:
	1 = Male
	2 = Female
From 2022-23 onwards: 
	9 = Indeterminate (unable to be classified as either male or female)
	X = Not Known (Person stated gender code not recorded)
From 1996-97 to 2022-23:
	9 = Not specified (indeterminate, i.e. unable to be classified as either male or female)
	0 = Not known (not recorded)
Prior to April 1996: 
	3 = Indeterminate, including those undergoing sex change operations
 * Other categories in the dataset:
 	       *   F  //female
	       *   I  //indeterminate
	       *   M  //male
	       *   U  //unknown
	*/


*============================================================*
 * 4. Gestational age and birthweight
*============================================================*

 * 4.1. High / low birth weight 
*------------------------------------------------------------*

	* Exceptions for Disorders related to long gestation/ high birth weight: From <https://icd.who.int/browse10/2019/en#/P07.2> 
gen hbweight_t=.
foreach var of varlist diag_01-diag_20 {
	replace hbweight_t =1 if (inlist (substr(`var',1,3), "P08")) | (inlist(`var',"P080", "P081", "P082", "P701", "P700"))
	}
bysort enc admidate_superspell del_d:egen hbweight=min(hbweight_t) 
	lab var hbweight "High birth weight or long gestation, ICD10"
	
gen lbweight_t=.
foreach var of varlist diag_01-diag_20 {
	replace lbweight_t =1 if (inlist (substr(`var',1,3), "P07")) | (inlist(`var',"P050", "P051"))
	}
bysort enc admidate_superspell del_d:egen lbweight=min(lbweight_t) 
	lab var lbweight "Low birth weight or short gestation, ICD10"

	drop *bweight_t
	
	/* Disorders related to long gestation/ high birth weight: From <https://icd.who.int/browse10/2019/en#/P07.2> 
		- P08 Disorders related to long gestation and high birth weight. Note: When both birth weight and gestational age are available, priority of assignment should be given to birth weight. Incl.: the listed conditions, without further specification, as causes of mortality, morbidity or additional care, in fetus or newborn
		- P08.0 Exceptionally large baby. Usually implies a birth weight of 4500 g or more. Excl.: infant of diabetic mother (P70.1), infant of mother with gestational diabetes (P70.0)
		- P08.1 Other heavy for gestational age infants. Usually implies a birth weight >90th percentile for gestational age or 4000 g or more at term. Other fetus or infant heavy- or large-for-dates regardless of period of gestation. Excl.: syndrome of infant of: diabetic mother (P70.1), mother with gestational diabetes (P70.0), Birth weight of 4500 g or more (P08.0)
		- P08.2 Post-term infant, not heavy for gestational age. Fetus or infant with gestation period of 42 completed weeks or more (294 days or more), not heavy- or large-for-dates. */


* 4.2 Gestation
*------------------------------------------------------------*
	* Copy: issue with duplicate admission date/delivery number- of these, 1 marked as a deliv==1 admission, other contains BW/GEST/MOB

clonevar gestat_t=gestat_1
replace gestat_t=. if gestat_t==99 | gestat_t>45
	label var gestat_t "Length of gestation, >20w"

bysort enc admidate_superspell del_d:egen gestation=min(gestat_t)
	lab var gestation "Gestation (from gestat_1)"
	replace gestation=gestat_t if gestat_t!=gestation & deliv==1 & gestat_t<. // if dif gestation in duplicate records, keep gestation from gestat_t variable in original delivery admission

bysort enc admidate_superspell del_d:egen birthweight=min(birweit_1) 
	lab var birthweight "Birthweight (from birweit_1)"
	replace birthweight=. if birthweight==9999 | birthweight==7000 //7000 seems to be used as another missing code

* 4.3 Birthweight
*------------------------------------------------------------*

 ///implasuible birth weight setting///
*Ref: WHO codes (based on the literature)-for minimum female weights and maximum male weights (below) for 23-42 weeks. 


foreach var of varlist birweit_1 birthweight {
	display _newline "`var' ___________________________________"
	gen implaus_`var'=.
	
		* Missing/ indeterminate gender (first- female minimum, male maximum)
		replace implaus_`var'=1 if gestation<22 & `var'>=990 & `var'!=. //added VS for gestation 20-21

		replace implaus_`var'=1 if gestation==22 & (`var'<=75   | (`var'>=990  & `var'!=.) )
		replace implaus_`var'=1 if gestation==23 & (`var'<=75   | (`var'>=990  & `var'!=.) )
		replace implaus_`var'=1 if gestation==24 & (`var'<=100  | (`var'>=1150 & `var'!=.) )
		replace implaus_`var'=1 if gestation==25 & (`var'<=125  | (`var'>=1300 & `var'!=.) )
		replace implaus_`var'=1 if gestation==26 & (`var'<=150  | (`var'>=1475 & `var'!=.) )
		replace implaus_`var'=1 if gestation==27 & (`var'<=190  | (`var'>=1660 & `var'!=.) )
		replace implaus_`var'=1 if gestation==28 & (`var'<=210  | (`var'>=1860 & `var'!=.) )
		replace implaus_`var'=1 if gestation==29 & (`var'<=260  | (`var'>=2090 & `var'!=.) )
		replace implaus_`var'=1 if gestation==30 & (`var'<=325  | (`var'>=2340 & `var'!=.) )
		replace implaus_`var'=1 if gestation==31 & (`var'<=400  | (`var'>=2625 & `var'!=.) )
		replace implaus_`var'=1 if gestation==32 & (`var'<=500  | (`var'>=2925 & `var'!=.) )
		replace implaus_`var'=1 if gestation==33 & (`var'<=610  | (`var'>=3250 & `var'!=.) )
		replace implaus_`var'=1 if gestation==34 & (`var'<=750  | (`var'>=3560 & `var'!=.) )
		replace implaus_`var'=1 if gestation==35 & (`var'<=910  | (`var'>=3860 & `var'!=.) )
		replace implaus_`var'=1 if gestation==36 & (`var'<=1100 | (`var'>=4150 & `var'!=.) )
		replace implaus_`var'=1 if gestation==37 & (`var'<=1300 | (`var'>=4390 & `var'!=.) )
		replace implaus_`var'=1 if gestation==38 & (`var'<=1510 | (`var'>=4600 & `var'!=.) )
		replace implaus_`var'=1 if gestation==39 & (`var'<=1725 | (`var'>=4790 & `var'!=.) )
		replace implaus_`var'=1 if gestation==40 & (`var'<=1910 | (`var'>=4960 & `var'!=.) )
		replace implaus_`var'=1 if gestation==41 & (`var'<=2075 | (`var'>=5125 & `var'!=.) )
		replace implaus_`var'=1 if gestation==42 & (`var'<=2110 | (`var'>=5150 & `var'!=.) ) //male minimum here is lower than female, so used male
		
		replace implaus_`var'=1 if gestation==43 & (`var'<=2110 | (`var'>=5150 & `var'!=.) ) //VS added as WHO reference only goes from 22 to 42 weeks
		replace implaus_`var'=1 if gestation==44 & (`var'<=2110 | (`var'>=5150 & `var'!=.) ) //VS added
		replace implaus_`var'=1 if gestation==45 & (`var'<=2110 | (`var'>=5150 & `var'!=.) ) //VS added

		replace implaus_`var'=1 if `var'>=5900 & `var'<. //added VS for implausable birthweight generally
		
		*Female birthweight range
	if sexbaby==0 {	
		replace implaus_`var'=1 if gestation<22 & `var'>=925 & `var'!=. //added VS for gestation 20-21

		replace implaus_`var'=1 if gestation==22 & (`var'<=75   | (`var'>=925  & `var'!=.) )
		replace implaus_`var'=1 if gestation==23 & (`var'<=75   | (`var'>=925  & `var'!=.) )
		replace implaus_`var'=1 if gestation==24 & (`var'<=100  | (`var'>=1075 & `var'!=.) )
		replace implaus_`var'=1 if gestation==25 & (`var'<=125  | (`var'>=1250 & `var'!=.) )
		replace implaus_`var'=1 if gestation==26 & (`var'<=150  | (`var'>=1425 & `var'!=.) )
		replace implaus_`var'=1 if gestation==27 & (`var'<=190  | (`var'>=1600 & `var'!=.) )
		replace implaus_`var'=1 if gestation==28 & (`var'<=210  | (`var'>=1800 & `var'!=.) )
		replace implaus_`var'=1 if gestation==29 & (`var'<=260  | (`var'>=2025 & `var'!=.) )
		replace implaus_`var'=1 if gestation==30 & (`var'<=325  | (`var'>=2275 & `var'!=.) )
		replace implaus_`var'=1 if gestation==31 & (`var'<=400  | (`var'>=2550 & `var'!=.) )
		replace implaus_`var'=1 if gestation==32 & (`var'<=500  | (`var'>=2825 & `var'!=.) )
		replace implaus_`var'=1 if gestation==33 & (`var'<=610  | (`var'>=3125 & `var'!=.) )
		replace implaus_`var'=1 if gestation==34 & (`var'<=750  | (`var'>=3425 & `var'!=.) )
		replace implaus_`var'=1 if gestation==35 & (`var'<=910  | (`var'>=3725 & `var'!=.) )
		replace implaus_`var'=1 if gestation==36 & (`var'<=1100 | (`var'>=3975 & `var'!=.) )
		replace implaus_`var'=1 if gestation==37 & (`var'<=1300 | (`var'>=4225 & `var'!=.) )
		replace implaus_`var'=1 if gestation==38 & (`var'<=1510 | (`var'>=4425 & `var'!=.) )
		replace implaus_`var'=1 if gestation==39 & (`var'<=1725 | (`var'>=4610 & `var'!=.) )
		replace implaus_`var'=1 if gestation==40 & (`var'<=1910 | (`var'>=4750 & `var'!=.) )
		replace implaus_`var'=1 if gestation==41 & (`var'<=2075 | (`var'>=4860 & `var'!=.) )
		replace implaus_`var'=1 if gestation==42 & (`var'<=2210 | (`var'>=4875 & `var'!=.) )
		
		replace implaus_`var'=1 if gestation==43 & (`var'<=2210 | (`var'>=4875 & `var'!=.) ) //VS added
		replace implaus_`var'=1 if gestation==44 & (`var'<=2210 | (`var'>=4875 & `var'!=.) ) //VS added
		replace implaus_`var'=1 if gestation==45 & (`var'<=2210 | (`var'>=4875 & `var'!=.) ) //VS added
				}
		
		*Male birthweight range
	if sexbaby==1 {	//male
		replace implaus_`var'=1 if gestation<22 & `var'>=990 & `var'!=. //added VS for gestation 20-21

		replace implaus_`var'=1 if gestation==22 & (`var'<=200  | (`var'>=990  & `var'!=.) )
		replace implaus_`var'=1 if gestation==23 & (`var'<=200  | (`var'>=990  & `var'!=.) )
		replace implaus_`var'=1 if gestation==24 & (`var'<=225  | (`var'>=1150 & `var'!=.) )
		replace implaus_`var'=1 if gestation==25 & (`var'<=250  | (`var'>=1300 & `var'!=.) )
		replace implaus_`var'=1 if gestation==26 & (`var'<=275  | (`var'>=1475 & `var'!=.) )
		replace implaus_`var'=1 if gestation==27 & (`var'<=300  | (`var'>=1660 & `var'!=.) )
		replace implaus_`var'=1 if gestation==28 & (`var'<=325  | (`var'>=1860 & `var'!=.) )
		replace implaus_`var'=1 if gestation==29 & (`var'<=360  | (`var'>=2090 & `var'!=.) )
		replace implaus_`var'=1 if gestation==30 & (`var'<=400  | (`var'>=2340 & `var'!=.) )
		replace implaus_`var'=1 if gestation==31 & (`var'<=450  | (`var'>=2625 & `var'!=.) )
		replace implaus_`var'=1 if gestation==32 & (`var'<=530  | (`var'>=2925 & `var'!=.) )
		replace implaus_`var'=1 if gestation==33 & (`var'<=625  | (`var'>=3250 & `var'!=.) )
		replace implaus_`var'=1 if gestation==34 & (`var'<=750  | (`var'>=3560 & `var'!=.) )
		replace implaus_`var'=1 if gestation==35 & (`var'<=900  | (`var'>=3860 & `var'!=.) )
		replace implaus_`var'=1 if gestation==36 & (`var'<=1075 | (`var'>=4150 & `var'!=.) )
		replace implaus_`var'=1 if gestation==37 & (`var'<=1260 | (`var'>=4390 & `var'!=.) )
		replace implaus_`var'=1 if gestation==38 & (`var'<=1460 | (`var'>=4600 & `var'!=.) )
		replace implaus_`var'=1 if gestation==39 & (`var'<=1650 | (`var'>=4790 & `var'!=.) )
		replace implaus_`var'=1 if gestation==40 & (`var'<=1840 | (`var'>=4960 & `var'!=.) )
		replace implaus_`var'=1 if gestation==41 & (`var'<=1990 | (`var'>=5125 & `var'!=.) )
		replace implaus_`var'=1 if gestation==42 & (`var'<=2110 | (`var'>=5150 & `var'!=.) )
		
		replace implaus_`var'=1 if gestation==43 & (`var'<=2110 | (`var'>=5150 & `var'!=.) ) //VS added
		replace implaus_`var'=1 if gestation==44 & (`var'<=2110 | (`var'>=5150 & `var'!=.) ) //VS added
		replace implaus_`var'=1 if gestation==45 & (`var'<=2110 | (`var'>=5150 & `var'!=.) ) //VS added
				}
	}

	tab1 implaus_* if deliv==1, mi
	
	replace birthweight=birweit_1 if birweit_1!=birthweight & deliv==1 & birweit_1<5900 & implaus_birweit_1!=1 // if dif BWs in duplicate records, keep BW from birweit_1 variable in original delivery admission, using maximum acceptable birthweight from below/not implaus
	
	replace birthweight=. if implaus_birthweight==1 & hbweight!=1 & lbweight!=1 //only replace birthweight missing if implausable and not indicated as a high birthweight pregnancy/long gestation. 

/* *based on: (access 02/2025)
REF: RCPCH, UK-WHO Child Growth Standards 2009
REF: Cole TJ et al. 1998;17:407-29. Stat.Med.
https://www.rcpch.ac.uk/sites/default/files/Boys_neonatal_and_infant_close_monitoring_growth_chart.pdf 
https://www.rcpch.ac.uk/sites/default/files/Girls_neonatal_and_infant_close_monitoring_growth_chart.pdf 

 lower (-4SD)  upper (99.6th)
gestweeks  lower        upper     gender
23           75           925       F
24           100         1075       F
25           125         1250       F
26           150         1425       F
27           190         1600       F
28           210         1800       F
29           260         2025       F
30           325         2275       F
31           400         2550       F
32           500         2825       F
33           610         3125       F
34           750         3425       F
35           910         3725       F
36           1100       3975       F
37           1300       4225       F
38           1510       4425       F
39           1725       4610       F
40           1910       4750       F
41           2075       4860       F
42           2210       4875       F

23           200         990        M
24           225         1150       M
25           250         1300       M
26           275         1475       M
27           300         1660       M
28           325         1860       M
29           360         2090       M
30           400         2340       M
31           450         2625       M
32           530         2925       M
33           625         3250       M
34           750         3560       M
35           900         3860       M
36           1075       4150       M
37           1260       4390       M
38           1460       4600       M
39           1650       4790       M
40           1840       4960       M
41           1990       5125       M
42           2110       5150       M
*/


*============================================================*
 * 5. Pregnancy window
*============================================================*
*for each del#date, create start, end (=del#date), and post preg variables
	*plan: collapse into which dates are within pregnancy window. identify the window for each pregnancy (del#date-gest or 280, end date= discharge+42) - start/end window for each pregnancy. within record check if admindate happened within preg window (any)

*gestation variable, per row for deliv==1
tempvar gest_day start_preg_d post_preg_d 
	gen `gest_day'= 280 if deliv==1 //40 weeks = 280 days
		replace `gest_day' =gestation *7 if deliv==1 & gestation<.
 * del#date is in all person records, whether it is a delivery or not, but gest_day is not. 

gen `start_preg_d' = admidate_superspell - `gest_day' if deliv==1
gen `post_preg_d' = admidate_superspell + 42  if deliv==1 

 * copy delivery dates over through all admissions, per 15 delivery entries
forval n=1/15 {
gen start_preg_d`n'_t = `start_preg_d' if del_d==`n'
bysort enc: egen start_preg_d`n'=min(start_preg_d`n'_t) //there should only be 1 (_t)
	lab var start_preg_d`n' "Date of delivery #`n' minus gestation/280d"

gen post_preg_d`n'_t = `post_preg_d' if del_d==`n' 
bysort enc: egen post_preg_d`n'=min(post_preg_d`n'_t) //there should only be 1 (_t)
	lab var post_preg_d`n' "42d post date of delivery `n'"

gen end_preg_d`n'_t = del`n'date if del_d==`n' 
	replace end_preg_d`n'_t =admidate_superspell if del_d==`n' & end_preg_d`n'_t==.
bysort enc: egen end_preg_d`n'=min(end_preg_d`n'_t) //there should only be 1 (_t)
	lab var end_preg_d`n' "End of preg/birth `n' began "
	}
	format start_preg_d* post_preg_d* end_preg_d* %td

drop *_preg_d*_t //middle/temp variables

*Pregnancy number
gen pregnancy=.
	lab var pregnancy "Admission during pregnancy #X, start-end pregnancy window"
	notes pregnancy: Indicates which del#date falls within the superspell period gestation/-280d to admission.

	forval n=1/15 {
replace pregnancy=`n' if inrange(admidate_superspell, start_preg_d`n', end_preg_d`n') & start_preg_d`n'<. & end_preg_d`n' <.
		}


*============================================================*
 * 7. Individual EMMOI conditions (by morbid event)
*============================================================*
	*Started from MGB code, condensed/updated by VS 11/2024 (see error_reports for original code)
	*Final triple checks by VS compared to D'Arcy tables on 21/01/2025. Some slight differences in table b4 and b5 in D'Arcy DPhil 2024- included codes indicated in either.
	*dates and labels at the end
	
gen acuteabdomen=.
gen arf=.
gen psychosis=.
gen cardiacarrest=.
gen respcomp=.
gen coma=.
gen dic=. 
gen cva=. 
gen anaesth=.
gen embolism=.
gen embolism_ps=.
gen shock=.
gen sepsis=.
gen sscrisis=. 

	foreach var of varlist diag_01-diag_20 {
	replace acuteabdomen =1 if (inlist (substr(`var',1,3), "K35", "K37")) | (inlist(`var',"K650", "K659", "N733", "N735", "K352", "K358", "K353")) | (inlist(`var',"K562", "K565", "K566", "K593")) 

	replace arf=1 if (inlist (substr(`var',1,3), "N17", "N19")) | (inlist(`var',"O904", "N990", "I120", "I131")) | (inlist(`var', "N170", "N171", "N172", "N178", "N179")) 

	replace psychosis=1 if (inlist (substr(`var',1,3), "F23")) | (inlist(`var',"F230", "F231", "F232", "F238", "F239", "F531")) 

	replace cardiacarrest=1 if (inlist (substr(`var',1,3), "I21", "I42", "I43", "I46", "I50")) | (inlist(`var',"O891", "O742", "O903", "I110", "I119", "I130", "I132"))| (inlist(`var', "I210", "I211", "I212", "I213", "I214", "I219"))| (inlist(`var', "I420", "I421", "I422", "I423"))| (inlist(`var', "I424", "I425", "I426", "I427", "I428", "I429"))| (inlist(`var', "I430", "I431", "I432", "I438", "I460")) | (inlist(`var', "I461", "I469", "I500", "I501", "I509"))  //"J80", "J81" moved to separate respcomp variable
	
	replace respcomp=1 if (inlist(`var',"J80", "J81")) //moved from cardiacarrest

	replace coma=1 if (inlist(`var',"G936", "R402")) 

	replace dic=1 if (inlist (substr(`var',1,3), "D65")) | (inlist(`var',"O450", "O460", "O670", "O723")) //DArcy code (table b4 adds 0723, but not table b5- included)

	replace cva=1 if (inlist (substr(`var',1,3), "I60", "I61", "I62", "I63", "I64")) | (inlist(`var',"I600", "I601", "I602", "I603", "I604")) | (inlist(`var', "I605", "I606", "I607", "I608", "I609")) | (inlist(`var',"I610", "I611", "I612", "I613", "I614")) | (inlist(`var', "I615", "I616", "I617", "I618", "I619"))| (inlist(`var',"I620", "I621", "I629")) | (inlist(`var',"I630", "I631", "I632", "I633", "I634")) | (inlist(`var',"I635", "I636", "I637", "I638", "I639")) 
	
	replace anaesth=1 if (inlist (substr(`var',1,3), "O740", "O741", "O742", "O743", "O749", "O890")) | (inlist(`var', "O891", "O892", "O290", "O291", "O292", "O296", "O293"))  

	replace embolism=1 if (inlist(`var',"O880", "O881", "O882", "O883", "O888", "I260", "I269")) | (inlist (substr(`var',1,3), "O88", "I26")) 
		replace embolism_ps=1 if (inlist(`var', "O882", "O883", "O888", "I260", "I269")) | (inlist (substr(`var',1,3), "I26")) //new code Apr 2025, removes air and amniotic embolism from variable
	
	replace shock=1 if (inlist(`var', "R570", "R571", "R572", "R578", "R579"))| (inlist(`var', "O751", "T805", "T886", "T782", "T780", "A483")) //DArcy code - A483 in table b5 (final), but not b4- included

	replace sepsis=1 if (inlist(`var', "O753", "A400", "A401", "A402", "A403", "A408", "A409")) | (inlist(`var', "A410", "A411", "A412", "A413", "A414", "A415")) | (inlist(`var', "A418", "A419", "A327"))| (inlist (substr(`var',1,3), "A40", "O85")) 

	replace sscrisis=1 if (inlist(`var', "D570")) 
	}

gen asthma1=.
gen epilepsy1=.
gen rupture=.
gen eclampsia=.
gen cvt=. 
gen panc=.
gen dissection=.
gen dka=. 

	foreach var of varlist diag_01-diag_20 {
	replace asthma1=1 if (inlist (substr(`var',1,3), "J46")) 

	replace epilepsy1=1 if (inlist (substr(`var',1,3), "G41")) | (inlist(`var',"G410", "G411", "G412", "G418", "G419")) 

	replace rupture=1 if (inlist(`var',"O710", "O711"))  //uterine rupture

	replace eclampsia=1 if (inlist(`var',"O150", "O151", "O152", "O159")) | (inlist (substr(`var',1,3), "O15")) 

	replace cvt=1 if (inlist(`var',"O873", "I636", "I676")) 

	replace panc=1 if (inlist(`var',"K850", "K851", "K852","K853", "K858", "K859", "K863" ))|(inlist (substr(`var',1,3), "K85")) 

	replace dissection=1 if (inlist(`var',"I710", "I711", "I713", "I715", "I718", "1722", "1723")) | (inlist(`var', "I712", "I714", "I716", "I719")) 

	replace dka=1 if (inlist(`var',"E100", "E101")) 

	}

gen vent=.
gen curr=. //evacuation of the uterus 
gen dialysis=.
gen hysterectomy=.

	foreach var of varlist opertn_01-opertn_24 {
		replace vent=1 if (inlist(`var',"E851", "E852", "E421", "E422", "E423")) |(inlist(`var', "E428", "E429", "E856", "E858", "E859")) |(inlist(`var', "E899", "E898", "Y733", "Y731", "Y732")) 

		replace curr=1 if (inlist(`var',"R281", "R288", "R289", "Q101", "Q102", "Q103", "Q108", "Q109")) | (inlist (substr(`var',1,3), "Q10"))|(inlist(`var',"Q111", "Q115", "Q116", "Q113")) 

		replace dialysis=1 if (inlist (substr(`var',1,3), "X40")) | (inlist(`var',"X411", "X421")) | (inlist(`var', "X401", "X402", "X403", "X404", "X405"))| (inlist(`var', "X406","X407","X408", "X409")) 

		replace hysterectomy=1 if (inlist(`var', "Q072", "Q074", "Q075", "R251"))   //Q072 in Table b4, but not b5 - included
		 }


gen collection=.
gen vaghaem=.
gen interventionhaem=. //was interventionmajorhaem. VS shortened. Arrest major haemorrhage following birth
gen cswound=.
gen cystostomy=. //Repair of damage to bladder or urinary tract
gen intestine=. // Repair... intestine and management of intestinal obstruction
gen managementacs=. // acute coronary syndrome
gen managementthrombo=.
gen managementspleenhaem=.
gen interventionstroke=.
gen repairaorta=.
gen managementpanc=.

	foreach var of varlist opertn_01-opertn_24 {
	
		replace collection=1 if (inlist(`var',"T341", "T342", "T343", "T348", "T349", "T451")) | (inlist(`var', "T452", "T453", "T454", "Y221")) | (inlist (substr(`var',1,3),"Y22")) | (inlist(`var', "T468", "T469", "T963", "Y055", "Y228")) | (inlist(`var', "Y229", "Y251", "Y252", "Y321", "Y322"))| (inlist(`var', "Y323", "Y328", "Y329", "T301", "T302"))| (inlist(`var', "T303", "H031", "H581", "H583")) 

		replace vaghaem=1 if (inlist(`var',"P093", "P271")) 

		replace interventionhaem=1 if (inlist(`var',"L702", "L703", "L713", "L933")) | (inlist(`var', "L941", "L995", "L531", "L538")) | (inlist(`var', "L539", "L543", "L663", "L721")) | (inlist(`var', "L937", "L974", "L975", "Y781")) | (inlist(`var', "Y782", "Y793", "Y788", "Y789", "Y799"))  

		replace cswound=1 if (inlist(`var',"T283", "T288", "T289" )) //DArcy code - fixed error - removed "S604" "T304" "S423", "S424"
		
		replace cystostomy=1 if (inlist(`var',"M373", "M378", "M379", "M736", "M737", "M738")) | (inlist(`var', "M136", "M182", "M188", "M189"))| (inlist(`var', "M191", "M192", "M194"))| (inlist(`var', "M201", "M202", "M203", "M208", "M209"))| (inlist(`var', "M211", "M212", "M213", "M214", "M216"))| (inlist(`var', "M221", "M222", "M228", "M229"))| (inlist(`var', "M274", "M277", "M278", "M279"))| (inlist(`var', "M294", "M331", "M332", "M338", "M339"))| (inlist(`var', "M734", "M738", "M739")) //DArcy code - fixed error -"M372", "M375", "M382", "M383"
		
		replace intestine=1 if (inlist (substr(`var',1,3), "G58", "G69", "G78", "H05")) | (inlist (substr(`var',1,3), "H06", "H07", "H08", "H10")) | (inlist (substr(`var',1,3), "H13", "H29", "H33")) | (inlist(`var', "G784", "G785", "G786", "H061", "H062", "H069")) | (inlist(`var', "H071", "H072", "H073", "H074", "H075", "H078", "H079")) | (inlist(`var', "H081", "H082", "H083", "H084", "H085", "H086")) | (inlist(`var', "H088", "H089", "H091", "H092", "H112", "H113")) | (inlist(`var', "H114", "H115", "H116", "H118", "H119")) | (inlist(`var', "T374", "T384")) | (inlist(`var', "H172", "H173", "H174", "H175", "H176")) | (inlist(`var', "H158", "H159", "H151", "H152", "H131", "H132")) | (inlist(`var', "H133", "H134", "H135", "H138", "H139", "H101", "H102")) | (inlist(`var', "H103", "H104", "H105", "H106", "H108", "H109")) | (inlist(`var', "H051", "H052", "H053", "H058", "H059", "G633")) | (inlist(`var', "G721", "G722", "G723", "G724", "G725")) | (inlist(`var', "G741", "G742", "G743", "G762", "G763", "T305")) // D'Arcy table b5

		replace managementacs=1 if (inlist (substr(`var',1,3), "L76", "L89")) | (inlist(`var',"K634", "K635", "K636", "K638", "K639", "K651"))| (inlist(`var',"K652", "K653", "K654","K658", "K659", "K751"))| (inlist(`var',"K751", "K752", "K753", "K754", "K758", "K759")) 

		replace managementthrombo=1 if (inlist(`var',"L124", "L131", "L961", "L962", "L968", "L969"))| (inlist(`var',"L791", "L993", "L994")) 
		
		replace managementspleenhaem=1 if (inlist(`var',"J691","J692", "J698", "J699"))| (inlist(`var',"J701", "J722", "J724", "J725")) 
		
		replace interventionstroke=1 if (inlist(`var',"L331", "L332","L333", "L334", "L338"))| (inlist(`var',"L339", "L343", "L344", "L351"))| (inlist(`var',"L352", "L353", "L354", "L358"))| (inlist(`var',"L359", "L961", "L962", "O011"))| (inlist(`var',"O012", "O013", "O014", "O018")) | (inlist(`var',"O019", "O021", "O022", "O023"))| (inlist(`var',"O028", "O029", "O031", "O032"))| (inlist(`var',"O033", "O034", "O035", "O036")) | (inlist(`var',"O038", "O039", "O041", "O042"))| (inlist(`var',"O043", "O048", "O049")) 
		
		replace repairaorta=1 if (inlist (substr(`var',1,3), "L18", "L19", "L27", "L29", "O20")) | (inlist(`var',"L265","L266", "L267")) 
		
		replace managementpanc=1 if (inlist(`var',"J576","J601", "J602", "J603")) | (inlist(`var',"J612","J613", "J614")) 
	}


gen bloodflowut=. //not in D'Arcy EMMOI list
	foreach var of varlist opertn_01-opertn_24 {
		replace bloodflowut=1 if (inlist(`var',"L693", "L694", "L702", "L703", "L713")) | (inlist(`var', "L933", "L941", "L946", "L947", "L991"))| (inlist(`var',"L995", "L996"))
		 }



gen sepsis_o753=.
gen sepsis_a327=.
gen sepsis_a40=.
gen sepsis_a41=.
gen sepsis_o85=.

	foreach var of varlist diag_01-diag_20 {
		replace sepsis_o753=1 if (inlist(`var', "O753"))
		replace sepsis_a327=1 if (inlist(`var', "A327"))
		replace sepsis_a40=1 if (inlist (substr(`var',1,3), "A40"))
		replace sepsis_a41=1 if (inlist (substr(`var',1,3), "A41"))
		replace sepsis_o85=1 if (inlist (substr(`var',1,3), "O85"))
	}

*============================================================*
 * 8. EMMOI individual before/during birth (Rhiannon D'Arcy)
*============================================================*

 * Create event date and variable which checks if event happened within each pregnancy window, for the 15 deliveries:
	//Start of preg to 42 days
foreach var of varlist acuteabdomen arf psychosis cardiacarrest coma dic cva anaesth embolism embolism_ps shock sepsis sscrisis asthma1 epilepsy1 rupture eclampsia cvt panc vent dialysis respcomp dka dissection collection managementacs managementthrombo managementspleenhaem interventionstroke repairaorta managementpanc { 
	gen `var'_date=admidate if `var'==1
		bysort enc:egen `var'_date1=min(`var'_date)
		format `var'_date `var'_date1 %td
	
	forval n=1/15 {
		gen `var'_d`n'_t = inrange(`var'_date, start_preg_d`n', post_preg_d`n') & start_preg_d`n'<. & post_preg_d`n' <.
			replace `var'_d`n'_t = .m if start_preg_d`n'==. | post_preg_d`n' ==.
			bysort enc:egen em_`var'_d`n'=max(`var'_d`n'_t) if `var'_d`n'_t < .
			lab var em_`var'_d`n' "Event `var' occurred within pregnancy window #`n'"
		}
	}	

	drop *_t //partial cleaning variables here
	
	//End of preg to 42 days: hysterectomy interventionhaem cswound vaghaem cystostomy intestine curr
foreach var of varlist hysterectomy interventionhaem cswound vaghaem cystostomy intestine curr sepsis_o753 sepsis_a327 sepsis_a40 sepsis_a41 sepsis_o85 {
	gen `var'_date=admidate if `var'==1
		bysort enc:egen `var'_date1=min(`var'_date)
		format `var'_date `var'_date1 %td
	
	forval n=1/15 {
		gen `var'_d`n'_t = inrange(`var'_date, end_preg_d`n', post_preg_d`n') & end_preg_d`n'<. & post_preg_d`n' <.
			replace `var'_d`n'_t = .m if end_preg_d`n'==. | post_preg_d`n' ==.
			bysort enc:egen em_`var'_d`n'=max(`var'_d`n'_t) if `var'_d`n'_t < .
			lab var em_`var'_d`n' "Event `var' occurred within pregnancy window #`n', end-42d"
		}
	}	


	//Finalise  individual EMMOI event variables, if event occurred in current pregnancy
foreach var of varlist acuteabdomen arf psychosis cardiacarrest coma dic cva anaesth embolism embolism_ps shock sepsis sepsis_o753 sepsis_a327 sepsis_a40 sepsis_a41 sepsis_o85 sscrisis asthma1 epilepsy1 rupture eclampsia cvt panc vent dialysis respcomp dka dissection collection managementacs managementthrombo managementspleenhaem interventionstroke repairaorta managementpanc hysterectomy interventionhaem cswound vaghaem cystostomy intestine curr {
	di _newline "*Generate em_`var'______________"
	gen em_`var'=0 if `var'_date1<. //occurred ever for woman
	forval n=1/15 {
		replace em_`var' =1 if del_d ==`n' & em_`var'_d`n'==1 //occurred within current pregnancy line
		}
	notes em_`var': EMMOI event ocurring in current pregnancy (start/end of pregnancy to 42d post birth)
	notes em_`var': Missing means it did not occurr in this dataset.
}



*Label individual EMMOI variables, within pregnancy window
	*time frames are: Start of pregnancy up to 42 days after birth, or
	*End of pregnancy up to 42 days after birth
label var em_acuteabdomen "Acute abdomen"
label var em_arf "Acute renal failure"
label var em_psychosis "Acute psychosis"
label var em_cardiacarrest "Acute cardiac event (cardiac infarction/failure/arrest, cardiomyopathy)"
label var em_coma "Cerebral oedema or coma"

label var em_dic "Disseminated intravascular coagulopathy"
label var em_cva "Cerebrovascular accident"
label var em_anaesth "Major complications of anaesthesia"
label var em_embolism "Embolic event (pulmonary, amniotic fluid, septic and air embolisms)"
	label var em_embolism_ps "Embolic event (ONLY pulmonary & septic)"
label var em_shock "Shock"

label var em_sepsis "Sepsis"
label var em_sscrisis "Sickle cell anaemia with crisis"
label var em_asthma1 "Status asthmaticus"
label var em_epilepsy1 "Status epilepticus"
label var em_rupture "Uterine rupture"

label var em_eclampsia "Eclampsia"
label var em_cvt "Cerebral venous thrombosis"
label var em_panc "Acute pancreatitis"
label var em_vent "Respiratory support"
label var em_curr "Surgical evacuation of the uterus following birth, end to 42d post"

label var em_dialysis "Dialysis"
label var em_hysterectomy "Hysterectomy, end to 42d post"
label var em_respcomp "Acute respiratory compromise"
label var em_dka "Diabetic ketoacidosis"

label var em_dissection "Rupture of aortic aneurysm or dissection of aorta"
label var em_collection "Management of intra-abdominal or pelvic collection"
label var em_vaghaem "Management of vulval or vaginal haematoma, end to 42d post"
label var em_interventionhaem "Interv/surgical proced, to arrest major haemorrhage following birth, end to 42d post"
label var em_cswound "Management of caesarean wound dehiscence, end to 42d post"

label var em_cystostomy "Repair of damage to bladder or urinary tract, end to 42d post"
label var em_intestine "Repair of damage to the intestine & management of intestinal obstruction, end to 42d post"
label var em_managementacs "Management of acute coronary syndrome"
label var em_managementthrombo "Interv/surgical proced to manage thromboembolism"
label var em_managementspleenhaem "Interv/surgical proced, manage major haemorrhage orig from spleen"

label var em_interventionstroke "Interv. procedures to treat haemorrhagic or ischaemic stroke"
label var em_repairaorta "Repair of aortic aneurysm or aortic dissection"
label var em_managementpanc "Surgical management of acute pancreatitis"

lab define yesno 0 "No" 1 "Yes"
	lab val em_* yesno

	lab define emmoi_lbl 0 "No, outside preg" 1 "Yes", replace

	foreach var of varlist acuteabdomen arf psychosis cardiacarrest coma dic cva anaesth embolism embolism_ps shock sepsis sscrisis asthma1 epilepsy1 rupture eclampsia cvt panc vent curr dialysis hysterectomy respcomp dka dissection collection vaghaem interventionhaem cswound cystostomy intestine managementacs managementthrombo managementspleenhaem interventionstroke repairaorta managementpanc {
		lab val em_`var' emmoi_lbl
	}

	
*List of EMMOI variables: em_acuteabdomen em_arf em_psychosis em_cardiacarrest em_coma em_dic em_cva em_anaesth em_embolism em_shock em_sepsis em_sscrisis em_asthma1 em_epilepsy1 em_rupture em_eclampsia em_cvt em_panc em_vent em_curr em_dialysis em_hysterectomy  em_respcomp em_dka em_dissection em_collection em_vaghaem em_interventionhaem em_cswound em_cystostomy em_intestine em_managementacs em_managementthrombo em_managementspleenhaem em_interventionstroke em_repairaorta em_managementpanc 

  /// end of individual emmoi conditions ///
*------------------------------------------------------------*


*============================================================*

* Drop temporary variables
	drop *_t *_date sepsis_o753* sepsis_a327* sepsis_a40* sepsis_a41* sepsis_o85* //partial cleaning variables here (to reduce size of dataset) //temp variables which end in  "_d`n'_t" in the code
	drop em_*_d? em_*_d?? //emmoi event by pregnancy number (individual variable)- not needed in final dta
	drop acuteabdomen arf psychosis cardiacarrest coma dic cva anaesth embolism embolism_ps shock sepsis sscrisis asthma1 epilepsy1 rupture eclampsia cvt panc vent curr dialysis hysterectomy respcomp dka dissection collection vaghaem interventionhaem cswound cystostomy intestine managementacs managementthrombo managementspleenhaem interventionstroke repairaorta managementpanc

compress
*============================================================*
	*Save dataset for next step:
save hes2024_mmn_prep1.dta, replace 

log close

exit
 