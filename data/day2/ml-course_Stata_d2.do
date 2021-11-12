*************************************************************************************************
*Mulitlevel modelling, University of Zurich, Fall 2021, Instructor: Conrad Ziller               *
*************************************************************************************************

set more off								// no- breaks- in output window
capture log close							// close the possibly opened Log-Files
log using ml-course_day2.log, replace 		// open new Log-Files




***********************************
* Multilevel versus Fixed Effects *
***********************************


use https://www.stata-press.com/data/r12/productivity.dta, clear //Panel or TSCS data on US states (48) over 16 years

describe
codebook state year gsp emp

gen time=year-1970
ta time

graph twoway (line gsp year, connect(ascending)),   xtitle(Year) ytitle(Gross State Product logged)
graph twoway (line emp year, connect(ascending)),   xtitle(Year) ytitle(Non-agriculture payrolls logged)


* Multilevel model, non-agricultural development as driver of GDP
mixed gsp emp time || state:  

* Alternative: state fixed effects 
reg gsp emp time  i.state
reg gsp emp i.time  i.state //two-way fixed effects

/*same approach but with different commands
xtset state time 
xtreg gsp emp time   , re mle
xtreg gsp emp time   , fe 
xtreg gsp emp i.time   , fe 
*/

*Another alternative: Hybrid multilevel model
bys state: center gsp emp time , pre(wi_) mean(bw_)

//Hybrid-Modell schätzen 
mixed gsp   wi_emp bw_emp i.time   ||state:  


************************************
* Three-level multilevel model ESS *
************************************

use "ess8.dta", clear
append using "ess5.dta", gen(ess5)

//Year variable
gen year = 2017
replace year = 2010 if ess5 == 1
	
//context in long format 
	gen gdp = gdp_2016 if year == 2017
		replace gdp = gdp_2010 if year == 2010
		
	gen gei = gei_2017 if year == 2017
		replace gei = gei_2010 if year == 2010		



//Models DV gender equality threat predicted by liberal gender policies (Level 2 variable)
*Cross-sectional with two waves
mixed gender_threat1 age i.edu sex i.urban income   gdp gei i.year	 ||cntry_2:	
bys year: ta cntry if e(sample) //check if all countries appear at both time points
drop if cntry=="SK" | cntry=="AT" | cntry=="CY" | cntry=="DK" | cntry=="HR" | cntry=="IT" | cntry=="BG" 

//Grouping Variable erstellen
		egen cyear=group(cntry_2 year)
		encode cntry_2, gen(c)
		
*Model taking country years, country, and time via a dummy into account		
mixed gender_threat1 age i.edu sex i.urban income gdp gei i.year  ||cntry_2: ||cyear:	

*Model with country & time fixed effects, country-year & country random intercepts,	
mixed gender_threat1 age i.edu sex i.urban income gdp gei i.year i.c ||cntry_2: ||cyear:
		
		
		
************************************
* Model without interactions *
************************************
use essdata_d1.dta , clear //loading data from DAY1


gen ln_gdp=ln(gdp_2018)

mixed trust income age relig  ln_gdp  ||id_c: income,  cov(un)
eststo M5


*Interactions

* Model with interaction: explanation of the RS for education with GDP / c
mixed trust income   age   relig  ln_gdp c.age#c.ln_gdp ||id_c: income age ,  cov(un) 
est sto M6
estat sum

sum ln_gdp if e(sample)

// Effect of age moderated by wealth (max. range of GDP-variable)
margins , dydx(age) at(ln_gdp=(8.7 (0.1) 11.2)) 
marginsplot,     scheme(plotplain) graphregion(fcolor(white) ilcolor(white) lcolor(white)) recast(line) yline(0) recastci(rline) ciopts(lpattern(dash)) legend(off) ///
xtitle("GDP/c (log)", size(large)) ytitle("Effekt von Alter auf Vertrauen", size(large)) title("")
graph export g6.png, replace

// Effect of GDP moderated by age (max. range of age variable)
margins , dydx(ln_gdp) at(age=(15 (5) 90)) 
marginsplot,  yline(0)   scheme(s1mono) graphregion(fcolor(white) ilcolor(white) lcolor(white)) recast(line) recastci(rline) ciopts(lpattern(dash)) legend(off) ///
xtitle("Alter", size(large)) ytitle("Effekt von GDP/c auf Vertrauen", size(large)) title("")
graph export g7.png, replace


*****Creating tables
esttab M0 M1 M5 M6 using "Table_1.rtf", b(3)  star(* 0.05 ** 0.01 )  nogap varwidth(25)  se label stats(N r2) transform(ln*: exp(@) exp(@)) replace 






**********************************
* Mediation						 *
**********************************

* loading ESS data 

use ess1_recoded.dta, clear

* Measurement model social trust (CFA)

sem	(ppltrst <-A) /// (X1=c1+f1*A+e1)
(pplfair <-A) /// (X2=c2+f2*A+e2)
(pplhlp  <-A) /// (X3=c3+f3*A+e3)
, latent(A) stand // definiere latente Variable
predict Trust_latent, latent 

* Example 1

sem (ppltrst pplfair pplhlp <- Trust) ///
(Trust <- income educyears) , latent(Trust) stand
estat gof , stats(all) //zeigt fit-Indizes an

* Example 2

sem (ppltrst pplfair pplhlp <- Trust) ///
(Trust <- income educyears) ///
(income <- educyears) , latent(Trust) stand  
estat teffects, stand //zeigt indirekte Effekte an
estat gof , stats(all)


* Example 3 (for correct standard errors based on latent level-2 variables use Mplus/Lavaan)
//means at level 2 in advance (Mplus/Lavaan does this on the fly)
bys id_c: center trust income educyears , mean(m_) 

gsem (trust <- income educyears m_income m_educyears M1[id_c]) (income <- educyears M1[id_c]) (m_income <- m_educyears M1[id_c]), intpoints(2)     //does not converge

*Alternative with other integration points and only 5 iterations
gsem (trust <- income educyears m_income m_educyears M1[id_c]) (income <- educyears M2[id_c]) (m_income <- m_educyears M3[id_c]), intpoints(2) iter(6) cov(M1[id_c]*M2[id_c]@0 M1[id_c]*M3[id_c]@0 M3[id_c]*M2[id_c]@0) 


nlcom _b[trust:income]*_b[income:educyears]  //indirect effect Level 1
nlcom _b[trust:m_income]*_b[m_income:m_educyears]  //indirect effect Level 2





 

*******************************
log close




	
