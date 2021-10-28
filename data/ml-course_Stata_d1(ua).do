*************************************************************************************************
*Mulitlevel modelling, University of Zurich, Fall 2021, Instructor: Conrad Ziller               *
*************************************************************************************************

set more off								// no- breaks- in output window
capture log close							// close the possibly opened Log-Files
log using ml-course_day1.log, replace 		// open new Log-Files


********************
* Data Preparation *
********************

* Loading the 2002 wave of the European Social Survey
use ESS9e03_1.dta, clear

* ID 
encode cntry, gen(id_c)

* Preparation of the individual variables

tab1 ppltrst pplfair pplhlp hinctnt eduyrs  agea gndr rlgdgr

// ST - social trust
gen trust=(ppltrst + pplfair + pplhlp)/3

// UV
gen income=hinctnt
gen age=agea
gen relig=rlgdgr


* Adding the macro variable
sort cntry
merge m:1 cntry using  "gdp_2018.dta"
drop if _merge == 2
drop _merge




***********************
* Multilevel analysis *
***********************

* Empty model

eststo M0
estat icc  //Variante 1 /Version 1





* Random intercept model with individual variables


eststo M1


//Output of variable descriptions after model estimation
estat summarize
predict model if e(sample)==1 //Alternative zu "estat sum", um einbezogene Fälle im Modell sichtbar zu machen //Alternative to "estat sum" to make included cases visible in the model
sum trust income age relig if model!=.



//maximum effect sizes do not always make sense, e.g. in the case of outliers
//Alternative standardization: ß = b * sdx / sdy



//Assessment of model fit (R-squared)


//Alternative assessments: AIC and BIC 
estat ic



**************
* Footnote/  *
**************
//It is usually a wise idea to base all observations on the most complex model we want to show
//We therefore fit a normal regression with all predictor variables and keep only the relevant observations

reg 
keep if e(sample)

//Reestimation Model0 & Model1

eststo M0

eststo M1

//Footnote end



*Random intercept model with individual and context variables


eststo M2

//Standardized effect of GDP



//Clarification of variance (R-squared)

//AIC and BIC 
estat ic


//Model comparison: Likelihood Ratio Test





**Does the effect of income vary across countries?
*Random Slope Modell 

//Random Slopes visualized
encode cntry, gen(c)

local functions ""
forvalues i=1/29 {
capture reg trust income if c == `i'
if !_rc {
local b = _b[income]
local a = _b[_cons]
local functions "`functions' (function y`i'=`a'+`b'*x, range(0 10) lwidth(thin))"
*disp "`functions'"
}
}

graph twoway `functions', legend(on) ylabel(3(1)8) xtitle("Income") ytitle("Trust") scheme( plottig )
graph export g1.png, replace


//ML model

eststo M3

//Model comparison: Likelihood Ratio Test



* Random slope model (covariance between intercept and slope specified)


eststo M4

//Model comparison: Likelihood Ratio Test





***************************
* Residential diagnostics *
***************************

//Level-1
mixed 
predict yhat
predict res1, rstandard
qnorm res1, name(res1)
twoway scatter res1 yhat, yline(0)
graph export g2.png, replace

//Level-2
predict u1 u0, reffects
preserve
collapse yhat u0 u1, by(id_c)
histogram u0, normal name(u0)
histogram u1, normal name(u1)
twoway scatter u0 yhat, yline(0) name(s0) mlabel(id_c)
graph export g3.png, replace
twoway scatter u1 yhat, yline(0) name(s1) mlabel(id_c)
graph export g4.png, replace
restore

//Possible transformation of GDP / c
preserve
collapse gdp_2018, by(id_c)
ladder gdp_2018
gladder gdp_2018
graph export g5.png, replace
restore 


************************************
* Final model without interactions *
************************************

gen ln_gdp=ln(gdp_2018)


eststo M5


*Interactions

* Model with interaction: explanation of the RS for education with GDP / c

est sto M6
estat sum

sum ln_gdp if e(sample)

// Effect of age moderated by wealth (max. range of GDP-variable)
margins , 
marginsplot,     scheme(plotplain) graphregion(fcolor(white) ilcolor(white) lcolor(white)) recast(line) yline(0) recastci(rline) ciopts(lpattern(dash)) legend(off) ///
xtitle("GDP/c (log)", size(large)) ytitle("Effekt von Alter auf Vertrauen", size(large)) title("")
graph export g6.png, replace

// Effect of GDP moderated by age (max. range of age variable)
margins ,  
marginsplot,  yline(0)   scheme(s1mono) graphregion(fcolor(white) ilcolor(white) lcolor(white)) recast(line) recastci(rline) ciopts(lpattern(dash)) legend(off) ///
xtitle("Alter", size(large)) ytitle("Effekt von GDP/c auf Vertrauen", size(large)) title("")
graph export g7.png, replace


*****Creating tables
esttab M0 M1 M5 M6 using "Table_1.rtf", b(3)  star(* 0.05 ** 0.01 )  nogap varwidth(25)  se label stats(N r2) transform(ln*: exp(@) exp(@)) replace 

 

*******************************
log close




	
