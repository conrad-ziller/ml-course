*************************************************************************************************
*Mulitlevel modelling, University of Zurich, Fall 2021, Instructor: Conrad Ziller               *
*************************************************************************************************

set more off								// no- breaks- in output window
capture log close							// close the possibly opened Log-Files
log using ml-course_day3.log, replace 		// open new Log-Files


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





***********************
* Growth Curve Models *
***********************

use https://data.princeton.edu/pop510/egm, clear

describe
codebook year childid schoolid 


* Empty model for estimating math scores to inspect components of variance
mixed math || schoolid: || childid: 
estat icc


* Growth curve model without predictors
mixed math year   || schoolid: year, cov(un)   || childid:  year, cov(un) var //year is centered, i.e. 0 = middle of 3rd grade!


* Growth curve model with predictors
codebook black hispanic lowinc //lowinc = proportion of low-income students

mixed math year black hispanic lowinc  || schoolid: year, cov(un)   || childid:  year, cov(un) var  //Explanation of intercept variance

mixed math year black hispanic c.year#c.black c.year#c.hispanic  || schoolid: year, cov(un)   || childid:  year, cov(un) var //Interactions to explain slope variance

mixed math year black hispanic lowinc c.year#c.black c.year#c.hispanic c.year#c.lowinc  || schoolid: year, cov(un)   || childid:  year, cov(un) var




********************************
* Logistic Multilevel Analysis *
********************************
* Load data

use ess1_recoded.dta, clear

* Recode vote at last election

ta vote 
ta vote, nol
recode vote 1=1 2=0 3=.

* Estimate empty ML model
xtmelogit vote ||id_c: , var ml

* calculate ICC 
estat icc
di .2715319/ (.2715319 + _pi^2 / 3)

* Random Intercept Model
xtmelogit vote educyears age female relig  ln_gdp ||id_c:  , var ml
est sto M4


* Random Slope Model
xtmelogit vote educyears age female relig  ln_gdp   ||id_c:  age   , var ml   intpoints(2)
eststo M5

lrtest M4 M5

* Marginal effects
margins, dydx(*) predict(mu fixedonly) post
est sto M5


 

*******************************
log close




	
