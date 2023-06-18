* Prevent "more" messages from appearing
set more off
* Control line length
set linesize 150

********************************************************************************
*******               BEGIN DATA MANIPULATION FOR EXAMPLE 2              *******
*******               CHANGE "filesave" to your directory                *******
********************************************************************************

* Defining global variable for file location to be replaced in code below
* \\Client\ precedes actual path when using UIowa Virtual Desktop
global filesave "C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example2"

* Import chapter 2 data into temporary file and center predictors
use "$filesave\STATA_Chapter2.dta", clear
* Centering continuous predictors
gen age85 = age - 85
gen grip9 = grip - 9
* Creating manual contrasts for dementia groups
gen demnf=0
gen demnc=0
* Demgroup = none   
replace demnf=0 if demgroup==1
replace demnc=0 if demgroup==1
* Demgroup = future 
replace demnf=1 if demgroup==2
replace demnc=0 if demgroup==2
* Demgroup = current
replace demnf=0 if demgroup==3
replace demnc=1 if demgroup==3
* Adding value labels 
label define fdemgroup 1 "1None" 2 "2Future" 3 "3Current"
label values demgroup fdemgroup
label define fsex 0 "0Men" 1 "1Women" 
label values sexmw fsex
* Labeling all variables
label variable age85  "age85: Age in Years (0=85)"
label variable grip9  "grip9: Grip Strength in Pounds (0=9)"
label variable sexmw  "sexmw: Sex (0=Men, 1=Women)"
label variable demnf  "demnf: Dementia Contrast for None=0 vs Future=1"
label variable demnc  "demnc: Dementia Contrast for None=0 vs Current=1"
label variable cognition  "Cognition Outcome"
label variable demgroup   "Dementia Group 1N 2F 3C"

    
********************************************************************************
*******                       BEGIN EXAMPLE 2A MODELS                    *******
********************************************************************************

* Save results to separate file
log using $filesave\STATA_Example2a_Output.log, replace name(STATA_Example2a)

display as result "Chapter 2: Descriptive Statistics for Example Variables"
format age grip cognition %4.2f
summarize age grip cognition, format
tabulate sexmw demgroup, cell

display as result "Eq 2.3: Empty Means Model"
mixed cognition , /// 
      variance reml dfmethod(residual) 

display as result "Eq 2.7: Age + Grip + Sex (0=M 1=W, as continuous predictor)"
mixed cognition c.age85 c.grip9 c.sexmw, ///
                  variance reml dfmethod(residual),
	  predict predagegripsex, xb		
	  corr cognition predagegripsex  
      display as result r(rho)^2
	  
display as result "Eq 2.8: Adding Dementia Group"
display as result "Using Manual Group Contrasts so Reference=None"
mixed cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc, ///
                variance reml dfmethod(residual),
      test (c.demnf=0) (c.demnc=0), small // Omnibus Dementia Group Test - change in R2
      predict predagegripsexdem, xb		
	  corr cognition predagegripsexdem  // model R2
      display as result r(rho)^2
 lincom _cons*1 + c.demnf*0 + c.demnc*0,   small // Intercept for None Group = B0
 lincom _cons*1 + c.demnf*1 + c.demnc*0,   small // Intercept for Future Group = B0+B4
 lincom _cons*1 + c.demnf*0 + c.demnc*1,   small // Intercept for Current Group = B0+B5
 lincom _cons*1 + c.demnf*.5 + c.demnc*.5, small // Intercept for Ever Group = B0+(B4+B5)/2
 lincom c.demnf*1  + c.demnc*0,  small // None vs Future = B4
 lincom c.demnf*0  + c.demnc*1,  small // None vs Current = B5
 lincom c.demnf*-1 + c.demnc*1,  small // Future vs Current = B5-B4
 lincom c.demnf*.5 + c.demnc*.5, small // None vs Ever = (B4+B5)/2


display as result "Eq 2.8: Adding Dementia Group"
display as result "Categorical Predictor for Dementia Group"
mixed cognition c.age85 c.grip9 c.sexmw ib(last).demgroup, ///
                  variance reml dfmethod(residual),
      contrast i.demgroup, small 
      margins  i.demgroup, at(c.age85=0 c.grip9=0 c.sexmw=0) df(544)
	  margins  i.demgroup, df(544)
      margins  i.demgroup, pwcompare(pveffects) df(544)
lincom _cons*1 + 1.demgroup*0  + 2.demgroup*.5 + 3.demgroup*.5, small // Intercept Ever = (B0+B5)/2 
lincom           1.demgroup*-1 + 2.demgroup*.5 + 3.demgroup*.5, small // None vs Ever = (B0+B5)/2 -B4

* Close log
log close STATA_Example2a
	  
********************************************************************************
*******                       BEGIN EXAMPLE 2B MODELS                    *******
********************************************************************************	  
	
* Save results to separate file
log using $filesave\STATA_Example2b_Output.log, replace name(STATA_Example2b)	
	
display as result "Eq 2.9: Adding Age by Grip Interaction"
display as result "Age (0=85), Grip (0=9)"
mixed cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc c.age85#c.grip9, ///
                  variance reml dfmethod(residual),
      estat vce,  // Aymptotic covariance matrix of fixed effects for regions
      predict predagegbyrip, xb		
      corr cognition predagegbyrip  // model R2
      display as result r(rho)^2
      lincom c.age85*1 + c.age85#c.grip9*-3, small  // Age Slope at Grip Strength =  6
      lincom c.age85*1 + c.age85#c.grip9*0, small   // Age Slope at Grip Strength =  9
      lincom c.age85*1 + c.age85#c.grip9*3, small   // Age Slope at Grip Strength = 12
      margins, at(c.grip9=(-3(3)3)) dydx(c.age85) df(543) vsquish // Age Slope per Grip
      lincom c.grip9*1 + c.age85#c.grip9*-5, small  // Grip Strength Slope at Age = 80
      lincom c.grip9*1 + c.age85#c.grip9*0, small   // Grip Strength Slope at Age = 85
      lincom c.grip9*1 + c.age85#c.grip9*5, small   // Grip Strength Slope at Age = 90
      margins, at(c.age85=(-5(5)5)) dydx(c.grip9) df(543) vsquish // Grip Slope per Age
      margins, at(c.age85=(-5(5)5) c.grip9=(-3(3)3) c.sexmw=0 c.demnf=0 c.demnc=0) vsquish,
      marginsplot, name(predicted_means, replace)	// Get and plot intercepts
	  
display as result "Eq 2.18: Adding Sex by Age by Grip Three-Way Interaction"
display as result "Age (0=85), Grip (0=9), Sex (0=Men), Dementia (0=None)"
mixed cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc ///
                c.age85#c.grip9 c.sexmw#c.demnf c.sexmw#c.demnc ///
                c.age85#c.sexmw c.grip9#c.sexmw c.age85#c.grip9#c.sexmw, /// 
                  variance reml dfmethod(residual),
test (c.sexmw#c.demnf=0) (c.sexmw#c.demnc=0), small // change in R2 for Sex*DemGroup
test (c.age85#c.sexmw=0) (c.grip9#c.sexmw=0) ///
     (c.age85#c.grip9#c.sexmw=0), small  // change in R2 for Age*Grip*Sex

* Each MARGINS below does what the set of LINCOM statements above it do
lincom c.age85*1 + c.age85#c.grip9*-3 + c.age85#c.sexmw*0  + c.age85#c.grip9#c.sexmw*0,  small // Age for Grip=6 Men
lincom c.age85*1 + c.age85#c.grip9*0  + c.age85#c.sexmw*0  + c.age85#c.grip9#c.sexmw*0,  small // Age for Grip=9 Men
lincom c.age85*1 + c.age85#c.grip9*3  + c.age85#c.sexmw*0  + c.age85#c.grip9#c.sexmw*0,  small // Age for Grip=12 Men
lincom c.age85*1 + c.age85#c.grip9*-3 + c.age85#c.sexmw*1  + c.age85#c.grip9#c.sexmw*-3, small // Age for Grip=6 Wom
lincom c.age85*1 + c.age85#c.grip9*0  + c.age85#c.sexmw*1  + c.age85#c.grip9#c.sexmw*0,  small // Age for Grip=9 Wom
lincom c.age85*1 + c.age85#c.grip9*3  + c.age85#c.sexmw*1  + c.age85#c.grip9#c.sexmw*3,  small // Age for Grip=12 Wom
margins, at(c.grip9=(-3(3)3) c.sexmw=(0(1)1)) dydx(c.age85) df(538) vsquish // Age Slope per Grip for Men, Women

lincom c.grip9*1 + c.age85#c.grip9*-5 + c.grip9#c.sexmw*0  + c.age85#c.grip9#c.sexmw*0, small //Grip for Age=80 for Men
lincom c.grip9*1 + c.age85#c.grip9*0  + c.grip9#c.sexmw*0  + c.age85#c.grip9#c.sexmw*0, small //Grip for Age=85 for Men
lincom c.grip9*1 + c.age85#c.grip9*5  + c.grip9#c.sexmw*0  + c.age85#c.grip9#c.sexmw*0, small //Grip for Age=90 for Men
lincom c.grip9*1 + c.age85#c.grip9*-5 + c.grip9#c.sexmw*1  + c.age85#c.grip9#c.sexmw*-5,small //Grip for Age=80 for Wom
lincom c.grip9*1 + c.age85#c.grip9*0  + c.grip9#c.sexmw*1  + c.age85#c.grip9#c.sexmw*0, small //Grip for Age=85 for Wom
lincom c.grip9*1 + c.age85#c.grip9*5  + c.grip9#c.sexmw*1  + c.age85#c.grip9#c.sexmw*5, small //Grip for Age=90 for Wom
margins, at(c.age85=(-5(5)5) c.sexmw=(0(1)1)) dydx(c.grip9) df(538) vsquish // Grip Slope per Age for Men, Women

lincom c.sexmw*1 + c.age85#c.sexmw*-5 + c.grip9#c.sexmw*-3 + c.age85#c.grip9#c.sexmw*15, small //Sex for Age=80 Grip=6
lincom c.sexmw*1 + c.age85#c.sexmw*0  + c.grip9#c.sexmw*-3 + c.age85#c.grip9#c.sexmw*0,  small //Sex for Age=85 Grip=6
lincom c.sexmw*1 + c.age85#c.sexmw*5  + c.grip9#c.sexmw*-3 + c.age85#c.grip9#c.sexmw*-15,small //Sex for Age=90 Grip=6
lincom c.sexmw*1 + c.age85#c.sexmw*-5 + c.grip9#c.sexmw*0  + c.age85#c.grip9#c.sexmw*0, small  //Sex for Age=80 Grip=9
lincom c.sexmw*1 + c.age85#c.sexmw*0  + c.grip9#c.sexmw*0  + c.age85#c.grip9#c.sexmw*0, small  //Sex for Age=85 Grip=9
lincom c.sexmw*1 + c.age85#c.sexmw*5  + c.grip9#c.sexmw*0  + c.age85#c.grip9#c.sexmw*0, small  //Sex for Age=90 Grip=9
lincom c.sexmw*1 + c.age85#c.sexmw*-5 + c.grip9#c.sexmw*-3 + c.age85#c.grip9#c.sexmw*15,small  //Sex for Age=80 Grip=12
lincom c.sexmw*1 + c.age85#c.sexmw*0  + c.grip9#c.sexmw*0  + c.age85#c.grip9#c.sexmw*0, small  //Sex for Age=85 Grip=12
lincom c.sexmw*1 + c.age85#c.sexmw*5  + c.grip9#c.sexmw*3  + c.age85#c.grip9#c.sexmw*15,small  //Sex for Age=90 Grip=12
margins, at(c.age85=(-5(5)5) c.grip9=(-3(3)3)) dydx(c.sexmw) df(538) vsquish // Sex Slope per Age and Grip

* MARGINS do not appear to work for simple two-way interactions below
lincom c.age85#c.grip9*1  + c.age85#c.grip9#c.sexmw*0,  small      // Age by Grip for Men
lincom c.age85#c.grip9*1  + c.age85#c.grip9#c.sexmw*1,  small      // Age by Grip for Women
lincom c.age85#c.sexmw*1  + c.age85#c.grip9#c.sexmw*-3, small      // Age by Sex for Grip=6
lincom c.age85#c.sexmw*1  + c.age85#c.grip9#c.sexmw*0,  small      // Age by Sex for Grip=9
lincom c.age85#c.sexmw*1  + c.age85#c.grip9#c.sexmw*3,  small      // Age by Sex for Grip=12
lincom c.sexmw#c.grip9*1  + c.age85#c.grip9#c.sexmw*-5, small      // Grip by Sex for Age=80
lincom c.sexmw#c.grip9*1  + c.age85#c.grip9#c.sexmw*0,  small      // Grip by Sex for Age=85
lincom c.sexmw#c.grip9*1  + c.age85#c.grip9#c.sexmw*5,  small      // Grip by Sex for Age=90
* Get predicted means
margins, at (c.age85=(-5(5)5) c.grip9=(-3(3)3) c.sexmw=(0(1)1) c.demnf=0 c.demnc=0) vsquish

* Close log
log close STATA_Example2b
	  
	  
********************************************************************************
*******                       BEGIN EXAMPLE 2C MODELS                    *******
********************************************************************************	  
	
* Save results to separate file
log using $filesave\STATA_Example2c_Output.log, replace name(STATA_Example2c)		  
	  
display as result "Eq 2.13: Adding Sex by Dementia Interaction"
display as result "Sex (0=Men), Dementia (0=None)"
mixed cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc ///
                c.age85#c.grip9 c.sexmw#c.demnf c.sexmw#c.demnc, ///
                  variance reml dfmethod(residual),
	  predict predsexbydem, xb		
	  corr cognition predsexbydem  // model R2
      display as result r(rho)^2
test (c.sexmw#c.demnf=0) (c.sexmw#c.demnc=0), small // Omnibus Dementia*Sex Interaction Test
margins, at(c.age85=0 c.grip9=0 c.sexmw=(0(1)1) c.demnf=0 c.demnc=0) vsquish, // Intercepts for None
margins, at(c.age85=0 c.grip9=0 c.sexmw=(0(1)1) c.demnf=1 c.demnc=0) vsquish, // Intercepts for Future
margins, at(c.age85=0 c.grip9=0 c.sexmw=(0(1)1) c.demnf=0 c.demnc=1) vsquish, // Intercepts for Current
lincom c.sexmw*1 + c.sexmw#c.demnf*0 + c.sexmw#demnc*0, small  // Sex Difference for None Dementia
lincom c.sexmw*1 + c.sexmw#c.demnf*1 + c.sexmw#demnc*0, small  // Sex Difference for Future Dementia
lincom c.sexmw*1 + c.sexmw#c.demnf*0 + c.sexmw#demnc*1, small  // Sex Difference for Current Dementia
lincom c.demnf*1 + c.sexmw#c.demnf*0, small                    // None-Future Difference for Men
lincom c.demnf*1 + c.sexmw#c.demnf*1, small                    // None-Future Difference for Women
lincom c.demnc*1 + c.sexmw#c.demnc*0, small		               // None-Current Difference for Men
lincom c.demnc*1 + c.sexmw#c.demnc*1, small                    // None-Current Difference for Women 
lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*0  + c.sexmw#c.demnc*0, small // Fut-Cur Diff for Men
lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*-1 + c.sexmw#c.demnc*1, small // Fut-Cur Diff for Women
lincom c.sexmw#c.demnf*1, small                      // None-Future Sex Difference
lincom c.sexmw#c.demnc*1, small                      // None-Current Sex Difference
lincom c.sexmw#c.demnf*-1 + c.sexmw#c.demnc*1, small // Future-Current Sex Difference

display as result "Continuous Sex (0=Men) and Categorical Dementia (Ref=Current)"
mixed cognition c.age85 c.grip9 c.sexmw i.demgroup  ///
                c.age85#c.grip9 c.sexmw#i.demgroup, ///
                  variance reml dfmethod(residual),
      contrast c.sexmw#i.demgroup, small // Omnibus test of sex*dementia interaction
      margins  i.demgroup, at(c.sexmw=(0(.5)1) c.age85=0 c.grip9=0)vsquish // Intercepts per cell        
      margins  i.demgroup, at(c.sexmw=.5 c.age85=0 c.grip9=0) ///
				   pwcompare(pveffects) df(541) vsquish // Dem pairwise for MEAN sex        
      margins  i.demgroup, at(c.sexmw=0 c.age85=0 c.grip9=0) ///
				   pwcompare(pveffects) df(541) vsquish // Dem pairwise for Men         
      margins  i.demgroup, at(c.sexmw=1 c.age85=0 c.grip9=0) ///
				   pwcompare(pveffects) df(541) vsquish // Dem pairwise for Women
      lincom c.sexmw*1 + c.sexmw#i1.demgroup*1, small  // Sex diff for None
      lincom c.sexmw*1 + c.sexmw#i2.demgroup*1, small  // Sex diff for Future
      lincom c.sexmw*1 + c.sexmw#i3.demgroup*1, small  // Sex diff for Current
      lincom c.sexmw#i1.demgroup*-1 + c.sexmw#i2.demgroup*1, small  // None-Future Sex Diff
      lincom c.sexmw#i1.demgroup*-1 + c.sexmw#i3.demgroup*1, small  // None-Current Sex Diff
      lincom c.sexmw#i2.demgroup*-1 + c.sexmw#i3.demgroup*1, small  // Future-Current Sex Diff


display as result "Categorical Sex (Ref=Women) and Categorical Dementia (Ref=Current)" 
mixed cognition c.age85 c.grip9 i.sexmw i.demgroup ///
                c.age85#c.grip9 i.sexmw#i.demgroup, ///
                 variance reml dfmethod(residual), 
contrast i.sexmw#i.demgroup, small // Omnibus interaction test
margins  i.sexmw#i.demgroup, at(c.age85=0 c.grip9=0) // cell means 
margins  i.sexmw#i.demgroup, pwcompare(pveffects)    df(541), // all cell comparisons
margins  i.sexmw@i.demgroup, at(c.age85=0 c.grip9=0) df(541), // Simple effect of sex per demgroup
margins  i.demgroup@i.sexmw, at(c.age85=0 c.grip9=0) df(541), // Simple effect of demgroup per sex
contrast {i.sexmw#i.demgroup -1  1  0  1 -1  0}, small // None-Future Sex Diff
contrast {i.sexmw#i.demgroup -1  0  1  1  0 -1}, small // None-Current Sex Diff 
contrast {i.sexmw#i.demgroup  0 -1  1  0  1 -1}, small // Future-Current Sex Diff


* Close log
log close STATA_Example2c
