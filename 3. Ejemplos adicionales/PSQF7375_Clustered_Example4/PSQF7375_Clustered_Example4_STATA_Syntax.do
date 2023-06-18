// Prevent "more" messages from appearing
set more off
// Control line length
set linesize 150

////////////////////////////////////////////////////////////////////////////////
////////     BEGIN IMPORT AND MANIPUATION OF CROSSED EXAMPLE DATA        ///////
////////                 CHANGE "filesave" to your directory             ///////
////////////////////////////////////////////////////////////////////////////////

// Define global variable for file location to be replaced in code below
// \\Client\ precedes actual path when using UIowa Virtual Desktop
global filesave "C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example4"
 
// Import example stata data file 
use "$filesave\pupcross.dta", clear

// Save results to separate file
log using $filesave\PSQF7375_Clustered_Example4_STATA_Output.log, replace

// Get means per primary school and secondary school of kid predictor
egen pmSES    = mean(pupses), by (pschool)
egen pmAchiev = mean(achiev), by (pschool)
label variable pmSES    "Primary Mean Student SES"
label variable pmAchiev "Primary Mean Student Achievement"
egen smSES    = mean(pupses), by (sschool)
egen smAchiev = mean(achiev), by (sschool)
label variable smSES    "Secondary Mean Student SES"
label variable smAchiev "Secondary Mean Student Achievement"
 // Center and label predictors
gen  pupSES4 = pupses - 4 
gen  pmSES4  = pmSES - 4  
gen  smSES4  = smSES - 4  
label variable pupSES4 "Student SES (0=4)"
label variable pmSES4  "Primary Mean Student SES (0=4)"
label variable smSES4  "Secondary Mean Student SES (0=4)"


////////////////////////////////////////////////////////////////////////////////
////////                     BEGIN ANALYSES FOR EXAMPLE 4                ///////
////////////////////////////////////////////////////////////////////////////////

display as result "STATA Primary School Descriptives"
preserve  // Save for later use, then compute school-level dataset
collapse  pdenom pmSES pmAchiev, by(pschool)
format    pdenom pmSES pmAchiev %4.2f
summarize pdenom pmSES pmAchiev, format
restore   // Go back to student-level dataset

display as result "STATA Secondary School Descriptives"
preserve  // Save for later use, then compute school-level dataset
collapse  sdenom smSES smAchiev, by(sschool)
format    sdenom smSES smAchiev %4.2f
summarize sdenom smSES smAchiev, format
restore   // Go back to student-level dataset

display as result "STATA Student Descriptives"
format    pupSES achiev %4.2f
summarize pupSES achiev, format

display as result "STATA Empty Means Model 1a: Secondary Random Intercept Only"
mixed achiev ,  ///
        || sschool: ,   variance reml covariance(un) ///
           dfmethod(satterthwaite) dftable(pvalue)  
       estimates store FitNested  // Save for LRT

display as result "STATA Empty Means Model 1b: Primary by Secondary Crossed Random Intercepts"
mixed achiev ,  ///
        || _all: R.sschool ,     ///
        || _all: R.pschool ,   variance reml  ///
           dfmethod(satterthwaite) dftable(pvalue)  
      estimates store FitCrossed   // Save for LRT
      lrtest FitCrossed FitNested  // Request LRT
             
display as result "STATA Model 2: Add School Denomination Variables"
mixed achiev pdenom sdenom,  ///
        || _all: R.sschool ,     ///
        || _all: R.pschool ,   variance reml  ///
           dfmethod(satterthwaite) dftable(pvalue) 
	  predict preddenom,         // save fixed-effect predicted outcomes
      corr achiev preddenom                        
      display as result r(rho)^2 // total R2
             
display as result "STATA Model 3a: Add Student SES"
mixed achiev c.pdenom c.sdenom c.pupSES4,  ///
        || _all: R.sschool ,     ///
        || _all: R.pschool ,   variance reml  ///
            dfmethod(satterthwaite) dftable(pvalue) 
             
display as result "STATA Model 3b: Add Student SES Contextual Effects"
mixed achiev c.pdenom c.sdenom c.pupSES4 c.pmSES4 c.smSES4,  ///
        || _all: R.sschool ,     ///
        || _all: R.pschool ,   variance reml  ///
            dfmethod(satterthwaite) dftable(pvalue)
	  test (c.pupSES4=0)(c.pmSES4=0)(c.smSES4=0), small // Multiv Wald test for SES
	  predict predSES,           // save fixed-effect predicted outcomes
      corr achiev predSES                        
      display as result r(rho)^2 // total R2
             
// STATA models with random slopes are not estimatble correctly 
// because covariance(un) is not allowed


// Close log
log close  

