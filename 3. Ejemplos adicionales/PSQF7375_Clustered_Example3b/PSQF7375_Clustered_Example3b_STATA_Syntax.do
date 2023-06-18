// Prevent "more" messages from appearing
set more off
// Control line length
set linesize 150
// Control matrix size
set matsize 1000

////////////////////////////////////////////////////////////////////////////////
////////                    BEGIN IMPORT FOR EXAMPLE DATA                ///////
////////                 CHANGE "filesave" to your directory             ///////
////////////////////////////////////////////////////////////////////////////////

// Define global variable for file location to be replaced in code below
// \\Client\ precedes actual path when using UIowa Virtual Desktop
global filesave "C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example3b"
 
// Import example stata data file 
use "$filesave\grade10school.dta", clear

// Save results to separate file
log using $filesave\PSQF7375_Clustered_Example3b_STATA_Output.log, replace name(STATA_Example3b)

// Label existing variables
label variable studentID "studentID: Student ID number"
label variable schoolID  "schoolID: School ID number"
label variable frlunch   "frlunch: Student Free/Reduced Lunch 0=No 1=Yes"
label variable math      "math: Student Free/Reduced Lunch 0=No 1=Yes"

// Get school means of variables and label them
egen SMfrlunch   = mean(frlunch),   by (schoolID)
egen SMmath      = mean(math),      by (schoolID)
label variable SMfrlunch "SMfrlunch: School Mean 0=No, 1=Free/Reduced Lunch"
label variable SMmath    "SMmath: School Mean Math Outcome"

// Get number of students per school
egen Nperschool = count(studentID), by (schoolID)
label variable Nperschool "Nperschool: # Students Contributing Data" 
// Center school mean predictor
gen SMfrlunch30 = SMfrlunch - .30
label variable SMfrlunch30 "SMfrlunch30: % Students with Free Lunch (0=30%)"
// Drop schools with <= 30 students
drop if Nperschool < 31

// Sort in order of ID variables
sort schoolID studentID

display as result "STATA School-Level Descriptives"
preserve  // Save for later use, then compute school-level dataset
collapse  Nperschool SMfrlunch SMmath, by(schoolID)
format    Nperschool SMfrlunch SMmath  %4.2f
summarize Nperschool SMfrlunch SMmath, format
restore   // Go back to student-level dataset

display as result "STATA Student-Level Descriptives"
format    math frlunch %4.2f
summarize math frlunch, format

////////////////////////////////////////////////////////////////////////////////
////////                 BEGIN ANALYSES FOR EXAMPLE 3B                   ///////
////////////////////////////////////////////////////////////////////////////////

display as result "STATA Model 1: 2-Level Empty Means, Random Intercept for Math Outcome"
mixed math  ,         ///
       || schoolID: , variance reml covariance(un) dfmethod(satterthwaite) ///
           dftable(pvalue) residuals(independent),  // residuals: diagonal R matrix default
       estat ic, n(94), // Get Information Criteria
       estat icc        // Get Intraclass Correlation
       estat recovariance, relevel(schoolID)            // Get G matrix for whole sample
       estat wcorrelation, covariance at(schoolID=125)  // Get V matrix for first schoolID
       estat wcorrelation, at(schoolID=125)             // Get VCORR matrix for first schoolID
    
display as result "STATA Model 2: Add Fixed Effect of Student Free/Reduced Lunch"
mixed math c.frlunch, ///
       || schoolID: , variance reml covariance(un) dfmethod(satterthwaite) dftable(pvalue),  
      estat ic, n(94)
    
display as result "STATA Model 3: Add Fixed Effect of School Proportion Free/Reduced Lunch"
mixed math c.frlunch c.SMfrlunch30,  ///
       || schoolID: , variance reml covariance(un) dfmethod(satterthwaite) dftable(pvalue),  
      estat ic, n(94), 
      lincom 1*c.frlunch + 1*c.SMfrlunch30, small    // FR lunch between-school effect
      estimates store FixFRLunch,                    // save LL for LRT
      predict predlunch,                             // save fixed-effect predicted outcomes
      corr math predlunch                        
      display as result r(rho)^2 // total R2
    
display as result "STATA Model 4: Add Random Effect of Student Free/Reduced Lunch"
mixed math c.frlunch c. SMfrlunch30,  ///
       || schoolID: c.frlunch, variance reml covariance(un) dfmethod(satterthwaite) dftable(pvalue),  
       estat ic, n(94),
       estat recovariance, relevel(schoolID)             // Get G matrix for whole sample
       estat recovariance, relevel(schoolID) correlation // Get GCORR matrix for whole sample
       estat wcorrelation, covariance at(schoolID=125)   // Get V matrix for first schoolID
       estat wcorrelation, at(schoolID=125)              // Get VCORR matrix for first schoolID
       estimates store RandFRLunch         // save LL for LRT
       lrtest RandFRLunch FixFRLunch       // LRT against fixed effect model

display as result "STATA Model 5: Add Cross-Level Interaction of Student by School Free/Reduced Lunch"
mixed math c.frlunch c.SMfrlunch30 c.frlunch#c.SMfrlunch30, ///
       || schoolID: c.frlunch, variance reml covariance(un) dfmethod(satterthwaite) dftable(pvalue),   
      estat ic, n(94)

display as result "STATA Model 6: Add Level-2 Interaction of Quadratic School Free/Reduced Lunch"
mixed math c.frlunch c.SMfrlunch30 c.frlunch#c.SMfrlunch30 c.SMfrlunch30#c.SMfrlunch30, ///
       || schoolID: c.frlunch, variance reml covariance(un) dfmethod(satterthwaite) dftable(pvalue),  
      estat ic, n(94),
      test (c.frlunch#c.SMfrlunch30=0) (c.SMfrlunch30#c.SMfrlunch30), small    // Test Omnibus Interaction
      lincom 1*c.frlunch + 1*c.SMfrlunch30, small                              // FR lunch BS simple main effect
      lincom 1*c.frlunch#c.SMfrlunch30 + 1*c.SMfrlunch30#c.SMfrlunch30, small  // FR lunch BS interaction
      margins, at(c.frlunch=(0 1) c.SMfrlunch30=(-.2 0 .2 .4)) vsquish         // create predicted values
      marginsplot, noci name(predicted_lunch, replace) xdimension(frlunch)     // plot predicted, no CI 
      predict predtotal,         // save fixed-effect predicted outcomes
      corr math predtotal                             
      display as result r(rho)^2 // total R2
     
// Close log
log close STATA_Example3b 
