// Prevent "more" messages from appearing
set more off
// Control line length
set linesize 150

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
log using $filesave\PSQF7375_Clustered_Example5a_STATA_Output.log, replace name(STATA_Example5a)

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

// Center school mean math
gen SMmath50 = (SMmath-50)/10
label variable SMmath50 "SMmath: School Mean Math (0=5)"

// Center to get within-school math
gen WSmath = (math-SMmath)/10
label variable SMmath "WSmath: Within-School Math (0=SM)"

// Drop schools with <= 30 students
drop if Nperschool < 31

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
////////                 BEGIN ANALYSES FOR EXAMPLE 5A                   ///////
////////////////////////////////////////////////////////////////////////////////

// Add option "or" to model options in melogit get odds ratios for fixed effects

display as result "STATA Model 1: Empty Means, Single-Level Logistic Model Predicting FRlunch"
melogit frlunch ,  
        estat ic, n(94), // getting AIC and BIC equivalent to SAS
        nlcom 1/(1+exp(-1*(_b[_cons]))) // fixed intercept in probability

display as result "STATA Model 2: Empty Means, Two-Level Logistic Model Predicting FRlunch"
melogit frlunch,  ||  schoolID:  , covariance(unstructured) intpoints(15),
        estat ic, n(94),
        nlcom 1/(1+exp(-1*(_b[_cons]))) // fixed intercept in probability
     
display as result "STATA Model 3: Add Level-2 Fixed Effect of School Mean Math"
melogit frlunch c.SMmath50,  ||  schoolID:  , covariance(unstructured) intpoints(15) or,
        estat ic, n(94),
        margins , at(c.SMmath50=(-1(1)1)) predict(xb) // unit-specific logits
        margins , at(c.SMmath50=(-1(1)1))             // marginal probabilities

display as result "STATA Model 4: Add Level-1 Fixed Effect of Group-MC Student Math"
melogit frlunch c.SMmath50 c.WSmath,  ||  schoolID:  , covariance(unstructured) intpoints(15),
        estat ic, n(94),
        estimates store FixMath,             // save LL for LRT
        lincom c.WSmath*-1 + c.SMmath50*1    // Between-School Contextual Effect of Math
    
display as result "STATA Model 5: Add Random Effect of Group-MC Student Math"
melogit frlunch c.SMmath50 c.WSmath,  ||  schoolID: WSmath, ///
           covariance(unstructured) intpoints(15),
        estat ic, n(94),
        estimates store RandMath   // save LL for LRT
        lrtest RandMath FixMath    // LRT against fixed effect model
     
display as result "STATA Model 6: Add Intra-Variable Interactions of School Mean Math and GMC Student Math"
melogit frlunch c.SMmath50 c.WSmath c.SMmath50#c.WSmath c.SMmath50#c.SMmath50, ///
           ||  schoolID: WSmath, covariance(unstructured) intpoints(15),
        estat ic, n(94),
        lincom c.WSmath*-1 + c.SMmath50*1                        // Contextual Math Main Effect
        lincom c.SMmath50#c.WSmath*-1 + c.SMmath50#c.SMmath50*1  // Contextual Math Interaction


// Close log
log close STATA_Example5a 
