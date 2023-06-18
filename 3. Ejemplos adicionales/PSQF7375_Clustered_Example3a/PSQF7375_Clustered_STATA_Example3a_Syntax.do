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
global filesave "C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example3a"

// Import example data excel data file into STATA
clear  // clear memory in case a dataset is already open
import excel "$filesave\Multilevel_Analysis_Missing.xlsx", ///
       sheet("book_data") firstrow case(preserve) 

// Label variables
label variable schoolID   "School ID"
label variable studentID "Student ID"
label variable langpost   "Student Language"
label variable IQverb     "Student Verbal IQ"
label variable IQperf     "Student Performance IQ"
label variable denomina   "School Denomination"
label variable homework   "School Homework Amount"
label variable mixedgra   "School Mixed Grade"
// Select variables to keep
keep schoolID studentID langpost IQverb IQperf denomina homework mixedgra
// Select complete cases
egen nummiss = rowmiss(schoolID studentID langpost IQverb IQperf ///
               denomina homework mixedgra)
drop if nummiss>0
// Remove denomina=4,5 for small sample sizes
drop if denomina==4
drop if denomina==5

// Z-score IQ variables for comparability as predictors
egen IQverbz=std(IQverb)
egen IQperfz=std(IQperf)
// Create school-level variables
egen Nperschool=count(langpost), by(schoolID)
egen SMlangpost=mean(langpost),  by(schoolID)
egen SMIQverbz=mean(IQverbz),    by(schoolID)
egen SMIQperfz=mean(IQperfz),    by(schoolID)
// Label school-level variables
label variable SMlangpost "School Mean Language"
label variable SMIQverbz  "School Mean Verbal IQ Z" 
label variable SMIQperfz  "School Mean Performance IQ Z"
// Contrasts for school denomination -- initialize to missing
gen den1v2=.
gen den1v3=.
replace den1v2=0 if denomina==1
replace den1v3=0 if denomina==1
replace den1v2=1 if denomina==2
replace den1v3=0 if denomina==2
replace den1v2=0 if denomina==3
replace den1v3=1 if denomina==3
label variable den1v2 "Denom 1 vs 2"
label variable den1v3 "Denom 1 vs 3"
// Center school predictors near sample mean
gen hw2=homework-2
gen SMIQverbz0=SMIQverbz-0 // Reminder to center all L2 predictors
gen SMIQperfz0=SMIQperfz-0 
label variable hw2 "School Homework (0=2)"
// Create group-MC student-level variables using original L2 means
gen WSlangpost=langpost-SMlangpost
gen WSIQverbz=IQverbz-SMIQverbz
gen WSIQperfz=IQperfz-SMIQperfz
label variable WSlangpost "Student Language (0=School Mean)"
label variable WSIQverbz  "Student Verbal IQ Z (0=School Mean)"
label variable WSIQperfz  "Student Performance IQ Z (0=School Mean)"


////////////////////////////////////////////////////////////////////////////////
////////     BEGIN ANALYSES FOR EXAMPLE 3A: FIXED EFFECTS ONLY           ///////
////////////////////////////////////////////////////////////////////////////////

// Save results to separate file
log using $filesave\STATA_Example3a_Output.log, replace name(STATA_Example3a)

display as result "STATA School-Level Descriptives"
preserve  // Save for later use, then compute school-level dataset
collapse  Nperschool SMlangpost SMIQverbz SMIQperfz homework denomina mixedgra, ///
          by(schoolID)
tabulate  denomina 
tabulate  mixedgra
format    SMlangpost SMIQverbz SMIQperfz homework Nperschool %4.2f
summarize SMlangpost SMIQverbz SMIQperfz homework Nperschool, format
restore   // Go back to student-level dataset

display as result "STATA Student-Level Descriptives"
format    langpost WSlangpost IQverbz WSIQverbz IQperfz WSIQperfz %4.2f
summarize langpost WSlangpost IQverbz WSIQverbz IQperfz WSIQperfz, format

display as result "STATA Model 1a for Language: Empty Means, No Random Intercept"
mixed langpost , /// 
      variance reml dfmethod(residual)
       
display as result "STATA Model 1b for Language: Empty Means, Random Intercept"
mixed langpost , /// 
         || schoolID: , variance reml covariance(unstructured) dfmethod(satterthwaite),
       estat icc  // Compute Intraclass Correlation
       
display as result "STATA Model 2a: Add 3 School Predictors"
mixed langpost c.hw2 c.mixedgra c.den1v2 c.den1v3, /// 
         || schoolID: , variance reml covariance(unstructured) dfmethod(satterthwaite),
       test (c.mixedgra=0) (c.den1v2=0) (c.den1v3=0), small // Test Change in R2 after HW
       test                (c.den1v2=0) (c.den1v3=0), small // Test Omnibus Denomination
       lincom c.den1v2*-1 + c.den1v3*1, small // Denomination 2 vs 3
       predict PredHMD, xb         
       corr langpost PredHMD  
       display as result r(rho)^2 // total R2
       
display as result "STATA Model 2b: Add Interaction of Mixed Grade by Denomination"
mixed langpost c.hw2 c.mixedgra c.den1v2 c.den1v3 c.mixedgra#c.den1v2 c.mixedgra#c.den1v3, /// 
         || schoolID: , variance reml covariance(unstructured) dfmethod(satterthwaite),
       test (c.mixedgra#c.den1v2=0) (c.mixedgra#c.den1v3=0), small // Test Omnibus Interaction
       // Simple effects of mixed grade per denomination
       lincom c.mixedgra*1 + c.mixedgra#c.den1v2*0  + c.mixedgra#c.den1v3*0, small // MG:Den=1
       lincom c.mixedgra*1 + c.mixedgra#c.den1v2*1  + c.mixedgra#c.den1v3*0, small // MG:Den=2
       lincom c.mixedgra*1 + c.mixedgra#c.den1v2*0  + c.mixedgra#c.den1v3*1, small // MG:Den=3
       // Simple effects of denomination per mixed grade
       lincom c.den1v2*1  + c.den1v3*0 + c.mixedgra#c.den1v2*0  + c.mixedgra#c.den1v3*0, small // D1v2: MG=0
       lincom c.den1v2*0  + c.den1v3*1 + c.mixedgra#c.den1v2*0  + c.mixedgra#c.den1v3*0, small // D1v3: MG=0
       lincom c.den1v2*-1 + c.den1v3*1 + c.mixedgra#c.den1v2*0  + c.mixedgra#c.den1v3*0, small // D2v3: MG=0
       lincom c.den1v2*1  + c.den1v3*0 + c.mixedgra#c.den1v2*1  + c.mixedgra#c.den1v3*0, small // D1v2: MG=1
       lincom c.den1v2*0  + c.den1v3*1 + c.mixedgra#c.den1v2*0  + c.mixedgra#c.den1v3*1, small // D1v3: MG=1
       lincom c.den1v2*-1 + c.den1v3*1 + c.mixedgra#c.den1v2*-1 + c.mixedgra#c.den1v3*1, small // D2v3: MG=1
       // Differences in simple effects -- interaction contrasts
       lincom c.mixedgra#c.den1v2*1  + c.mixedgra#c.den1v3*0, small // MG:Den=1v2
       lincom c.mixedgra#c.den1v2*0  + c.mixedgra#c.den1v3*1, small // MG:Den=1v3
       lincom c.mixedgra#c.den1v2*-1 + c.mixedgra#c.den1v3*1, small // MG:Den=2v3
       predict PredMGxD, xb        
       corr langpost PredMGxD  
       display as result r(rho)^2 // total R2
       
display as result "STATA Model 1b for IQverb: Empty Means, Random Intercept"
mixed IQverbz , /// 
         || schoolID: , variance reml covariance(unstructured) dfmethod(satterthwaite),
       estat icc  // Compute Intraclass Correlation     
       
display as result "STATA Model 1b for IQperf: Empty Means, Random Intercept"
mixed IQperfz , /// 
         || schoolID: , variance reml covariance(unstructured) dfmethod(satterthwaite),
       estat icc  // Compute Intraclass Correlation     
      
display as result "STATA Model 3a: Remove Mixed Grade by Denom, Add L2 and L1 Student Verbal IQ"
mixed langpost c.hw2 c.mixedgra c.den1v2 c.den1v3 c.SMIQverbz0 c.WSIQverbz, /// 
         || schoolID: , variance reml covariance(unstructured) dfmethod(satterthwaite),
       test (c.SMIQverbz0=0) (c.WSIQverbz=0), small  // Test Omnibus Verbal IQ
       lincom c.den1v2*-1 + c.den1v3*1, small        // Denomination 2 vs 3
       lincom c.SMIQverbz0*1 + c.WSIQverbz*-1, small // L1 Contextual Effect of Verbal IQ
       predict PredIQverb, xb      
       corr langpost PredIQverb  
       display as result r(rho)^2 // total R2
       
display as result "STATA Model 3b: Add L2 and L1 Student Performance IQ"
mixed langpost c.hw2 c.mixedgra c.den1v2 c.den1v3 c.SMIQverbz0 c.WSIQverbz c.SMIQperfz0 c.WSIQperfz, /// 
         || schoolID: , variance reml covariance(unstructured) dfmethod(satterthwaite),
       test (c.SMIQverbz0=0) (c.WSIQverbz=0), small   // Test Omnibus Verbal IQ
       test (c.SMIQperfz0=0) (c.WSIQperfz=0), small   // Test Omnibus Performance IQ
       lincom c.den1v2*-1 + c.den1v3*1, small         // Denomination 2 vs 3
       lincom c.SMIQverbz0*1  + c.WSIQverbz*-1, small // L1 Contextual Effect of Verbal IQ
       lincom c.SMIQperfz0*1  + c.WSIQperfz*-1, small // L1 Contextual Effect of Performance IQ
       lincom c.SMIQverbz0*-1 + c.SMIQperfz0*1, small // L2 Verbal vs Performance IQ Effects
       lincom c.WSIQverbz*-1  + c.WSIQperf*1,  small  // L1 Verbal vs Performance IQ Effects
       predict PredIQperf, xb      
       corr langpost PredIQperf  
       display as result r(rho)^2 // total R2
       
display as result "STATA Model 3c: Add L2 and L1 IQ Interactions"
mixed langpost c.hw2 c.mixedgra c.den1v2 c.den1v3 c.SMIQverbz0 c.WSIQverbz c.SMIQperfz0 c.WSIQperfz /// 
               c.SMIQverbz0#c.SMIQperfz0 c.WSIQverbz#c.WSIQperfz, ///
         || schoolID: , variance reml covariance(unstructured) dfmethod(satterthwaite),
       test (c.SMIQverbz0#c.SMIQperfz0) (c.WSIQverbz#c.WSIQperfz), small   // Test Omnibus Interactions
       lincom c.den1v2*-1 + c.den1v3*1, small  // Denomination 2 vs 3
	   // Get simple slopes of L2 IQ perf at representative values of L2 IQ verb
       lincom c.SMIQperfz0*1 + c.SMIQperfz0#c.SMIQverbz0*-1, small // L2 IQ perf effect at L2 IQ verbz = -1
       lincom c.SMIQperfz0*1 + c.SMIQperfz0#c.SMIQverbz0*0,  small // L2 IQ perf effect at L2 IQ verbz =  0
       lincom c.SMIQperfz0*1 + c.SMIQperfz0#c.SMIQverbz0*1,  small // L2 IQ perf effect at L2 IQ verbz =  1
       // Get simple slopes of L1 IQ perf at representative values of L1 IQ verb
       lincom c.WSIQperfz*1 + c.WSIQperfz#c.WSIQverbz*-1, small // L1 IQ perf effect at L1 IQ verb z = -1
       lincom c.WSIQperfz*1 + c.WSIQperfz#c.WSIQverbz*0,  small // L1 IQ perf effect at L1 IQ verb z =  0
       lincom c.WSIQperfz*1 + c.WSIQperfz#c.WSIQverbz*1,  small // L1 IQ perf effect at L1 IQ verb z =  1
       estat vce  // Aymptotic covariance matrix of fixed effects for regions
       predict PredIQxIQ, xb       
       corr langpost PredIQxIQ  
       display as result r(rho)^2 // total R2

// Close log
log close STATA_Example3a  

       

       












