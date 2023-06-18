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
global filesave "C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example6"
 
// Import example stata data file 
use "$filesave\o38stack.dta", clear

// Save results to separate file
log using $filesave\PSQF7375_Clustered_Example6_STATA_Output.log, replace

// Keep items with item predictor variables
egen nmiss=rowmiss(cor rclut rrel rbrit legsign)
drop if nmiss>0
drop if use38==0


////////////////////////////////////////////////////////////////////////////////
////////                     BEGIN ANALYSES FOR EXAMPLE 6                ///////
////////////////////////////////////////////////////////////////////////////////


display as result "STATA Single-Level Empty Model"
melogit cor ,  intmethod(laplace),
nlcom 1/(1+exp(-1*(_b[_cons]))) // fixed intercept in probability

display as result "STATA Random Subjects, Empty Items Model"
melogit cor , || partid: , covariance(unstructured) intmethod(laplace),
nlcom 1/(1+exp(-1*(_b[_cons]))) // fixed intercept in probability

display as result "STATA Random Subjects, LLTM-Predicted Fixed Items Model"
melogit cor rclut rrel rbrit legsign, || partid: , covariance(unstructured) intmethod(laplace)

display as result "STATA Random Subjects, Rasch Saturated Fixed Items Model"
melogit cor i.picture, || partid: , covariance(unstructured) intmethod(laplace)
        margins i.picture, predict(xb)  // Get logit intercept per item
          
display as result "STATA Random Subjects, Random Items Empty Model"
melogit cor , || _all: R.picture || partid: , covariance(unstructured) intmethod(laplace)

display as result "STATA Random Subjects, LLTM + Random Items Model"
melogit cor rclut rrel rbrit legsign, || _all: R.picture || partid: , ///
        covariance(unstructured) intmethod(laplace)
     
     
// Would not converge    
*display as result "Random Subjects, Random Items Models for Correct Responses"
*display as result "LLTM Model (Easiness-Predicted Items)"
*melogit cor rclut rrel rbrit legsign,,  || _all: R.picture ||  partid: rbrit , ///
*        covariance(unstructured) intmethod(laplace)
        
// STATA models with random slopes are not estimatble correctly 
// because covariance(un) is not allowed


// Close log
log close  

