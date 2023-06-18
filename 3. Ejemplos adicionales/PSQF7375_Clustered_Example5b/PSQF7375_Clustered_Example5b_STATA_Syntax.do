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
global filesave "C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example5b"
 
// Import example stata data file 
use "$filesave\skincancer_v11.dta", clear

// Save results to separate file
log using $filesave\PSQF7375_Clustered_Example5b_STATA_Output.log, replace name(STATA_Example5b)

// Label existing variables
label variable region  "region: Region Nesting Variable"
label variable deaths  "deaths: Count of Deaths"
label variable uv      "uv: Amount of UV Exposure"

// Select cases complete for analysis variables
egen nmiss=rowmiss(region deaths uv nation)
drop if nmiss>0
drop if nation==8 // remove Luxemburg because N=3

// Get region means of variables and label them
egen RMdeaths = mean(deaths), by(region)
egen RMuv     = mean(uv),     by(region)
label variable RMdeaths "RMdeaths: Region Mean of Death Count"
label variable RMuv     "RMuv: Region Mean of UV Exposure"

// Get count per region and label it
egen Nperregion = count(deaths), by(region)
label variable Nperregion "Nperregion: Count per Region"

// Center region mean uv (uncentered, but remember to center it)
gen RMuv0 = RMuv
label variable RMuv0 "RMuv0: Region Mean of UV Exposure (0=0)"

// Center to get within-region deaths and UV
gen WRdeaths = deaths - RMdeaths
gen WRuv = uv - RMuv
label variable WRdeaths "WRdeaths: Within-Region Deaths (0=RM)"
label variable WRuv "WRuv: Within-Region UV Exposure (0=RM)"

display as result "STATA Region-Level Descriptives"
preserve  // Save for later use, then compute region-level dataset
collapse  Nperregion RMdeaths RMuv, by(region)
format    Nperregion RMdeaths RMuv  %4.2f
summarize Nperregion RMdeaths RMuv, format
histogram RMdeaths, percent discrete width(10) start(0)
histogram RMuv, percent discrete width(1) start(-8)
restore   // Go back to county-level dataset

display as result "STATA County-Level Descriptives"
format    deaths WRdeaths uv WRuv %4.2f
summarize deaths WRdeaths uv WRuv, format
tabulate  nation // Asking for it here to preserve value labels
histogram deaths, percent discrete width(20) start(0)
histogram WRdeaths, percent discrete width(20) start(-90)
histogram uv, percent discrete width(1) start(-9)
histogram WRuv, percent discrete width(0.5) start(-2.50)


////////////////////////////////////////////////////////////////////////////////
////////                 BEGIN ANALYSES FOR EXAMPLE 5B                   ///////
////////////////////////////////////////////////////////////////////////////////

display as result "STATA Empty Means, Random Intercept Model for UV Exposure (predictor)"
mixed uv , /// 
         || region: , variance reml covariance(unstructured) dfmethod(satterthwaite),
      estat ic, n(77), // get AIC and BIC equivalent to SAS
      estat icc        // compute Intraclass Correlation

display as result "STATA Model 1a: Empty Means, Single-Level Model for Deaths (outcome)"
display as result "Log Link, Poisson Conditional Distribution"
mepoisson deaths , 
          estat ic, n(77),     // get AIC and BIC equivalent to SAS
          nlcom exp(_b[_cons]) // fixed intercept in counts
        
display as result "STATA Model 1b: Empty Means, Single-Level Model for Deaths (outcome)"
display as result "Log Link, Negative Binomial Conditional Distribution"
menbreg deaths , 
        estat ic, n(77),     // get AIC and BIC equivalent to SAS
        nlcom exp(_b[_cons]) // fixed intercept in counts
             
display as result "STATA Model 2a: Empty Means, Random Intercept Model for Deaths (outcome)"
display as result "Log Link, Poisson Conditional Distribution"
mepoisson deaths , || region: , covariance(unstructured) intpoints(15),
          estat ic, n(77),          // get AIC and BIC equivalent to SAS
          nlcom exp(_b[_cons])      // fixed intercept in counts
          estimates store Poisson2  // Save for LRT
             
display as result "STATA Model 2b: Empty Means, Random Intercept Model for Deaths (outcome)"
display as result "Log Link, Negative Binomial Conditional Distribution"
menbreg deaths , || region: , covariance(unstructured) intpoints(15),
        estat ic, n(77),         // get AIC and BIC equivalent to SAS
        nlcom exp(_b[_cons])     // fixed intercept in counts
        estimates store NegBin2  // save LL for LRT
        lrtest NegBin2 Poisson2  // LRT against fixed effect model -- not working yet

display as result "STATA Model 3a: Add Fixed Slope of Between-Region Mean UV Predictor"
menbreg deaths c.RMuv0, || region: , covariance(unstructured) intpoints(15),
        estat ic, n(77), 
        margins , at(c.RMuv0=(-1(1)1)) predict(xb) // predicted log counts
        margins , at(c.RMuv0=(-1(1)1))             // marginal predicted counts

display as result "STATA Model 3b: Add Fixed Slope of Within-Region UV Predictor"
menbreg deaths c.RMuv0 c.WRuv, || region: , covariance(unstructured) intpoints(15),
        estat ic, n(77), 
        lincom c.WRuv*-1 + c.RMuv0*1              // contextual UV effect
        margins , at(c.WRuv=(-1(1)1)) predict(xb) // predicted log counts
        margins , at(c.WRuv=(-1(1)1))             // marginal predicted counts                
        estimates store Fixed                     // save LL for LRT     
          
display as result "STATA Model 3c: Add Random Slope of Within-Region UV Predictor"
menbreg deaths c.RMuv0 c.WRuv, || region: c.WRuv, covariance(unstructured) intpoints(15),
        estat ic, n(77), 
        lincom c.WRuv*-1 + c.RMuv0*1 // contextual UV effect
        estimates store Random       // save LL for LRT
        lrtest Random Fixed          // LRT against fixed effect model          
          
display as result "STATA Model 4a: Nation as Control Predictor"
display as result "Main Effect of Nation Only"
menbreg deaths c.RMuv0 c.WRuv i.nation, ///
           || region: c.WRuv, covariance(unstructured) intpoints(15),
        estat ic, n(77), 
        contrast i.nation  // Omnibus test of df=7 nation on intercept 
                     
display as result "STATA Model 4b: Nation as Control Predictor"
display as result "Add Nation Interactions with Between-Region UV and Within-Region UV"
menbreg deaths c.RMuv0 c.WRuv i.nation i.nation i.nation#c.RMuv0 i.nation#c.WRuv, ///
           || region: c.WRuv, covariance(unstructured) intpoints(15),
        estat ic, n(77), 
        contrast i.nation          // Omnibus test of df=7 nation on intercept 
        contrast i.nation#c.RMuv0  // Omnibus test of df=7 nation on RMuv0 slope
        lincom  c.RMuv0*1 + i1.nation#c.RMuv0*1 // RMuv0 fixed slope per nation
        lincom  c.RMuv0*1 + i2.nation#c.RMuv0*1
        lincom  c.RMuv0*1 + i3.nation#c.RMuv0*1
        lincom  c.RMuv0*1 + i4.nation#c.RMuv0*1
        lincom  c.RMuv0*1 + i5.nation#c.RMuv0*1
        lincom  c.RMuv0*1 + i6.nation#c.RMuv0*1
        lincom  c.RMuv0*1 + i7.nation#c.RMuv0*1
        lincom  c.RMuv0*1 + i9.nation#c.RMuv0*1
        contrast i.nation#c.WRuv  // Omnibus test of df=7 nation on WRuv slope
        lincom  c.WRuv*1 + i1.nation#c.WRuv*1 // WRuv fixed slope per nation
        lincom  c.WRuv*1 + i2.nation#c.WRuv*1
        lincom  c.WRuv*1 + i3.nation#c.WRuv*1
        lincom  c.WRuv*1 + i4.nation#c.WRuv*1
        lincom  c.WRuv*1 + i5.nation#c.WRuv*1
        lincom  c.WRuv*1 + i6.nation#c.WRuv*1
        lincom  c.WRuv*1 + i7.nation#c.WRuv*1
        lincom  c.WRuv*1 + i9.nation#c.WRuv*1

          
// Close log
log close STATA_Example5b
