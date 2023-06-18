* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
  OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Open datasets even if the formats are not available;
  OPTIONS NOfmterr; 
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
  TITLE; ODS TRACE OFF; 
* Turn on old-school output and new HTML graphics;
  ODS LISTING; ODS GRAPHICS ON;

***********************************************************************************;
*******     MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR THIS EXAMPLE        *******;
*******               NOTHING IN HERE NEEDS TO BE CHANGED                   *******;
***********************************************************************************;

* To use PseudoR2G macro;
* Ncov =     TOTAL # entries in covariance parameters output table;
* CovFewer = name of covparms table for nested model;
* CovMore =  name of covparms table for comparison modell;
%MACRO PseudoR2G(NCov, CovFewer, CovMore);
DATA &CovFewer.; LENGTH Name $30.; SET &CovFewer.; Name="&CovFewer."; RUN;
DATA &CovMore.;  LENGTH Name $30.; SET &CovMore.;  Name="&CovMore.";  RUN;
DATA CovCompare; LENGTH Name $30.; SET &CovFewer. &CovMore.; RUN;
DATA CovCompare; SET CovCompare; 
     PseudoR2=(LAG&Ncov.(Estimate)-Estimate)/LAG&Ncov.(Estimate); RUN;
DATA CovCompare; SET CovCompare; 
     IF CovParm IN("UN(2,1)","UN(3,1)","UN(4,1)","UN(3,2)","UN(4,2)","UN(4,3)") 
     THEN DELETE; RUN;
TITLE2 "PsuedoR2 (% Reduction) for &CovFewer. vs. &CovMore.";
PROC PRINT NOOBS DATA=CovCompare; RUN; TITLE2;
%MEND PseudoR2G;

***********************************************************************************;
*******                     BEGIN IMPORT FOR EXAMPLE DATA                   *******;
*******                  CHANGE "filesave" to your directory                *******;
***********************************************************************************;

* Define global variable for file location to be replaced in code below;
* \\Client\ precedes actual path when using UIowa Virtual Desktop;
%LET filesave=C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example5b;
LIBNAME example "&filesave.";

* Import GSS_subsample STATA data file into SAS;
PROC IMPORT DATAFILE="&filesave.\skincancer_v11.dta" 
            OUT=work.skincancer DBMS=DTA REPLACE; 
RUN;

* Open output directory to save results to;
* STARTPAGE=NO means dont start each output on its own page;
ODS RTF FILE="&filesave.\PSQF7375_Clustered_Example5b_SAS_Output.rtf" STYLE=HTMLBlue STARTPAGE=NO;

* Label existing variables;
DATA work.skincancer; SET work.skincancer; 
     LABEL region= "region: Region Nesting Variable"
           deaths= "deaths: Count of Deaths"
           uv=     "uv: Amount of UV Exposure";
     * Select cases that are complete for analysis variables;
     IF NMISS(region,deaths,uv,nation)>0 THEN DELETE; 
     * Remove Luxemberg (N=3);
     IF nation=8 THEN DELETE;
RUN;

* Get region means;
PROC SORT DATA=work.skincancer; BY region; RUN;
PROC MEANS NOPRINT N DATA=work.skincancer; 
     BY region; VAR deaths uv;
     OUTPUT OUT=work.RegionMeans MEAN(deaths uv)= RMdeaths RMuv; RUN;

* Label new region mean variables;
DATA work.RegionMeans; SET work.RegionMeans;
     Nperregion = _FREQ_; * Saving N per region;
     DROP _TYPE_ _FREQ_;  * Dropping unneeded SAS-created variables;
     LABEL Nperregion= "Nperregion: # Students Contributing Data"
           RMdeaths=   "RMdeaths: Region Mean of Death Count"
           RMuv=       "RMuv: Region Mean of UV Exposure";
RUN;

* Merge region means back with individual data;
DATA work.skincancer; MERGE work.skincancer work.RegionMeans; BY region;
     * Center region mean uv (uncentered, but remember to center it);
     RMuv0 = RMuv - 0; 
     LABEL RMuv0= "RMuv0: Region Mean of UV Exposure (0=0)"; 
     * Center to get within-region deaths and UV;
     WRdeaths = deaths - RMdeaths;
     WRuv = uv - RMuv;
     LABEL WRdeaths= "WRdeaths: Within-Region Deaths (0=RM)"
           WRuv= "WRuv: Within-region UV Exposure (0=RM)";
RUN;

TITLE "Region-Level Descriptives";
PROC MEANS NDEC=2 DATA=work.RegionMeans; 
     VAR Nperregion RMdeaths RMuv; RUN; 
PROC UNIVARIATE NOPRINT DATA=work.RegionMeans;
     VAR RMdeaths RMuv;
     HISTOGRAM RMdeaths / MIDPOINTS=0 TO 160 BY 10;
     HISTOGRAM RMuv / MIDPOINTS = -8 TO 13 BY 1;
RUN; QUIT; TITLE;

TITLE "County-Level Descriptives";
PROC MEANS NDEC=2 DATA=work.skincancer; 
     VAR deaths WRdeaths uv WRuv; RUN; 
PROC UNIVARIATE NOPRINT DATA=work.skincancer;
     VAR deaths WRdeaths uv WRuv;
     HISTOGRAM deaths / MIDPOINTS=0 TO 320 BY 20;
     HISTOGRAM WRdeaths / MIDPOINTS=-90 TO 360 BY 20;
     HISTOGRAM uv / MIDPOINTS = -9 TO 14 BY 1;
     HISTOGRAM WRuv / MIDPOINTS = -2.50 TO 2.00 BY 0.50;
RUN; QUIT; TITLE;
PROC FREQ DATA=work.skincancer;
     TABLE nation; RUN; TITLE;

* Removing value labels from nation to get numeric order back;
DATA work.skincancer; SET work.skincancer;
     FORMAT nation; RUN;


***********************************************************************************;
*******                    BEGIN ANALYSES FOR EXAMPLE 5B                    *******;
***********************************************************************************;

TITLE "SAS Empty Means, Random Intercept Model for UV Exposure (predictor)";
PROC MIXED DATA=work.skincancer NOCLPRINT COVTEST NAMELEN=100 IC METHOD=REML;
     CLASS region; 
     MODEL uv = / SOLUTION DDFM=Satterthwaite CHISQ; 
     RANDOM INTERCEPT / VCORR=2 TYPE=UN SUBJECT=region; * VCORR gives ICC;
RUN; TITLE;

TITLE1 "SAS Model 1a: Empty Means, Single-Level Model for Deaths (outcome)";
TITLE2 "Log Link, Poisson Conditional Distribution";
PROC GLIMMIX DATA=work.skincancer NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS region;
     MODEL deaths =  / SOLUTION LINK=LOG DIST=POISSON DDFM=Satterthwaite CHISQ;
     ESTIMATE "Intercept" intercept 1 / ILINK; * ILINK is inverse link (to un-log);
RUN; TITLE1; TITLE2;

TITLE1 "SAS Model 1b: Empty Means, Single-Level Model for Deaths (outcome)";
TITLE2 "Log Link, Negative Binomial Conditional Distribution";
PROC GLIMMIX DATA=work.skincancer NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS region;
     MODEL deaths =  / SOLUTION LINK=LOG DIST=NEGBIN DDFM=Satterthwaite CHISQ;
     ESTIMATE "Intercept" intercept 1 / ILINK; * ILINK is inverse link (to un-log);
RUN; TITLE1; TITLE2;

TITLE1 "SAS Model 2a: Empty Means, Random Intercept Model for Deaths (outcome)";
TITLE2 "Log Link, Poisson Conditional Distribution";
PROC GLIMMIX DATA=work.skincancer NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS region;
     MODEL deaths =  / SOLUTION LINK=LOG DIST=Poisson DDFM=BW CHISQ;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=region;
     ESTIMATE "Intercept" intercept 1 / ILINK; * ILINK is inverse link (to un-log);
     COVTEST "Random Region Intercept?" 0;     * Test if G matrix (1,1)=0;
RUN; TITLE1; TITLE2;

TITLE1 "SAS Model 2b: Empty Means, Random Intercept Model for Deaths (outcome)";
TITLE2 "Log Link, Negative Binomial Conditional Distribution";
PROC GLIMMIX DATA=work.skincancer NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS region;
     MODEL deaths =  / SOLUTION LINK=LOG DIST=NEGBIN DDFM=BW CHISQ;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=region;
     ESTIMATE "Intercept" intercept 1 / ILINK; * ILINK is inverse link (to un-log);
     COVTEST "Random Region Intercept?" 0 .;   * Test if G matrix (1,1)=0;
     COVTEST "Overdispersion?" . 1;            * Test if overdispersion=0 (1=Poisson);
     ODS OUTPUT CovParms=CovEmpty;             * Save random int var for pseudo-R2;
RUN; TITLE1; TITLE2;

TITLE1 "SAS Model 3a: Add Fixed Slope of Between-Region Mean UV Predictor";
PROC GLIMMIX DATA=work.skincancer NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS region;
     MODEL deaths =  RMuv0 / SOLUTION LINK=LOG DIST=NEGBIN DDFM=BW CHISQ;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=region;
     ESTIMATE "Intercept if RMuv0=-1"  intercept 1 RMuv0 -1 / ILINK; 
     ESTIMATE "Intercept if RMuv0= 0"  intercept 1 RMuv0  0 / ILINK; 
     ESTIMATE "Intercept if RMuv0= 1"  intercept 1 RMuv0  1 / ILINK; 
     ODS OUTPUT CovParms=CovBRuv;              * Save random int var for pseudo-R2;
RUN; TITLE1; 

* Calculate PseudoR2 relative to previous model 2b;
%PseudoR2G(NCov=2, CovFewer=CovEmpty, CovMore=CovBRuv);

TITLE1 "SAS Model 3b: Add Fixed Slope of Within-Region UV Predictor";
PROC GLIMMIX DATA=work.skincancer NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS region;
     MODEL deaths =  RMuv0 WRuv / SOLUTION LINK=LOG DIST=NEGBIN DDFM=BW CHISQ;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=region;
     ESTIMATE "Contextual UV Effect"  WRuv -1  RMuv0 1;
     CONTRAST "Multivariate Wald test for Math Effects"  RMuv0 1, WRuv 1 / CHISQ;
     ESTIMATE "Intercept if WRuv=-1"  intercept 1 WRuv -1 / ILINK; 
     ESTIMATE "Intercept if WRuv= 0"  intercept 1 WRuv  0 / ILINK; 
     ESTIMATE "Intercept if WRuv= 1"  intercept 1 WRuv  1 / ILINK; 
     ODS OUTPUT CovParms=CovWRuv;              * Save random int var for pseudo-R2;
RUN; TITLE1; 

* Calculate PseudoR2 relative to previous model 3a;
%PseudoR2G(NCov=2, CovFewer=CovBRuv, CovMore=CovWRuv);

TITLE1 "SAS Model 3c: Add Random Slope of Within-Region UV Predictor";
PROC GLIMMIX DATA=work.skincancer NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS region;
     MODEL deaths =  RMuv0 WRuv / SOLUTION LINK=LOG DIST=NEGBIN DDFM=BW CHISQ;
     RANDOM INTERCEPT WRuv / TYPE=UN SUBJECT=region;
     ESTIMATE "Contextual UV Effect"  WRuv -1  RMuv0 1;
     COVTEST "Random WRuv Slope?" . 0 0 .; * Leave (1,1) and OD, test if (2,1) and (2,2) =0;
     ODS OUTPUT CovParms=CovRandWRuv;      * Save random variances for pseudo-R2;
RUN; TITLE1; 

TITLE1 "SAS Model 4a: Nation as Control Predictor";
TITLE2 "Main Effect of Nation Only";
PROC GLIMMIX DATA=work.skincancer NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS region nation;
     MODEL deaths =  RMuv0 WRuv nation / SOLUTION LINK=LOG DIST=NEGBIN DDFM=BW CHISQ;
     RANDOM INTERCEPT WRuv / TYPE=UN SUBJECT=region;
     ODS OUTPUT CovParms=CovNatMain;   * Save random variances for pseudo-R2;
RUN; TITLE1; 

* Calculate PseudoR2 relative to previous model 3c;
%PseudoR2G(NCov=4, CovFewer=CovRandWRuv, CovMore=CovNatMain);

TITLE1 "SAS Model 4b: Nation as Control Predictor";
TITLE2 "Add Nation Interactions with Between-Region UV and Within-Region UV";
PROC GLIMMIX DATA=work.skincancer NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS region nation;
     MODEL deaths =  RMuv0 WRuv nation nation*RMuv0 nation*WRuv 
                     / SOLUTION LINK=LOG DIST=NEGBIN DDFM=BW CHISQ;
     RANDOM INTERCEPT WRuv / TYPE=UN SUBJECT=region;
     ODS OUTPUT CovParms=CovNatUV;   * Save random variances for pseudo-R2;
     ESTIMATE "Between-Region UV For Nation 1" RMuv0 1 RMuv0*nation 1 0 0 0 0 0 0 0; 
     ESTIMATE "Between-Region UV For Nation 2" RMuv0 1 RMuv0*nation 0 1 0 0 0 0 0 0; 
     ESTIMATE "Between-Region UV For Nation 3" RMuv0 1 RMuv0*nation 0 0 1 0 0 0 0 0; 
     ESTIMATE "Between-Region UV For Nation 4" RMuv0 1 RMuv0*nation 0 0 0 1 0 0 0 0; 
     ESTIMATE "Between-Region UV For Nation 5" RMuv0 1 RMuv0*nation 0 0 0 0 1 0 0 0; 
     ESTIMATE "Between-Region UV For Nation 6" RMuv0 1 RMuv0*nation 0 0 0 0 0 1 0 0; 
     ESTIMATE "Between-Region UV For Nation 7" RMuv0 1 RMuv0*nation 0 0 0 0 0 0 1 0; 
     ESTIMATE "Between-Region UV For Nation 9" RMuv0 1 RMuv0*nation 0 0 0 0 0 0 0 1; 
     ESTIMATE "Within-Region UV For Nation 1" WRuv 1  WRuv*nation 1 0 0 0 0 0 0 0; 
     ESTIMATE "Within-Region UV For Nation 2" WRuv 1  WRuv*nation 0 1 0 0 0 0 0 0; 
     ESTIMATE "Within-Region UV For Nation 3" WRuv 1  WRuv*nation 0 0 1 0 0 0 0 0; 
     ESTIMATE "Within-Region UV For Nation 4" WRuv 1  WRuv*nation 0 0 0 1 0 0 0 0; 
     ESTIMATE "Within-Region UV For Nation 5" WRuv 1  WRuv*nation 0 0 0 0 1 0 0 0; 
     ESTIMATE "Within-Region UV For Nation 6" WRuv 1  WRuv*nation 0 0 0 0 0 1 0 0; 
     ESTIMATE "Within-Region UV For Nation 7" WRuv 1  WRuv*nation 0 0 0 0 0 0 1 0; 
     ESTIMATE "Within-Region UV For Nation 9" WRuv 1  WRuv*nation 0 0 0 0 0 0 0 1; 
RUN; TITLE1; 


* Calculate PseudoR2 relative to previous model 4a;
%PseudoR2G(NCov=4, CovFewer=CovNatMain, CovMore=CovNatUV);

* Calculate PseudoR2 for total nation effect;
%PseudoR2G(NCov=4, CovFewer=CovRandWRuv, CovMore=CovNatUV);

* Close output;
ODS RTF CLOSE;


