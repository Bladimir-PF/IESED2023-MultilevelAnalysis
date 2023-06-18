* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
  OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Open datasets even if the formats are not available;
  OPTIONS NOfmterr; 
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
  TITLE; ODS TRACE OFF; 
* Turn on old-school output in addition to HTML output;
  ODS LISTING; 

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
%LET filesave=C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example3b;
LIBNAME example "&filesave.";
     
* Save SAS syntax and output to permanent text file;
PROC PRINTTO PRINT="&filesave.\PSQF7375_Clustered_Example5a_SAS_Output.txt" 
               LOG="&filesave.\PSQF7375_Clustered_Example5a_SAS_Output.txt"; RUN;

* Import data into work library;
DATA work.grade10; SET example.grade10school; 
     LABEL studentID= "studentID: Student ID number"
           schoolID=  "schoolID: School ID number"
           frlunch=   "frlunch: 0=No, 1=Free/Reduced Lunch"
           math=      "math: Math Test Score Outcome";
     * Selecting cases that are complete for analysis variables;
     IF NMISS(studentID, schoolID, frlunch, math)>0 THEN DELETE; RUN;

* Get school means;
PROC SORT DATA=work.grade10; BY schoolID studentID; RUN;
PROC MEANS NOPRINT N DATA=work.grade10; 
     BY schoolID; VAR frlunch math;
     OUTPUT OUT=work.SchoolMeans MEAN(frlunch math)= SMfrlunch SMmath; RUN;

* Label new school mean variables;
DATA work.SchoolMeans; SET work.SchoolMeans;
     Nperschool = _FREQ_; * Saving N per school;
     DROP _TYPE_ _FREQ_;  * Dropping unneeded SAS-created variables;
     LABEL Nperschool= "Nperschool: # Students Contributing Data"
           SMfrlunch=  "SMfrlunch: School Mean 0=No, 1=Free/Reduced Lunch"
           SMmath=     "SMmath: School Mean Math Outcome";
     * Arbitrarily select only schools with at least 30 students;
     IF Nperschool < 31 THEN DELETE; 
RUN;

* Merge school means back with individual data;
DATA work.grade10; MERGE work.grade10 work.SchoolMeans; BY schoolID;
     * Arbitrarily select only schools with at least 30 students;
     IF Nperschool < 31 THEN DELETE; 
     * Center math predictors;
     WSmath = (math - SMmath)/10; 
     LABEL WSmath= "WSmath: Within-School Math (0=SM)";
     SMmath50 = (SMmath - 50)/10; 
     LABEL SMmath50= "SMmath50: School Mean Math (0=5)"; RUN;

TITLE "School-Level Descriptives";
PROC MEANS NDEC=2 DATA=work.SchoolMeans; 
     VAR Nperschool SMmath SMfrlunch; RUN; TITLE;

TITLE "Student-Level Descriptives";
PROC MEANS NDEC=2 DATA=work.grade10; 
     VAR math frlunch; RUN; TITLE;


***********************************************************************************;
*******                    BEGIN ANALYSES FOR EXAMPLE 5A                    *******;
***********************************************************************************;

TITLE "SAS Model 1: Empty Means, Single-Level Logistic Model for Student Free/Reduced Lunch";
PROC GLIMMIX DATA=work.grade10 NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS schoolID;
     * Descending makes us predict the 1 instead of the 0;
     MODEL frlunch (DESCENDING) =  / SOLUTION LINK=LOGIT DIST=BINARY DDFM=Satterthwaite;
     ESTIMATE "Intercept" intercept 1 / ILINK; * ILINK is inverse link (to un-logit);
RUN; TITLE;

TITLE "SAS Model 2: Empty Means, Two-Level Logistic Model for Student Free/Reduced Lunch";
PROC GLIMMIX DATA=work.grade10 NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS schoolID;
     MODEL frlunch (DESCENDING) =  / SOLUTION LINK=LOGIT DIST=BINARY DDFM=BW;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=schoolID;
     ESTIMATE "Intercept" intercept 1 / ILINK; * ILINK is inverse link (to un-logit);
     COVTEST "Random School Intercept?" 0;     * Test if G matrix (1,1)=0;
     ODS OUTPUT CovParms=CovEmpty;             * Save random int var for pseudo-R2;
RUN; TITLE;

TITLE "SAS Model 3: Add Level-2 Fixed Effect of School Mean Math";
PROC GLIMMIX DATA=work.grade10 NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS schoolID;
     MODEL frlunch (DESCENDING) = SMmath50 / SOLUTION LINK=LOGIT DIST=BINARY DDFM=BW ODDSRATIO;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=schoolID;
     ESTIMATE "Intercept if SMmath=49"  intercept 1 SMmath50 -1 / ILINK; 
     ESTIMATE "Intercept if SMmath=50"  intercept 1 SMmath50  0 / ILINK; 
     ESTIMATE "Intercept if SMmath=51"  intercept 1 SMmath50  1 / ILINK; 
     ESTIMATE "L2 Math Slope"           SMmath50 1 / ILINK; * Example of non-sense ILINK;
     ODS OUTPUT CovParms=CovSMmath;     * Save random int var for pseudo-R2;
RUN; TITLE;
* Calculate PseudoR2 relative to previous model 2;
%PseudoR2G(NCov=1, CovFewer=CovEmpty, CovMore=CovSMmath);

TITLE "SAS Model 4: Add Level-1 Fixed Effect of Group-MC Student Math";
PROC GLIMMIX DATA=work.grade10 NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS schoolID;
     MODEL frlunch (DESCENDING) = SMmath50 WSmath / SOLUTION LINK=LOGIT DIST=BINARY DDFM=BW ODDSRATIO;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=schoolID; 
     ESTIMATE "Contextual Between-School Effect of Math"  WSmath -1  SMmath50 1;
     CONTRAST "Multivariate Wald test for Math Effects"  SMmath50 1, WSmath 1 / CHISQ;
RUN; TITLE;

TITLE "SAS Model 5: Add Random Effect of Group-MC Student Math";
PROC GLIMMIX DATA=work.grade10 NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS schoolID;
     MODEL frlunch (DESCENDING) = SMmath50 WSmath / SOLUTION LINK=LOGIT DIST=BINARY DDFM=BW ODDSRATIO;
     RANDOM INTERCEPT WSmath / TYPE=UN SUBJECT=schoolID; 
     COVTEST "Random Student Math Slope?" . 0 0; * Leave (1,1), test if (2,1) and (2,2) =0;
     ODS OUTPUT CovParms=CovRandMath;   * Save random variances for pseudo-R2;
RUN; TITLE;

TITLE "SAS Model 6: Add Intra-Variable Interactions of School Mean and Group-MC Student Math";
PROC GLIMMIX DATA=work.grade10 NOCLPRINT NAMELEN=100 METHOD=QUAD(QPOINTS=15) GRADIENT;
     CLASS schoolID;
     MODEL frlunch (DESCENDING) = SMmath50 WSmath SMmath50*WSmath SMmath50*SMmath50 
                       / SOLUTION LINK=LOGIT DIST=BINARY DDFM=BW ODDSRATIO;
     RANDOM INTERCEPT WSmath / TYPE=UN SUBJECT=schoolID;
     ESTIMATE "Contextual Math Main Effect" WSmath -1 SMmath50 1; 
     ESTIMATE "Contextual Math Interaction" SMmath50*WSmath -1 SMmath50*SMmath50 1;
     CONTRAST "Multivariate Wald test for Interactions"  SMmath50*WSmath 1, SMmath50*SMmath50 1 / CHISQ;
     ODS OUTPUT CovParms=CovInteract;   * Save random variances for pseudo-R2;
RUN; TITLE;
* Calculate PseudoR2 relative to previous model 5;
%PseudoR2G(NCov=3, CovFewer=CovRandMath, CovMore=CovInteract);


* Close output;
PROC PRINTTO; RUN;
