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
*******                     BEGIN IMPORT FOR EXAMPLE DATA                   *******;
*******                  CHANGE "filesave" to your directory                *******;
***********************************************************************************;

* Define global variable for file location to be replaced in code below;
* \\Client\ precedes actual path when using UIowa Virtual Desktop;
%LET filesave=C:\Dropbox\19_PSQF7375_Clustered\PSQF7375_Clustered_Example6;
LIBNAME example "&filesave.";

* Reading in data and keeping items with item predictor variables;
DATA work.o38stack; SET example.o38stack; 
     WHERE use38=1 AND NMISS(cor,rclut, rrel, rbrit, legsign)=0; RUN;
PROC SORT DATA=work.o38stack; BY PartID picture; RUN;  

* Open output directory to save results to;
* STARTPAGE=NO means dont start each output on its own page;
ODS RTF FILE="&filesave.\PSQF7375_Clustered_Example6_SAS_Output.rtf" STYLE=HTMLBlue STARTPAGE=NO;

TITLE "SAS Single-Level Empty Model";
PROC GLIMMIX DATA=work.o38stack NOCLPRINT NOITPRINT GRADIENT METHOD=Laplace; 
     CLASS PartID picture;
     MODEL cor (DESCENDING) =  / SOLUTION LINK=LOGIT DIST=BINARY;  * Binary response, logit link;
     ESTIMATE "Intercept" intercept 1 / ILINK; * Inverse: Logit to probability;
RUN; TITLE;

TITLE "SAS Random Subjects, Empty Items Model";
PROC GLIMMIX DATA=work.o38stack NOCLPRINT NOITPRINT GRADIENT METHOD=Laplace; 
     CLASS PartID picture;
     MODEL cor (DESCENDING) =  / SOLUTION LINK=LOGIT DIST=BINARY; 
     RANDOM INTERCEPT / SOLUTION TYPE=UN SUBJECT=PartID; * theta/U0 per subject;
     ESTIMATE "Intercept" intercept 1 / ILINK; * Inverse: Logit to probability;
     COVTEST "Need subject random int?" 0; 
RUN; TITLE;

TITLE "SAS Random Subjects, LLTM-Predicted Fixed Items Model";
PROC GLIMMIX DATA=work.o38stack NOCLPRINT NOITPRINT GRADIENT METHOD=Laplace; 
     CLASS PartID picture;
     MODEL cor (DESCENDING) = rclut rrel rbrit legsign / SOLUTION LINK=LOGIT DIST=BINARY; 
     RANDOM INTERCEPT / SOLUTION TYPE=UN SUBJECT=PartID;
RUN; TITLE;

TITLE "SAS Random Subjects, Rasch Fixed Items Model";
PROC GLIMMIX DATA=work.o38stack NOCLPRINT NOITPRINT GRADIENT METHOD=Laplace; 
     CLASS PartID picture;
     MODEL cor (DESCENDING) = picture / SOLUTION LINK=LOGIT DIST=BINARY; 
     RANDOM INTERCEPT / SOLUTION TYPE=UN SUBJECT=PartID;
     LSMEANS picture; * Get logit intercept per item;
     ODS OUTPUT LSMEANS=Rasch; * Save to dataset;
RUN; TITLE;

/*
PROC SORT DATA=work.o38stack; BY PartID picture; RUN;
PROC TRANSPOSE DATA=work.o38stack OUT=work.o38mult PREFIX=cor;
     BY PartID; ID Picture; VAR Cor; RUN;
PROC IRT DATA=work.o38mult LINK=LOGIT;
     MODEL Cor2--Cor179/RESFUNC=ONEP; RUN;
*/

TITLE "SAS Random Items, Empty Subjects Model";
PROC GLIMMIX DATA=work.o38stack NOCLPRINT NOITPRINT GRADIENT METHOD=Laplace; 
     CLASS PartID picture;
     MODEL cor (DESCENDING) =  / SOLUTION LINK=LOGIT DIST=BINARY; 
     RANDOM INTERCEPT / SOLUTION TYPE=UN SUBJECT=picture; * easiness per item;
     COVTEST "Need item random int?" 0; 
RUN; TITLE;


TITLE "SAS Random Subjects, Random Items Empty Model";
PROC GLIMMIX DATA=work.o38stack NOCLPRINT NOITPRINT GRADIENT METHOD=Laplace; 
     CLASS PartID picture;
     MODEL cor (DESCENDING) =  / SOLUTION LINK=LOGIT DIST=BINARY; 
     RANDOM INTERCEPT / SOLUTION TYPE=UN SUBJECT=PartID;  * Each subject gets a theta/U0;
     RANDOM INTERCEPT / SOLUTION TYPE=UN SUBJECT=picture; * Each item gets an easiness;
     COVTEST "Need item random int?" . 0; 
     COVTEST "Need subject random int?" 0 .; 
     ODS OUTPUT SolutionR=Crossed;
RUN; TITLE;
* Creating analogous individual item effects to fixed effects model;
DATA Crossed; SET Crossed; WHERE INDEX(Subject, "PICTURE")>0;
     Random_b=1.5928 + Estimate; * Add fixed intercept to each;
     KEEP Random_b;
RUN;

TITLE "SAS Random Subjects, LLTM + Random Items Model";
PROC GLIMMIX DATA=work.o38stack NOCLPRINT NOITPRINT GRADIENT METHOD=Laplace; 
     CLASS PartID picture;
     MODEL cor (DESCENDING) = rclut rrel rbrit legsign / SOLUTION LINK=LOGIT DIST=BINARY; 
     RANDOM INTERCEPT / SOLUTION TYPE=UN SUBJECT=PartID;  * Each subject gets a theta/U0;
     RANDOM INTERCEPT / SOLUTION TYPE=UN SUBJECT=picture; * Each item gets an easiness leftover;
RUN; TITLE;

DATA items; SET work.o38stack; WHERE PartID=201; RUN;
DATA Merged; MERGE Rasch (RENAME=(Estimate=Fixed_b)) Crossed items; RUN;
DATA Merged; SET Merged; 
     LLTM_pred_b    = 0.8619 + (-.2675*rclut) + (.2204*rrel) + (.4742*rbrit) + (.6621*legsign);
     Crossed_pred_b = 1.3105 + (-.3239*rclut) + (.03715*rrel) + (.7896*rbrit) + (.7388*legsign);
RUN;
PROC CORR DATA=Merged; VAR fixed_b random_b LLTM_pred_b Crossed_pred_b; RUN;

TITLE1 "SAS Add Random Clutter Slope (NPD)";
PROC GLIMMIX DATA=work.o38stack NOCLPRINT NOITPRINT GRADIENT METHOD=Laplace; 
     CLASS PartID picture;
     MODEL cor (DESCENDING) = rclut rrel rbrit legsign / SOLUTION LINK=LOGIT DIST=BINARY; 
     RANDOM INTERCEPT rclut / TYPE=UN SUBJECT=PartID;  * Each subject gets a theta;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=picture; * Each item gets an easiness;
     COVTEST "Need Random rclut Slope?" . 0 0 ;
RUN;

TITLE1 "SAS Add Random Relevance Slope (NS)";
PROC GLIMMIX DATA=work.o38stack NOCLPRINT NOITPRINT GRADIENT METHOD=Laplace; 
     CLASS PartID picture;
     MODEL cor (DESCENDING) = rclut rrel rbrit legsign / SOLUTION LINK=LOGIT DIST=BINARY; 
     RANDOM INTERCEPT rrel / TYPE=UN SUBJECT=PartID;  * Each subject gets a theta;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=picture; * Each item gets an easiness;
     COVTEST "Need Random rrel Slope?" . 0 0 ;
RUN;

TITLE1 "SAS Add Random Brightness Slope (NS)";
PROC GLIMMIX DATA=work.o38stack NOCLPRINT NOITPRINT GRADIENT METHOD=Laplace; 
     CLASS PartID picture;
     MODEL cor (DESCENDING) = rclut rrel rbrit legsign / SOLUTION LINK=LOGIT DIST=BINARY; 
     RANDOM INTERCEPT rbrit / TYPE=UN SUBJECT=PartID;  * Each subject gets a theta;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=picture; * Each item gets an easiness;
     COVTEST "Need Random rbrit Slope?" . 0 0 ;
RUN;

TITLE1 "SAS Add Random Legible Sign Slope (NS)";
PROC GLIMMIX DATA=work.o38stack NOCLPRINT NOITPRINT GRADIENT METHOD=Laplace; 
     CLASS PartID picture;
     MODEL cor (DESCENDING) = rclut rrel rbrit legsign / SOLUTION LINK=LOGIT DIST=BINARY; 
     RANDOM INTERCEPT legsign / TYPE=UN SUBJECT=PartID;  * Each subject gets a theta;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=picture; * Each item gets an easiness;
     COVTEST "Need Random legsign Slope?" . 0 0 ;
RUN;

* Close output;
ODS RTF CLOSE;
