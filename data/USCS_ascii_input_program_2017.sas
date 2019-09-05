/******************************************************************************************************/
/* Program Name - USCS_ASCII_INPUT_PROGRAM.SAS                                                        */
/******************************************************************************************************/
/* ICF, May 2017                                                                                      */
/******************************************************************************************************/
/* Purpose - To read 10 ascii text files to create appropriate SAS datasets. The users need to assign */
/* the specific location where the text files are saved. The users also need to assign the libname    */
/* pointing to the directory to hold the new SAS files.                                               */
/******************************************************************************************************/

/***********************************************************************************************/
/* Assign libname to hold SAS files                                                            */
/***********************************************************************************************/
libname out "The location where SAS files will be saved";

/***********************************************************************************************/
/* Assign the location where ASCII files are saved.                                            */
/***********************************************************************************************/
%let textpath=The locatiion where ASCII files are saved;

%MACRO SASREAD(FILE);
PROC IMPORT OUT= OUT.&FILE
            DATAFILE= "&TEXTPATH\&FILE..TXT"
            DBMS=DLM REPLACE;
     DELIMITER='|';
     GETNAMES=YES;
     DATAROW=2;
     GUESSINGROWS=500000;
RUN;
%MEND SASREAD;

%SASREAD(BRAINBYSITE)
%SASREAD(BYAGE)
%SASREAD(BYAREA)
%SASREAD(BYSITE)
%SASREAD(CHILDBYAGE_ADJ)
%SASREAD(CHILDBYAGE_CR)
%SASREAD(CHILDBYSITE)
%SASREAD(ICCCBYAGE_ADJ)
%SASREAD(ICCCBYAGE_CR)
%SASREAD(ICCCBYSITE)
