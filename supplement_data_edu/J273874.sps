*  PSID DATA CENTER *****************************************************
   JOBID            : 273874                            
   DATA_DOMAIN      : TAi                               
   USER_WHERE       : NULL                              
   FILE_TYPE        : NULL                              
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : sps                               
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 18                                
   N_OF_OBSERVATIONS: 2526                              
   MAX_REC_LENGTH   : 40                                
   DATE & TIME      : March 21, 2020 @ 22:31:07
*************************************************************************
.

FILE HANDLE J273874 /
   NAME = "[path]\J273874.txt" LRECL = 40 .

DATA LIST FILE = J273874 FIXED /
      ER30000              1 - 1           ER30001              2 - 5           ER30002              6 - 8     
      ER32000              9 - 9           ER66001             10 - 10          ER70707             11 - 12    
      ER70717             13 - 14          ER70845             15 - 16          ER70855             17 - 18    
      ER34501             19 - 23          ER34502             24 - 25          ER34503             26 - 27    
      ER34504             28 - 30          ER34506             31 - 34          ER34507             35 - 35    
      TA170001            36 - 36          TA171981            37 - 38          TA171983            39 - 40    
   .
   EXECUTE .
VARIABLE LABELS
      ER30000       "RELEASE NUMBER"                          
      ER30001       "1968 INTERVIEW NUMBER"                   
      ER30002       "PERSON NUMBER                         68"
      ER32000       "SEX OF INDIVIDUAL"                       
      ER66001       "RELEASE NUMBER"                          
      ER70707       "K5 EDUCATION OF FATHER IN US-SP"         
      ER70717       "K15 EDUCATION OF MOTHER IN US-SP"        
      ER70845       "L5 EDUCATION OF FATHER IN US-RP"         
      ER70855       "L15 EDUCATION OF MOTHER IN US-RP"        
      ER34501       "2017 INTERVIEW NUMBER"                   
      ER34502       "SEQUENCE NUMBER                       17"
      ER34503       "RELATION TO REFERENCE PERSON          17"
      ER34504       "AGE OF INDIVIDUAL                     17"
      ER34506       "YEAR INDIVIDUAL BORN                  17"
      ER34507       "MARITAL PAIRS INDICATOR               17"
      TA170001      "RELEASE NUMBER"                          
      TA171981      "COMPLETED EDUCATION OF MOTHER"           
      TA171983      "COMPLETED EDUCATION OF FATHER"           
.
EXECUTE .
