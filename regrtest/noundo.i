 /* file  noundo.i : test-cases for rule "noundo"
   finds define parameter|variable without no-undo */


DEFINE VARIABLE noundo_1 AS CHARACTER.
DEFINE VARIABLE noundo_2 AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt_noundo 
   FIELD test1 AS CHAR 
   FIELD test2 AS INT
   INDEX idx1 AS PRIMARY test1.

                      
PROCEDURE ip_noundo :         
   /* see if the rule also inspects parameters and vars inside an ip */
   DEFINE INPUT  PARAMETER ip_noundo_1 AS CHAR.
   {&_proparse_ prolint-nowarn(abbrevkwd)}
   DEF OUTPUT PARAM ip_noundo_2 AS CHAR.
   DEFINE INPUT-OUTPUT PARAMETER ip_noundo_3 AS LOGICAL INITIAL TRUE.
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt_Noundo.
      
   DEFINE VARIABLE noundo_1 AS CHARACTER.
   DEFINE VARIABLE noundo_2 AS CHARACTER NO-UNDO.   
   
END.                                       

                 
PROCEDURE apifunction EXTERNAL "somedll.dll":U :
  DEFINE INPUT PARAMETER cX AS LONG.
  DEFINE INPUT PARAMETER cY AS LONG.
END.                 
                            
DEFINE TEMP-TABLE tt_noundo2 NO-UNDO
   FIELD test1 AS CHAR 
   FIELD test2 AS INT
   INDEX idx1 AS PRIMARY test1.
                                             
/* NO-UNDO is not explicitly specified, but inherited from tt_noundo2.
   Still Prolint should raise a warning, because NO-UNDO was not specified (explicitly): */
DEFINE TEMP-TABLE tt_noundo3 LIKE tt_noundo2.

/* but not for these two, because they are explicit */
DEFINE TEMP-TABLE tt_noundo4 NO-UNDO LIKE tt_noundo.
DEFINE TEMP-TABLE tt_noundo5 UNDO    LIKE tt_noundo2.
                            

PROCEDURE ip_noundo3 :
   /* look for false positives: don't report these: */
   {&_proparse_ prolint-nowarn(varusage)}
   DEFINE INPUT  PARAMETER ip_noundo_1 AS CHARACTER NO-UNDO.
   {&_proparse_ prolint-nowarn(varusage)}
   DEFINE INPUT  PARAMETER ip_noundo_2 AS HANDLE    NO-UNDO.
   {&_proparse_ prolint-nowarn(varusage)}
   DEFINE OUTPUT PARAMETER ip_noundo_3 AS INTEGER   NO-UNDO.
   {&_proparse_ prolint-nowarn(varusage)}
   DEFINE INPUT-OUTPUT PARAMETER ip_noundo_4 AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER ip_noundo_5 AS HANDLE NO-UNDO.
   {&_proparse_ prolint-nowarn(varusage)}
   DEFINE INPUT PARAMETER TABLE-HANDLE iph_noundo.
END.
                            