/* noeffect.i:  testcases for rule "noeffect"
   warn about statements that have no effect, because they are probably typos  */
RETURN.   
                                         
DEFINE VARIABLE i_noeffect AS INTEGER   NO-UNDO.
DEFINE VARIABLE c_noeffect AS CHARACTER NO-UNDO.
DEFINE VARIABLE h_noeffect AS HANDLE    NO-UNDO.       

FUNCTION UdfNoEffect RETURNS LOGICAL:
  /* test */
  RETURN TRUE.
END.

CURRENT-LANGUAGE = h_noeffect:SCREEN-VALUE.
CURRENT-LANGUAGE = "Dutch":U.               

/* the following 4 statements have effect: */                                                                                
UPDATE i_noeffect.
DISPLAY i_noeffect ENTERED.
DISPLAY i_noeffect NOT ENTERED.
i_noeffect = h_noeffect:NUM-RESULTS.
/* ...but these two should raise a warning: */
i_noeffect.
i_noeffect + 12.

/* handle:property has no effect, it just returns a value */
h_noeffect:READ-ONLY.
/* but handle:method is supposed to have some effect: */
h_noeffect:CLEAR().                              
/* just like a function call: */
UdfNoEffect().                  
 
/* if h_noeffect is a query object handle, the following 
   statements are effective but they still raise a warning
   because someone forgot to add the parentheses... */
h_noeffect:query-open.
h_noeffect:get-first.
h_noeffect:get-next.
                                      
/* this is better: */
h_noeffect:query-open().
h_noeffect:get-first().
h_noeffect:get-next().

/* a statement similar to this one was found in profiler.p: */ 
IF c_noeffect EQ ? THEN 
   c_noeffect EQ "".
                               

      