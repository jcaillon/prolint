/* ----------------------------------------------
   file    : prolint/regrtest/nocomment.i
   purpose : a couple of test cases for rule "nocomment":
             Report each function/procedure that does not begin with a comment.
             Prototype declarations are ignored.
   --------------------------------------------------------------------------- */             

/* forward declarartions needs not be commented. */
FUNCTION udf_nocomment_1 returns CHARACTER (test1 AS CHARACTER) forward. /* end with PERIOD */
FUNCTION udf_nocomment_2 returns CHARACTER (test1 AS CHARACTER) forward: /* ends with COLON */

/* external procedures need not be commented,
   they are supposed to be commented in the API reference of dll-manufacturer */
PROCEDURE ip_nocomment_1 EXTERNAL "nonsense.dll":U :
   DEFINE INPUT  PARAMETER something AS LONG NO-UNDO.
   DEFINE RETURN PARAMETER retval    AS SHORT NO-UNDO.
END PROCEDURE.

/* "IN SUPER" is treated as external: no comment required */
/* this one ends with PERIOD */
PROCEDURE ip_nocomment_2 IN SUPER.
   DEFINE INPUT PARAMETER xyz  AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER abc AS INTEGER NO-UNDO.
END PROCEDURE.

/* but this one ends with COLON */
PROCEDURE ip_nocomment_2b IN SUPER:
   DEFINE INPUT PARAMETER xyz  AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER abc AS INTEGER NO-UNDO.
END PROCEDURE.
                                                                         

PROCEDURE ip_nocomment_3 :
   DISPLAY "hello":U.
END PROCEDURE.                                       
   

PROCEDURE ip_nocomment_4 .
   DISPLAY "hello":U.
END PROCEDURE.

PROCEDURE ip_nocomment_5 :
   /* this one ends with COLON */
   /* purpose : at last, an ip with a comment */
   DISPLAY "hello":U.
END PROCEDURE.

PROCEDURE ip_nocomment_6 .
   /* but this one ends with PERIOD */
   /* purpose : at last, an ip with a comment */
   DISPLAY "hello":U.
END PROCEDURE.
                          
FUNCTION udf_nocomment_1 returns CHARACTER (test1 AS CHARACTER).
   RETURN "abc":U.
END FUNCTION.

FUNCTION udf_nocomment_2 returns CHARACTER (test1 AS CHARACTER):
   RETURN "def":U.
END FUNCTION.

FUNCTION udf_nocomment_3 returns CHARACTER (test1 AS CHARACTER).
   /* purpose: test rule nocomment */
   RETURN "abc":U.
END FUNCTION.

FUNCTION udf_nocomment_4 returns CHARACTER (test1 AS CHARACTER):
   /* purpose: test rule nocomment */
   RETURN "def":U.
END FUNCTION.

  
{&_proparse_ prolint-nowarn(varusage)}  
DEFINE VARIABLE nocomment_var1 AS CHARACTER NO-UNDO.
{&_proparse_ prolint-nowarn(varusage)}  
DEFINE VARIABLE nocomment_var2 AS CHARACTER NO-UNDO.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadCfgEditor C-Win 
PROCEDURE ReadCfgEditor :
/*------------------------------------------------------------------------------
  Purpose:     this procedure doesn't start with DEFINE and 
               proparse inserts an implicit ASSIGN. That breaks the nocomment rule
               if you traverse nodes from ASSIGN backwards.
------------------------------------------------------------------------------ */

  FILE-INFO:FILE-NAME = "prolint/settings/exteditor.cfg":U.
  IF file-info:full-pathname<>? THEN DO:
     INPUT FROM VALUE(file-info:FULL-PATHNAME).
       IMPORT UNFORMATTED nocomment_var1.
       IMPORT UNFORMATTED nocomment_var2.
     INPUT CLOSE.
  END.             

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




PROCEDURE AbGeneratedProcedure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DISPLAY "nothing":U.

END PROCEDURE.  /* of procedure AbGeneratedProcedure */


PROCEDURE ip_nocomment_7 :
END PROCEDURE.

/* accept a comment before the procedure keyword.
   but not the one before ip_nocomment_7 because that
   one belongs to the END keyword */

PROCEDURE ip_nocomment_8 :
END PROCEDURE.



