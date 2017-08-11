

loop1:
  DO WHILE TRUE
     ON ERROR UNDO.

     UNDO loop1.

     UNDO, RETRY loop1.
 
     UNDO, RETRY.
   

     UNDO, LEAVE.

     UNDO, LEAVE loop1.
  END.


loop2:
  DO WHILE TRUE
     ON ERROR UNDO, LEAVE.

     UNDO loop2.

     UNDO, RETRY loop2.
 
     UNDO, RETRY.
   

     UNDO, LEAVE.

     UNDO, LEAVE loop2.
  END.

DEFINE TEMP-TABLE ttblaat NO-UNDO
   FIELD c AS CHARACTER.
/* false positive on the following UNDO?? */
DEFINE TEMP-TABLE ttblaatundo UNDO LIKE ttblaat.
                              
