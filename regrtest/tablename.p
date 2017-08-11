
FOR EACH Customer FIELDS(Custnum Name) NO-LOCK:
END.

NAME = NAME + "dbfield!!!":U.

DEF VAR xyz AS integer NO-UNDO.

ON CLOSE OF THIS-PROCEDURE DO:
    DEFINE VARIABLE xyz AS INTEGER NO-UNDO.
    xyz = 15.
    FIND FIRST customer NO-LOCK.
    custnum = xyz.
END.    
       
       
procedure intprocA :
   DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
   FIND FIRST customer NO-LOCK.
   on value-changed of hWidget 
      PERSISTENT run ddbgdwufoejdbjh IN THIS-PROCEDURE(custnum).
END PROCEDURE.

procedure intprocB :
   DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
   on value-changed of hWidget 
      PERSISTENT run ddbgdwufoejdbjh IN THIS-PROCEDURE(hWidget).
END PROCEDURE.

