
IF TRUE THEN DO:
    
    FUNCTION nestedfunc RETURNS CHARACTER ( someparameter AS character ) :
        RETURN someparameter + "silly":U.
    END FUNCTION.
    
    PROCEDURE nestedproc :
        DEFINE INPUT PARAMETER someparam AS CHARACTER NO-UNDO.
        DEFINE INPUT PARAMETER notused AS INTEGER NO-UNDO.
        someparam = "nothing really":U.
    END PROCEDURE.
    
END.

    