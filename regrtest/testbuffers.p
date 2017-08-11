/*------------------------------------------------------------------------
    File        : testbuffers.p
    Purpose     : testcases for regression testing 
    Author(s)   : jurjen
    Created     : Sat Apr 12 16:49:28 CEST 2008
    Notes       :
  ----------------------------------------------------------------------*/

FUNCTION FnReadCustomer RETURNS INTEGER (BUFFER P_bCustomer FOR Customer, INPUT xyz AS INTEGER):
   DEFINE VARIABLE nCustomer AS INTEGER NO-UNDO.
   FOR EACH P_bCustomer NO-LOCK:
       nCustomer = nCustomer + 1.
   END.
   RETURN nCustomer.
END FUNCTION.

FUNCTION FnTestHandle RETURNS INTEGER (hh AS HANDLE):
   RETURN 8.
END FUNCTION.

DEFINE BUFFER bCustomer FOR Customer.
FnTestHandle ((BUFFER bCustomer:HANDLE)).  /* notice the double parenthesis? */

PROCEDURE SomeLocalBlock :
    DEFINE BUFFER bufOrder FOR Order.
    FnTestHandle (BUFFER bufOrder:HANDLE).
END PROCEDURE.    

DEFINE VARIABLE qh AS HANDLE NO-UNDO.
ON GO ANYWHERE DO:
   DEFINE BUFFER buf-dossier-path FOR customer.
   DEFINE BUFFER buf-combinations FOR order.

   IF true THEN DO:
      CREATE QUERY qh.
      qh:SET-BUFFERS(BUFFER buf-dossier-path:HANDLE, BUFFER buf-combinations:HANDLE).
   END.
END.



