/* file wholeindex.i : test-cases for rule "wholeindex"
   looks for WHOLE-INDEX in XREF-file */

DEFINE TEMP-TABLE tt_wholeindex
   FIELD test1 AS CHAR 
   FIELD test2 AS INT
   INDEX idx1 AS PRIMARY test1.
                               
DEFINE BUFFER buf_wholeindex FOR tt_wholeindex.
DEFINE BUFFER buf_customer    FOR customer.

FIND FIRST customer WHERE customer.cust = 40.
FIND NEXT customer.

FIND buf_customer WHERE buf_customer.cust = 40.
FIND NEXT buf_customer.

FIND tt_wholeindex WHERE tt_wholeindex.test1 = "test".
FIND NEXT tt_wholeindex.

FIND buf_wholeindex WHERE buf_wholeindex.test1 = "test".
FIND NEXT buf_wholeindex.

FOR EACH customer NO-LOCK BY customer.balance :
END.
    
         