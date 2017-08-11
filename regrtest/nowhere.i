/* file nowhere.i : test-cases for rule "nowhere"
   looks for queries without WHERE-clause */

DEFINE TEMP-TABLE tt_nowhere NO-UNDO
   FIELD test1 AS CHAR 
   FIELD test2 AS INT
   INDEX idx1 AS PRIMARY test1.
                               
FIND FIRST customer WHERE customer.cust = 40 NO-LOCK.
FIND NEXT customer NO-LOCK.

FIND tt_nowhere WHERE tt_nowhere.test1 = "test":U.
FIND NEXT tt_nowhere.

FOR EACH customer NO-LOCK WHERE customer.cust > 40,
    EACH order NO-LOCK OF customer :
END.

FIND FIRST customer NO-LOCK WHERE customer.cust > 40.
FIND CURRENT customer EXCLUSIVE-LOCK NO-ERROR.

DEFINE BUFFER buf-cust FOR customer.
DO FOR buf-cust TRANSACTION :
END.

