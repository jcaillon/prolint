/* sharelock.i:  testcases for rule "sharelock"
   warn for statements where no NO-LOCK or EXCLUSIVE-LOCK was specified, ignores temp-tables. */
   
DEFINE TEMP-TABLE tt_sharelock NO-UNDO 
   FIELD testfield AS CHAR.

DEFINE VARIABLE vSharelock AS CHAR NO-UNDO.
DEFINE QUERY    qcust      FOR customer, order.
DEFINE BUFFER   buf_cust   FOR customer. 
DEFINE BUFFER   buftt      FOR tt_sharelock.
                                                                 
/* see if we can distinguish temp-tables from database tables: */
FIND FIRST customer      WHERE customer.NAME = vShareLock NO-ERROR.                 
FIND FIRST buf_cust      WHERE buf_cust.name = vShareLock NO-ERROR.                 
FIND FIRST tt_sharelock  WHERE tt_sharelock.testfield = vShareLock NO-ERROR.                 
FIND FIRST buftt         WHERE buftt.testfield = "x" NO-ERROR.
                                                      
/* customer is fine, order should give a warning */                                                      
OPEN QUERY qcust FOR EACH customer WHERE customer.NAME = vSharelock NO-LOCK, EACH order OF customer.
               
/* see if we can distinguish temp-tables from database tables, but now with FOR: */
FOR EACH tt_sharelock :
   DISPLAY tt_sharelock.
END.
FOR EACH order:
   DISPLAY order.
END.   

/* demo/test _proparse_ directives: */

{&_PROPARSE_ prolint-nowarn(sharelock)}
FOR EACH order WHERE order.sales=? :      
    /* we actually do expect a warning here */
    FIND FIRST customer OF order NO-ERROR.                     
END.                          
                      
FIND FIRST buf_cust      WHERE buf_cust.name = vShareLock NO-ERROR.

DO PRESELECT EACH customer NO-LOCK:
  FIND NEXT customer. /* want no warning */
END.

DO PRESELECT EACH customer :
  FIND NEXT customer. /* want no warning here, but a warning on prev line */
END.

DO PRESELECT EACH order NO-LOCK :
  FIND NEXT customer. /* want a warning because customer is not preselected */
END.

DO PRESELECT EACH customer NO-LOCK
                           BREAK BY customer.city :
    FIND LAST customer NO-ERROR.
END.

