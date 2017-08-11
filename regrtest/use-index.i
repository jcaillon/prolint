/* test rule "use-index" */

/* accept this one: */
DEFINE TEMP-TABLE tt_customer LIKE customer
   USE-INDEX SalesRep.

/* raise a warning about this one: */   
FIND FIRST customer NO-LOCK
   USE-INDEX NAME
   NO-ERROR.



