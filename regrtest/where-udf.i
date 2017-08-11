/* where-udf.i */

FUNCTION wu-test1 RETURNS INTEGER (sometext AS CHAR) :
   /* this function is NOT safe because it contains db-access */
   FIND FIRST salesrep NO-LOCK NO-ERROR.
   RETURN 50.
END FUNCTION.

FUNCTION wu-test2 RETURNS INTEGER (sometext AS CHAR) :
   /* this function is safe */
   RETURN 50 + 34.
END FUNCTION.

/* expect warning */
FOR EACH customer NO-LOCK WHERE customer.balance = wu-test1 ("hello":U) :
END.

/* expect warning */
FOR EACH customer NO-LOCK WHERE customer.balance = DYNAMIC-FUNCTION("wu-test1":U, "hello" ) :
END.

/* don't expect warning */
FOR EACH customer NO-LOCK WHERE customer.balance = wu-test2 ("hello":U) :
END.

/* don't expect warning */
FOR EACH customer NO-LOCK WHERE customer.balance = DYNAMIC-FUNCTION("wu-test2":U, "hello" ) :
END.


