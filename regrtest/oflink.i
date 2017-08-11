/* some test cases for rule "oflink" */

FIND FIRST customer NO-LOCK NO-ERROR.

FIND FIRST order OF customer NO-LOCK NO-ERROR.

FOR EACH order NO-LOCK OF customer :

END.

IF CAN-FIND(FIRST order OF customer) THEN .

IF CAN-FIND(FIRST order WHERE order.salesrep="me":U) THEN .

DEFINE QUERY qoflink FOR customer, order.

OPEN QUERY qoflink FOR EACH customer NO-LOCK, EACH order NO-LOCK OF customer.

