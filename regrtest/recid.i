/* test rule "recid" */

DEFINE VARIABLE xyz AS RECID NO-UNDO.      /* vartype */
xyz = RECID(customer).                     /* assign  */
FIND customer WHERE xyz = RECID(customer) NO-LOCK. /* where   */
IF xyz = RECID(customer) THEN .            /* if      */
IF FALSE AND xyz = RECID(customer) THEN .  /* if      */

