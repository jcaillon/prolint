/* test rule "sepdbui" */

/* "LIKE" would raise a false positive.
   False, because db-access is only at compiletime - not at run-time */
DEFINE TEMP-TABLE tt_sepdbui NO-UNDO LIKE customer.

