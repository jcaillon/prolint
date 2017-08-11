/* file findstate.i : test-cases for rule "findstate" */

FIND FIRST order NO-LOCK.
FIND customer OF order NO-LOCK. /* this should NOT raise a warning */

/* a couple of variations using NOT :  */

/* Table testtable1, unique index on field1 field2 field3 */
DEFINE VARIABLE v AS LOGICAL NO-UNDO.
find testtable1 no-lock
   where testtable1.field1 = 0
      and testtable1.field2 = (NOT V)
      and testtable1.field3 = "xxx":U
      no-error
   .

/* Table testtable1, unique index on field1 field2 field3 */
find testtable1 no-lock
   where testtable1.field1 = 0
      and NOT testtable1.field2 = V
      and testtable1.field3 = "xxx":U
      no-error
   .            