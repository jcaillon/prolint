
DEFINE TEMP-TABLE ttAccount NO-UNDO
FIELD Id AS CHARACTER
INDEX i1 IS UNIQUE PRIMARY Id.

FIND ttAccount WHERE ttAccount.Id = "xyz":U NO-ERROR.



