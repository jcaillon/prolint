/* test cases for rule "ifparens" */
DEFINE VARIABLE a AS INTEGER NO-UNDO.
DEFINE VARIABLE b AS INTEGER NO-UNDO.

IF a=b THEN a=b.

a = IF b=2
      THEN 23
      ELSE 876
    + 2.

a = 2 + IF b=2 THEN 23 ELSE 876.

a = (IF b=2 THEN 23 ELSE 876) + 2.

a = (IF b=2 THEN 23 ELSE 876 + 2).

a = IF b=2 THEN 23 ELSE (876 + 2).

a = IF b=2
      THEN 23
      ELSE (876 + 2)
      + 5.

a = IF b=2
      THEN 23
      ELSE (876
            + 2)
    + 5.

a = (IF b=2
      THEN 23
      ELSE (876
            + 2))
    + 5.
    
{&_proparse_ prolint-nowarn(ifparens)}
a = IF b=2 THEN 23 ELSE (876 + 2) + 5.


