
define variable substitute_1 as character no-undo.
define variable substitute_2 as character no-undo.
define variable substitute_3 as integer no-undo.
{&_proparse_ prolint-nowarn(varusage)}
define variable substitute_4 as character no-undo.

{&_proparse_ prolint-nowarn(strattrib)}
substitute_4 = "you ":U +
               "should ":T +
               substitute_1 +
               substitute_2 +
               " really use ":U +
               substitute_2 +
               " subsitute!":U +
               string(substitute_3,"999":U).

/* let's see if the _proparse_ directive still works */
{&_proparse_ prolint-nowarn(substitute,strattrib)}
substitute_4 = "you ":U +
               "should ":T +
               substitute_1 +
               substitute_2 +
               " really use ":U +
               substitute_2 +
               " subsitute!":U +
               string(substitute_3,"999":U).

{&_proparse_ prolint-nowarn(strattrib)}
substitute_4 = "less obvious":U + substitute_1.

{&_proparse_ prolint-nowarn(strattrib)}
substitute_4 = "more " + substitute_1 + " obvious".

{&_proparse_ prolint-nowarn(strattrib)}
DISPLAY "also ":U + substitute_1 + " obvious".

/* same statements, but now all strings have the :U attribute
   so there is no need to use the SUBSTITUTE function.
   After all, we're only using SUBSTITUE to help the translator. */

substitute_4 = "you ":U +
               "should ":U +
               substitute_1 +
               substitute_2 +
               " really use ":U +
               substitute_2 +
               " subsitute!":U +
               string(substitute_3,"999":U).

substitute_4 = "you ":U +
               "should ":U +
               substitute_1 +
               substitute_2 +
               " really use ":U +
               substitute_2 +
               " subsitute!":U +
               string(substitute_3,"999":U).
               
substitute_4 = "less obvious":U + substitute_1.

substitute_4 = "more ":U + substitute_1 + " obvious":U.

DISPLAY "also ":U + substitute_1 + " obvious":U.

/* your typical statement for list-item-pairs : */
substitute_4 = substitute_1 + ",":U + "screen-value":T.

