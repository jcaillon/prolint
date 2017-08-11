/* testcases for runargs: run-time arguments  */

{&_proparse_ prolint-nowarn(varusage)}
define variable runargs_1 as integer no-undo.
{&_proparse_ prolint-nowarn(varusage)}
define variable runargs_2 as integer no-undo.
{&_proparse_ prolint-nowarn(varusage)}
define variable runargs_3 as integer no-undo.

/* common mistake: too many right-parentheses */
run something ((""))).
run something (("")) ).  /* with an extra space */

/* difficult: filenames with a space in their name */
run something .p ("a":U).

/* forgot the dot at the end of the line: next line is runtime argument! */
run jhsgfjhg.p ("")
runargs_1 = runargs_2 + runargs_3.


