/* some unit-test cases for rule streamclose */
define input parameter pc1 as char no-undo.          /* ignore this INPUT/OUTPUT */
define output parameter opc1 as char no-undo.        /* ignore this INPUT/OUTPUT */
define input-output parameter iopc1 as char no-undo. /* ignore this INPUT/OUTPUT */

define variable value1 as character no-undo.
define variable value2 as integer no-undo.

run test (input value1, output value2).              /* ignore this INPUT/OUTPUT */

define stream s1.
define stream s2.
define stream sUnused.                               /* Error, stream defined but not used */

output to c:\temp\test1.txt.

output stream s1 to value(value1).                   /* Error, stream s1 opened for output but not closed */

output close.

input through ls.                                    /* Error, unnamed-input opened for input but not closed */

output stream s2 to "c:/temp/1.txt".
input stream s2 from "c:/temp/1.txt".                /* Error, stream s2 opened for input before closed for output */
