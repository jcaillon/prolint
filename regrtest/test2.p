/* ---------------------------------------------------------------------------------
   file    : prolint/regrtest/test2.p.
             see also prolint/regrtest/test.p.
   purpose : provide a couple of test cases to test prolint.
   usage   : run prolint/launch/test.p.
   --------------------------------------------------------------------------------- */            
   
RETURN. /* nobody should actually run this program! */

/* test-cases, one includefile for each rule: */

/* each includefile should be designed to:
     1. generate warnings for the rule of the same name
     2. don't generate warnings for other rules (if possible)
     3. include syntax that could easily generate "false positives" if the rule isn't sharp enough
     4. be careful not to define objects that might also be defined in other includefiles; use 
        the rule-id in objectnames one way or another
*/

{prolint/regrtest/ifindent.i}   

{prolint/regrtest/where-udf.i}
{prolint/regrtest/colon-t.i}
{prolint/regrtest/use-index.i}


