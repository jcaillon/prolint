/* ---------------------------------------------------------------------------------
   file    : prolint/regrtest/test.p. 
   purpose : provide a couple of test cases to test prolint.
   usage   : run prolint/launch/test.p.
   --------------------------------------------------------------------------------- */            
   
RETURN. /* nobody should actually run this program! 
           It might damage your sports/sports2000 database, or worse */

/* test-cases, one includefile for each rule: */

/* each includefile should be designed to:
     1. generate warnings for the rule of the same name
     2. don't generate warnings for other rules (if possible)
     3. include syntax that could easily generate "false positives" if the rule isn't sharp enough
     4. be careful not to define objects that might also be defined in other includefiles; use 
        the rule-id in objectnames one way or another
*/
                               
{prolint/regrtest/sepdbui.i}
{prolint/regrtest/noeffect.i}  
{Prolint/Regrtest/Message.i}  
{prolint/regrtest/varusage.i}  
{prolint/regrtest/nocomment.i}
{prolint/regrtest/abbrevkwd.i}         
{prolint/regrtest/blocklabel.i}
{prolint/regrtest/noundo.i}         
{prolint/regrtest/wholeindex.i}         
{prolint/regrtest/nowhere.i}         
{prolint/regrtest/SortAccess.i}         
{prolint/regrtest/sharelock.i}
{prolint/regrtest/RunName.i} 
{prolint/regrtest/strattrib.i}
{prolint/regrtest/i18nlength.i}    
{prolint\regrtest\shared.i}
{prolint/regrtest/substitute.i}
{prolint/regrtest/runargs.i}
{prolint/regrtest/recid.i}
{prolint/regrtest/dotcomment.i}
{prolint/regrtest/do1.i}
{prolint/regrtest/findstate.i}
{prolint/regrtest/oflink.i}

/* see more cases in prolint/regrtest/test2.p */

