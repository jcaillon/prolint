/* runname.i: test-cases for rule "runname"
   see if filenames are Unix-compatible.
   rule "runname" looks for backslashes and uppercase in filenames. */

DEFINE VARIABLE hRunname AS HANDLE NO-UNDO.
DEFINE VARIABLE vRunname AS CHAR NO-UNDO.
DEFINE VARIABLE hRunnameServer AS HANDLE NO-UNDO.

/* test for uppercase filespecs and backslashes */
RUN gjhgjhg.p.
RUN subdir/ghhtf.p.
RUN SubDir/Ghhtf.p.
RUN subdir\ghhtf.p.
RUN SubDir\Ghhtf.p.

/* this is an internal procedure because it has no dot in its name */
RUN Without_dot.

/* same name, but now it's external because it runs persistent */
RUN Without_dotp PERSISTENT SET hRunname.
RUN without_dotp PERSISTENT SET hRunname.
RUN jkhhgf.p PERSISTENT SET hRunname.
RUN Jkhhgf.p PERSISTENT SET hRunname.
                                                      
/* look at the string literals in the VALUE expression */                                                      
RUN VALUE( "part1":U + vRunname + ".p":T2) PERSISTENT SET hRunname.
RUN VALUE( "Part1":U + vRunname + ".p":T2) PERSISTENT SET hRunname.

/* this is internal, because of the IN keyword, even if there's a dot in the name */
RUN soMethIng.p IN hRunname.

RUN kjckjshcr.p ('parameter1', 'parameter2', OUTPUT vRunname).

/* the next are not Unix-compatible, but warning is suppressed for the first one */
/* 35 */ {&_PROPARSE_ prolint-nowarn(runname)}
/* 36 */ RUN SubDir\Shgchcg.p.
/* 37 */ RUN SubDir\Shgchcg.p.
/* 38 */ 
/* 39 */ {&_PROPARSE_ prolint-nowarn(strattrib,runname)} 
/* 40 */ RUN VALUE( "Part1" + vRunname + ".p":T2) PERSISTENT SET hRunname.
/* 41 */ RUN VALUE( "Part1" + vRunname + ".p":T2) PERSISTENT SET hRunname.
/* 42 */
/* 43 */ {&_PROPARSE_ prolint-nowarn(runname)}
/* 44 */ DO:
/* 45 */   RUN SubDir\Shgchcg.p.
/* 46 */   RUN Another\Shgchcg.p.
/* 47 */ END.
/* 48 */ RUN Rjhsghj.p.
   
/* statement containing both ON and IN : */   
RUN Ahjsgf.p ON SERVER hRunnameServer ASYNCHRONOUS EVENT-PROCEDURE "event_ip" IN hRunname.
       
               
PROCEDURE RTB_xref_generator :   
/* RUN statements are insterted by Roundtable when you add a smartobject to 
   a smartcontainer: */
   
   RUN "gsd\hwhdhe.w *RTB-SmObj* ".
   RUN gsd\hwhdhe.r.
   
END PROCEDURE.   
                                
RUN VALUE("test\jhdgjah.p").
RUN VALUE("test~\jhdgjah.p").
RUN VALUE("test\\jhdgjah.p").

PROCEDURE CtrlFrame.TreeView.NodeClicked :
/* progname contains dots, so prolint might think it is an external procedure? */
   {&_PROPARSE_ prolint-nowarn(varusage)}
   DEFINE INPUT PARAMETER cNothing AS CHARACTER NO-UNDO.
END PROCEDURE.

RUN CtrlFrame.TreeView.NodeClicked ('whatever':U).


