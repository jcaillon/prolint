/* =======================================================================================
   file    : prolint/regrlog.p
   by      : Jurjen Dijkstra
   purpose : write results to a logfile. Intended to be used by profile "regression test"
             which is used by program prolint/launch/test.p
    -----------------------------------------------------------------

    Copyright (C) 2001,2002 Jurjen Dijkstra

    This file is part of Prolint.

    Prolint is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    Prolint is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Prolint; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   ======================================================================================= */
{prolint/core/dlc-version.i}
                                                                      
DEFINE VARIABLE tempdir AS CHAR NO-UNDO.                                                                      
DEFINE VARIABLE logfile AS CHAR NO-UNDO.

SUBSCRIBE TO "Prolint_InitializeResults" ANYWHERE.
SUBSCRIBE TO "Prolint_AddResult" ANYWHERE.
SUBSCRIBE TO "Prolint_FinalizeResults" ANYWHERE.

RETURN.

          
PROCEDURE Prolint_InitializeResults :  
   /* purpose : start with an empty logfile. If one exists make it empty */
   {&_proparse_ prolint-nowarn(varusage)}
   DEFINE INPUT PARAMETER pClearOutput AS LOGICAL NO-UNDO.
   
   tempdir =  SESSION:TEMP-DIRECTORY.
   IF NOT ((SUBSTRING(tempdir, LENGTH(tempdir,"CHARACTER":U)) = '~\':U) OR (SUBSTRING(tempdir, LENGTH(tempdir,"CHARACTER":U)) = '/':U)) THEN 
      tempdir = tempdir + '/':U.       
   logfile = tempdir + "lint-selftest.log":U.
   
   OUTPUT TO VALUE(logfile). 
   OUTPUT CLOSE.
END PROCEDURE.              
   
   
PROCEDURE Prolint_AddResult :              
   /* purpose: add one result from a 'rule' to the logfile, 
               using the format of your choice.
               The format in this example looks pretty useless to me */
   DEFINE INPUT PARAMETER pCompilationUnit  AS CHAR    NO-UNDO.  /* the sourcefile we're parsing          */
   DEFINE INPUT PARAMETER pSourcefile       AS CHAR    NO-UNDO.  /* may be an includefile                 */
   DEFINE INPUT PARAMETER pLineNumber       AS INTEGER NO-UNDO.  /* line number in pSourceFile            */
   DEFINE INPUT PARAMETER pDescription      AS CHAR    NO-UNDO.  /* human-readable hint                   */
   DEFINE INPUT PARAMETER pRuleID           AS CHAR    NO-UNDO.  /* defines rule-program and maps to help */
   {&_proparse_ prolint-nowarn(varusage)}
   DEFINE INPUT PARAMETER pSeverity         AS INTEGER NO-UNDO.  /* importance of this rule, scale 0-9    */
   
   OUTPUT TO VALUE (logfile) APPEND.
   PUT UNFORMATTED SUBSTITUTE ("rule=&4; source=&1; line=&2; descr=&3":U, 
                               LC(pSourceFile), 
                               STRING(pLineNumber,"zzz9":U), /* format makes it easier to sort */
                               pDescription, 
                               pRuleID, 
                               pCompilationUnit)
                   SKIP.
   OUTPUT CLOSE.
END PROCEDURE.

   
PROCEDURE Prolint_FinalizeResults :                                    
   /* purpose: close the logfile.
               compare it to the expected logfile, differences are possible bugs (or new features) */

   DEF VAR file1 AS CHAR NO-UNDO.
   DEF VAR file2 AS CHAR NO-UNDO.
   DEF VAR file3 AS CHAR NO-UNDO.

   file-info:FILE-NAME = logfile.
   file1 = file-info:FULL-PATHNAME.
   
   file-info:FILE-NAME = "prolint/regrtest/expect.log":U.
   file2 = file-info:FULL-PATHNAME.

   file-info:FILE-NAME = "prolint/regrtest-oo/expect.log":U.
   file3 = file-info:FULL-PATHNAME.
   
   /* compare the new logfile to the old one: */                       
   OUTPUT TO VALUE(tempdir + "regrtest.bat":U).
      
      IF SUBSTRING(tempdir,2,1)=":":U THEN 
         PUT UNFORMATTED SUBSTRING(tempdir,1,2) + "~n":U.
      
      PUT UNFORMATTED SUBSTITUTE('cd "&1"~n':U,tempdir).
      PUT UNFORMATTED SUBSTITUTE('sort "&1">prolint.log~n':U,file1).
      &IF {&dlc-version}>=10 &THEN
      PUT UNFORMATTED SUBSTITUTE('copy "&1"+"&2" expects.log~n':U, file2,file3).
      &ELSE
      PUT UNFORMATTED SUBSTITUTE('copy "&1" expects.log~n':U, file2).
      &ENDIF
      PUT UNFORMATTED SUBSTITUTE('sort expects.log>expect.log~n':U, file2,file3,tempdir).
      PUT UNFORMATTED            'fc prolint.log expect.log > diff.log~n':U.
  
   OUTPUT CLOSE.                                                                                 

   OS-COMMAND SILENT VALUE(tempdir + "regrtest.bat":U).

   RUN prolint/proparse-shim/utilities/resultswindow.p (tempdir + "diff.log":U).
     
   /* This procedure will not be invoked again, so it can exit */
   DELETE PROCEDURE THIS-PROCEDURE.                          
   
END PROCEDURE.

