/* =======================================================================================
   file    : prolint/ed4win.p
   purpose : publish results (found by rules) to the Ed4Win "Build"-window
   by      : Jurjen Dijkstra
   note    : requires at least Progress 9.
             prolint/ed4win/serveEd4Win.p needs to be running persistently,
             a connection with Ed4Win needs to be established by prorun.exe
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

SUBSCRIBE TO "Prolint_InitializeResults" ANYWHERE.
SUBSCRIBE TO "Prolint_AddResult" ANYWHERE.
SUBSCRIBE TO "Prolint_FinalizeResults" ANYWHERE.

define variable numWarnings as integer no-undo initial 0.
RETURN.

                  
PROCEDURE Prolint_InitializeResults :  
   /* purpose : start with an empty logfile. If one exists make it empty */
   DEFINE INPUT PARAMETER pClearOutput AS LOGICAL NO-UNDO.

   numWarnings = 0.
END PROCEDURE.
   
   
PROCEDURE Prolint_AddResult :              
   /* purpose: send one result from a 'rule' to Ed4Win,
               formatted like a Progress syntax error. */
   DEFINE INPUT PARAMETER pCompilationUnit  AS CHAR    NO-UNDO.  /* the sourcefile we're parsing          */
   DEFINE INPUT PARAMETER pSourcefile       AS CHAR    NO-UNDO.  /* may be an includefile                 */
   DEFINE INPUT PARAMETER pLineNumber       AS INTEGER NO-UNDO.  /* line number in pSourceFile            */
   DEFINE INPUT PARAMETER pDescription      AS CHAR    NO-UNDO.  /* human-readable hint                   */
   DEFINE INPUT PARAMETER pRuleID           AS CHAR    NO-UNDO.  /* defines rule-program and maps to help */
   DEFINE INPUT PARAMETER pSeverity         AS INTEGER NO-UNDO.  /* importance of this rule, scale 0-9    */

   /* make full-qualified filename so Ed4Win can always find it, no matter how the
      Search Paths are configured */
   file-info:file-name = pSourcefile.
   pSourcefile = file-info:full-pathname.
   pSourcefile = REPLACE(pSourcefile, "/":U, "~\":U).

   numWarnings = numWarnings + 1.

   /* publish message, formatted like a Progress syntax error. */
   /* (WriteToEd4Windows is in procedure proed4w/proed4w.p) */
   PUBLISH "WriteToEd4Windows":U
                             (SUBSTITUTE("** &1 line &2: &3 (rule &4)":U,
                                         pSourceFile,
                                          STRING(pLineNumber),
                                          pDescription,
                                          pRuleID)).


END PROCEDURE.

   
PROCEDURE Prolint_FinalizeResults :                                    
   /* purpose: close the logfile and/or show it. Free resources  */

   if numWarnings = 0 then
      PUBLISH "WriteToEd4Windows":U ("Prolint found no problems       ":U).

   PUBLISH "EndEd4WindowsConversation":U.

   DELETE PROCEDURE THIS-PROCEDURE.                          
   
END PROCEDURE.

