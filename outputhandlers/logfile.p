/* =======================================================================================
   file    : prolint/logfile.p
   purpose : write results (found by rules) to a logfile.
   by      : Jurjen Dijkstra
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

DEFINE VARIABLE logfile AS CHAR NO-UNDO.
                                            
SUBSCRIBE TO "Prolint_InitializeResults" ANYWHERE.
SUBSCRIBE TO "Prolint_AddResult" ANYWHERE.
SUBSCRIBE TO "Prolint_FinalizeResults" ANYWHERE.

logfile = SUBSTITUTE("&1prolint.log", DYNAMIC-FUNCTION("ProlintProperty", "outputhandlers.outputdirectory")).

RETURN.

                  
PROCEDURE Prolint_InitializeResults :  
   /* purpose : start with an empty logfile. If one exists make it empty */
   DEFINE INPUT PARAMETER pClearOutput AS LOGICAL NO-UNDO.
 
   IF pClearOutput THEN DO:  
      OUTPUT TO VALUE(logfile). 
      OUTPUT CLOSE.           
   END.
   
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
   DEFINE INPUT PARAMETER pSeverity         AS INTEGER NO-UNDO.  /* importance of this rule, scale 0-9    */
   
   OUTPUT TO VALUE (logfile) APPEND.
   PUT UNFORMATTED SUBSTITUTE ("&6;&3;line=&2;sourcefile=&1;rcodefile=&5;test=&4":U, 
                               pSourceFile, 
                               STRING(pLineNumber), 
                               pDescription, 
                               pRuleID, 
                               pCompilationUnit,
                               STRING(pSeverity))
                   SKIP.
   OUTPUT CLOSE.
END PROCEDURE.

   
PROCEDURE Prolint_FinalizeResults :                                    
   /* purpose: close the logfile and/or show it. Free resources  */
   
   /* In this case there are no open resources, so we're done. */
   /* This procedure will not be invoked again, so it can exit */
   DELETE PROCEDURE THIS-PROCEDURE.                          
   
END PROCEDURE.

