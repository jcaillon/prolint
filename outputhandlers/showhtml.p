/* =======================================================================================
    file    : prolint/outputhandlers/showhtml.p
    purpose : write results (found by rules) to an html file and show it
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
DEFINE VARIABLE oddrow  AS LOGICAL NO-UNDO.
DEFINE VARIABLE prolint_url AS CHAR NO-UNDO.

logfile = SUBSTITUTE("&1&2lint_result.html", DYNAMIC-FUNCTION("ProlintProperty", "outputhandlers.outputdirectory"),
                                             DYNAMIC-FUNCTION("ProlintProperty", "logincode")).

prolint_url = "http://oehive.org/prolint/rules":U.

/* cache helpfile locations */
DEFINE TEMP-TABLE tt_help NO-UNDO 
  FIELD ruleid AS CHARACTER 
  FIELD url    AS CHARACTER
  INDEX ixrule AS PRIMARY UNIQUE ruleid.
  
                                            
SUBSCRIBE TO "Prolint_InitializeResults" ANYWHERE.
SUBSCRIBE TO "Prolint_Status_FileStart" ANYWHERE.
SUBSCRIBE TO "Prolint_AddResult" ANYWHERE.
SUBSCRIBE TO "Prolint_Status_FileEnd" ANYWHERE.
SUBSCRIBE TO "Prolint_FinalizeResults" ANYWHERE.
   
RETURN.

FUNCTION helpfile RETURNS CHARACTER (pRuleid AS CHARACTER) :
   /* purpose: find url to helpfile of pRuleid. Use a temp-table to cache results for performance */

   RETURN prolint_url + "/":U + pRuleId.

END FUNCTION.
                  
PROCEDURE Prolint_InitializeResults :  
   /* purpose : start with an empty logfile. If one exists make it empty */
   DEFINE INPUT PARAMETER pClearOutput AS LOGICAL NO-UNDO.
 
   IF pClearOutput THEN DO:  
      OUTPUT TO VALUE(logfile).
      PUT UNFORMATTED 
'<HTML>
<HEAD> 
<STYLE>     
BODY   ~{font-family: "Arial"; font-size: 9pt; ~}           
H1     ~{font-size: 15pt; color: 800040; text-align: Left; ~}
H2     ~{font-size: 12pt; color: BB0000; text-align: Left; ~}
TD     ~{font-family: "Arial"; font-size: 9pt; ~}
.thead ~{background-color : #003399; color: #FFFFFF; font-weight: 700;~} 
.even  ~{background-color : #CCFFFF~}
.odd   ~{background-color : #FFFFCC~}
</STYLE>
</HEAD> 
<BODY>
<h1>Prolint results</h1>':U
      SKIP.
      OUTPUT CLOSE.           
   END.
   
END PROCEDURE.              
                           
                           
PROCEDURE Prolint_Status_FileStart :
  /* purpose: Prolint notifies you it starts on a new sourcefile. You may use this as an 
              opportunity to open a new table in htm */
  DEFINE INPUT PARAMETER pSourceFile AS CHAR NO-UNDO.

   OUTPUT TO VALUE (logfile) APPEND.
   
   PUT UNFORMATTED SUBSTITUTE ("<h2>&1</h2><table>":U,pSourceFile)
                   SKIP.

   PUT UNFORMATTED SUBSTITUTE ("<tr class=""thead""><td>&1</td><td>&2</td><td>&3</td><td>&4</td><td>&5</td></tr>":U, 
                               "sourcefile":U, 
                               "line":U, 
                               "description":U, 
                               "rule":U, 
                               "severity":U)
                   SKIP.            

   OUTPUT CLOSE.                           
   oddrow = FALSE.

END PROCEDURE.
                           
   
PROCEDURE Prolint_AddResult :              
   /* purpose: add one result from a 'rule' to the logfile, 
               using the format of your choice.
               The format in this example looks pretty useless to me */
   {&_proparse_ prolint-nowarn(varusage)}            
   DEFINE INPUT PARAMETER pCompilationUnit  AS CHAR    NO-UNDO.  /* the sourcefile we're parsing          */
   DEFINE INPUT PARAMETER pSourcefile       AS CHAR    NO-UNDO.  /* may be an includefile                 */
   DEFINE INPUT PARAMETER pLineNumber       AS INTEGER NO-UNDO.  /* line number in pSourceFile            */
   DEFINE INPUT PARAMETER pDescription      AS CHAR    NO-UNDO.  /* human-readable hint                   */
   DEFINE INPUT PARAMETER pRuleID           AS CHAR    NO-UNDO.  /* defines rule-program and maps to help */
   DEFINE INPUT PARAMETER pSeverity         AS INTEGER NO-UNDO.  /* importance of this rule, scale 0-9    */
   
   OUTPUT TO VALUE (logfile) APPEND.
   PUT UNFORMATTED SUBSTITUTE ("<tr class=""&6""><td>&1</td><td>&2</td><td>&3</td><td><a href=""&7"">&4</a></td><td>&5</td></tr>":U, 
                               pSourceFile, 
                               STRING(pLineNumber), 
                               REPLACE(REPLACE(pDescription,"<":U,"&lt;":U),">":U,"&gt;":U),
                               pRuleID, 
                               STRING(pSeverity),
                               IF oddrow THEN "odd":U ELSE "even":U,
                               helpfile(pRuleID))
                   SKIP.
   OUTPUT CLOSE.
   oddrow = NOT oddrow.
END PROCEDURE.

PROCEDURE Prolint_Status_FileEnd :
  /* purpose: Prolint notifies you when it's done linting a sourcefile. You may use this as an 
              opportunity to close the table in htm or print some totals, etc */

   OUTPUT TO VALUE (logfile) APPEND.
   PUT UNFORMATTED "</table>":U
                   SKIP.
   OUTPUT CLOSE.

END PROCEDURE.

   
PROCEDURE Prolint_FinalizeResults :                                    
   /* purpose: close the logfile and/or show it. Free resources  */
   
   /* don't use closing tags like </body></html> so you can append to it later. 
      most browsers don't care much about these closing tags */

   DEFINE VARIABLE fullname AS CHARACTER NO-UNDO.
   FILE-INFO:FILE-NAME = logfile.
   fullname = FILE-INFO:FULL-PATHNAME.
   IF fullname NE ? THEN
      RUN prolint/core/openhtml.p (fullname).

   /* This procedure will not be invoked again, so it can exit */
   DELETE PROCEDURE THIS-PROCEDURE.                          
   
END PROCEDURE.


