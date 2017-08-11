/* =======================================================================================
    file    : prolint/outputhandlers/prolintdbsess.p
    purpose : write results (found by rules) to a session-aware database
    note    : create the database with prolint/prolintdb/prolintdb.df
    by      : Glen West
    -----------------------------------------------------------------

    Copyright (C) 2002,2007 Jurjen Dijkstra, Glen West

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

DEFINE VARIABLE cRuleList      AS CHARACTER NO-UNDO.
DEFINE VARIABLE dToday         AS DATE      NO-UNDO.
DEFINE VARIABLE iSessionID     AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSessionName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSessionExists AS LOGICAL   NO-UNDO INITIAL FALSE.

ASSIGN dToday = TODAY.

SUBSCRIBE TO "Prolint_List_Rules" ANYWHERE.
SUBSCRIBE TO "Prolint_Status_FileStart" ANYWHERE.
SUBSCRIBE TO "Prolint_AddResult" ANYWHERE.
SUBSCRIBE TO "Prolint_FinalizeResults" ANYWHERE.

/* as we are run persistently by prolint.p, we only have a single session-context. */
/* so get our session ID and name */
ASSIGN iSessionID   = INTEGER(DYNAMIC-FUNCTION ("ProlintProperty","SessionID"))
       cSessionName = DYNAMIC-FUNCTION ("ProlintProperty","SessionName").
       
IF iSessionID = 0 OR iSessionID = ? THEN DO:
    ASSIGN 
       iSessionID = 0
       cSessionName = "".
END.
ASSIGN lSessionExists = CAN-FIND(lint_session NO-LOCK WHERE lint_session.sessionid = iSessionID).
   
RETURN.


PROCEDURE Prolint_List_Rules :
  /* purpose: Prolint tells you which rules are selected */
  DEFINE INPUT PARAMETER pRuleList AS CHARACTER NO-UNDO.  /* comma-separated list of ruleid */

  cRuleList = TRIM(pRuleList + ",compiler,prolint,proparse":U, ",":U).

END PROCEDURE.

PROCEDURE Prolint_Status_FileStart :
  /* purpose: Prolint notifies you when it starts on a new sourcefile.
              remove old warnings from database.
              keep warnings from rules that are not selected. */
  DEFINE INPUT PARAMETER pSourceFile AS CHAR NO-UNDO.

  DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cRuleId AS CHARACTER NO-UNDO.
  
  DO TRANSACTION:
      /* create the session record if it does not yet exist */
      IF iSessionId > 0 AND NOT lSessionExists THEN DO:
         CREATE lint_session. 
         ASSIGN lint_session.sessionid    = iSessionID
            lint_session.sessionuser      = USERID("prolintdb")
            lint_session.sessionbox       = ''
            lint_session.sessionrules     = cRuleList
            lint_session.sessionparms     = ''
            lint_session.sessionname      = cSessionName
            lint_session.sessionStartDate = TODAY 
            lint_session.sessionStartTime = TIME 
            lint_session.sessionLastDate  = TODAY 
            lint_session.sessionLastTime  = TIME
            lSessionExists                = TRUE.
      END.

      DO i=1 TO NUM-ENTRIES(cRuleList) :
         cRuleId = ENTRY(i, cRuleList).
         FOR EACH prolintdb.lint_warning EXCLUSIVE-LOCK
                                         WHERE prolintdb.lint_warning.sessionID = iSessionID
                                           AND prolintdb.lint_warning.ruleId = cRuleId
                                           AND prolintdb.lint_warning.compunit = pSourceFile :
             DELETE prolintdb.lint_warning.
         END.
      END.
            
  END. /* transaction */

END PROCEDURE.
                           
   
PROCEDURE Prolint_AddResult :              
   /* purpose: add one result from a 'rule' to the database */
   DEFINE INPUT PARAMETER pCompilationUnit  AS CHAR    NO-UNDO.  /* the sourcefile we're parsing          */
   DEFINE INPUT PARAMETER pSourcefile       AS CHAR    NO-UNDO.  /* may be an includefile                 */
   DEFINE INPUT PARAMETER pLineNumber       AS INTEGER NO-UNDO.  /* line number in pSourceFile            */
   DEFINE INPUT PARAMETER pDescription      AS CHAR    NO-UNDO.  /* human-readable hint                   */
   DEFINE INPUT PARAMETER pRuleID           AS CHAR    NO-UNDO.  /* defines rule-program and maps to help */
   DEFINE INPUT PARAMETER pSeverity         AS INTEGER NO-UNDO.  /* importance of this rule, scale 0-9    */

   DEFINE VARIABLE cSubdir AS CHARACTER NO-UNDO.
   DEFINE VARIABLE iSlash  AS INTEGER   NO-UNDO.

   /* remove leading ./ from sourcefile. this helps selecting distinct warnings for includefiles */
   IF SUBSTRING(pSourcefile,1,2)="./":U THEN
      pSourceFile = SUBSTRING(pSourcefile,3).

   /* same for comp.unit, because you can identify an includefile only if comp.unit<>sourcefile */
   IF SUBSTRING(pCompilationUnit,1,2)="./":U THEN
      pCompilationUnit = SUBSTRING(pCompilationUnit,3).

   iSlash = R-INDEX(pSourcefile, "/":U).
   IF iSlash < 2 THEN
      cSubdir = ".":U.
   ELSE
      cSubdir = SUBSTRING(pSourcefile, 1, iSlash - 1).
   IF SUBSTRING(cSubdir,1,2)="./":U THEN
      cSubdir = SUBSTRING(cSubdir,3).

   DO TRANSACTION:
      CREATE prolintdb.lint_warning.
      ASSIGN prolintdb.lint_warning.compunit   = pCompilationUnit
             prolintdb.lint_warning.sourcefile = pSourcefile
             prolintdb.lint_warning.linenumber = pLineNumber
             prolintdb.lint_warning.comment    = pDescription
             prolintdb.lint_warning.ruleid     = pRuleId
             prolintdb.lint_warning.severity   = pSeverity
             prolintdb.lint_warning.lintdate   = dToday
             prolintdb.lint_warning.subdir     = cSubdir
             prolintdb.lint_warning.sessionid  = iSessionID.
   END.

END PROCEDURE.


PROCEDURE Prolint_FinalizeResults :                                    

   IF iSessionId > 0 THEN 
       DO TRANSACTION:
          FIND lint_session EXCLUSIVE-LOCK 
               WHERE lint_session.sessionid    = iSessionID NO-ERROR.
          IF AVAILABLE lint_session THEN ASSIGN
             lint_session.sessionLastDate  = TODAY 
             lint_session.sessionLastTime  = TIME.
       END.
   
   /* This procedure will not be invoked again, so it can exit */
   DELETE PROCEDURE THIS-PROCEDURE.                          
   
END PROCEDURE.


