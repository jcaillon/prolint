/* =======================================================================================
    file    : prolint/prolintdb/recalcstats.p
    purpose : delete all statistics and calculate new statistics
    note    : create the database with prolint/prolintdb/prolintdb.df
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

   DEFINE VARIABLE DoNotRun    AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE prevdir     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prevcount   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE prevscore   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE maxseverity AS INTEGER   NO-UNDO.
   
   DEFINE TEMP-TABLE ttRule NO-UNDO     FIELD ruleid      AS CHARACTER
     FIELD numwarnings AS INTEGER 
     FIELD severity    AS INTEGER 
     INDEX KEY AS PRIMARY UNIQUE ruleid.
     
   DEFINE TEMP-TABLE ttRuleDir NO-UNDO
     FIELD ruleid      AS CHARACTER
     FIELD subdir      AS CHARACTER 
     FIELD numwarnings AS INTEGER 
     FIELD severity    AS INTEGER 
     FIELD score       AS INTEGER 
     INDEX KEY AS PRIMARY UNIQUE 
       ruleid 
       subdir.
       
   DEFINE TEMP-TABLE ttInc NO-UNDO
     FIELD sourcefile       AS CHARACTER     FIELD ruleid           AS CHARACTER     FIELD numwarnings      AS INTEGER 
     FIELD distinctwarnings AS INTEGER     FIELD score            AS INTEGER     FIELD scoredistinct    AS DECIMAL     INDEX KEY AS PRIMARY UNIQUE       sourcefile
       ruleid.
   
   DEFINE TEMP-TABLE ttSubdir NO-UNDO
     FIELD subdir      AS CHARACTER 
     FIELD numwarnings AS INTEGER 
     FIELD severity    AS INTEGER 
     FIELD score       AS INTEGER 
     INDEX KEY AS PRIMARY UNIQUE 
       subdir.
       
   DEFINE TEMP-TABLE ttDistinct NO-UNDO     FIELD sourcefile  AS CHARACTER     FIELD ruleid      AS CHARACTER
     FIELD linenum     AS INTEGER     FIELD ruledesc    AS CHARACTER /* receives 'comment', apparenlty older versions 
                                       can get different comments for same rule/line? */
     INDEX KEY AS PRIMARY UNIQUE       sourcefile
       ruleid
       linenum
       ruledesc.
       
SUBSCRIBE TO "Prolint_Stat_Rebuild" ANYWHERE
  RUN-PROCEDURE "rebuild-all".
SUBSCRIBE TO "Prolint_Stat_Details" ANYWHERE
  RUN-PROCEDURE "publish-detail".
SUBSCRIBE TO "Prolint_Stat_Distinct" ANYWHERE
  RUN-PROCEDURE "publish-distinct".
SUBSCRIBE TO "Prolint_Stat_PruneInc" ANYWHERE
  RUN-PROCEDURE "purge-inc-rule".
  
PUBLISH "Prolint_Stat_Heartbeat" (OUTPUT DoNotRun).
IF DoNotRun THEN RUN DestroyMe IN THIS-PROCEDURE.

SUBSCRIBE TO "Prolint_Stat_Heartbeat" ANYWHERE.
SUBSCRIBE TO "Prolint_Stat_Shutdown"  ANYWHERE
  RUN-PROCEDURE "DestroyMe".
   
   
PROCEDURE rebuild-all:
   DEFINE OUTPUT PARAMETER po_success AS LOGICAL INITIAL TRUE.
   RUN set-hourglass (TRUE).
   RUN clear-tables  IN THIS-PROCEDURE.
   RUN build-stats   IN THIS-PROCEDURE.
   RUN publish-stats IN THIS-PROCEDURE.
   RUN set-hourglass (FALSE).
END PROCEDURE. /* rebuild all */

PROCEDURE set-hourglass :
/*------------------------------------------------------------------------------
       Purpose:     Change the cursor to reflect the process state
       Parameters:  Logical - Yes/True = Wait
       Notes:
------------------------------------------------------------------------------*/
   define input parameter MUST-WAIT as LOGICAL no-undo.
   /* Set the cursor */
   if MUST-WAIT then SESSION:set-wait-state("GENERAL":U). /* Hourglass */
                else SESSION:set-wait-state("").        /* Arrow */
END PROCEDURE. /* set-hourglass */

PROCEDURE clear-tables:
    DEFINE VARIABLE l-count AS INTEGER    NO-UNDO.
    
    TRXLOOP: DO WHILE TRUE TRANSACTION:
      FOR EACH prolintdb.lint_stat_rule EXCLUSIVE-LOCK :
         DELETE prolintdb.lint_stat_rule.
         ASSIGN l-count = l-count + 1.
         IF l-count >= 100 THEN NEXT TRXLOOP.
      END.

      FOR EACH prolintdb.lint_stat_subdir EXCLUSIVE-LOCK :
         DELETE prolintdb.lint_stat_subdir.
         ASSIGN l-count = l-count + 1.
         IF l-count >= 100 THEN NEXT TRXLOOP.
      END.

      FOR EACH prolintdb.lint_stat_ruledir EXCLUSIVE-LOCK :
         DELETE prolintdb.lint_stat_ruledir.
         ASSIGN l-count = l-count + 1.
         IF l-count >= 100 THEN NEXT TRXLOOP.
      END.

      FOR EACH prolintdb.lint_stat_inc EXCLUSIVE-LOCK :
         DELETE prolintdb.lint_stat_inc.
         ASSIGN l-count = l-count + 1.
         IF l-count >= 100 THEN NEXT TRXLOOP.
      END.
      LEAVE trxloop.
    END.
END PROCEDURE. /* clear-tables */

PROCEDURE build-stats:
    DEFINE QUERY qWarnings FOR prolintdb.lint_warning.
    DEFINE VARIABLE loopcount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE hQuery    AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hTable    AS HANDLE     NO-UNDO.
    
    ASSIGN hQuery = QUERY qWarnings:HANDLE.
    CREATE BUFFER hTable FOR TABLE "lint_session" NO-ERROR.
    
    OPEN QUERY qWarnings FOR EACH prolintdb.lint_warning NO-LOCK.
    IF VALID-HANDLE(hTable) THEN DO:
       hQuery:QUERY-PREPARE("for each prolintdb.lint_warning no-lock "
                            + " where prolintdb.lint_warning.sessionid = 0").
       hQuery:QUERY-OPEN().
    END.
    
  QUERYLOOP: DO WHILE TRUE:
    GET NEXT qWarnings.
    IF QUERY-OFF-END("qWarnings") THEN LEAVE QUERYLOOP.
    ASSIGN loopcount = loopcount + 1.

    /* start with rule */
    FIND ttRule WHERE ttRule.ruleid = prolintdb.lint_warning.ruleid NO-ERROR.
    IF NOT AVAILABLE ttRule THEN DO:
       CREATE ttRule.
       ASSIGN ttRule.ruleid = prolintdb.lint_warning.ruleid.
    END.
    ASSIGN ttRule.numwarnings = ttRule.numwarnings + 1
           ttRule.severity    = MAXIMUM(ttRule.severity,prolintdb.lint_warning.severity).
           
    /* now with subdir */
    FIND ttSubDir WHERE ttSubDir.subdir = prolintdb.lint_warning.subdir NO-ERROR.
    IF NOT AVAILABLE ttSubDir THEN DO:
       CREATE ttSubDir.
       ASSIGN ttSubDir.subdir = prolintdb.lint_warning.subdir.
    END.
    ASSIGN ttSubDir.numwarnings = ttSubDir.numwarnings + 1
           ttSubDir.severity    = MAXIMUM(ttRule.severity,prolintdb.lint_warning.severity)
           ttSubDir.score       = ttSubDir.Score + prolintdb.lint_warning.severity.

    /* now with rule + subdir */
    FIND ttRuleDir WHERE ttRuleDir.ruleid = prolintdb.lint_warning.ruleid
                     AND ttRuleDir.subdir = prolintdb.lint_warning.subdir NO-ERROR.
    IF NOT AVAILABLE ttRuleDir THEN DO:
       CREATE ttRuleDir.
       ASSIGN ttRuleDir.ruleid = prolintdb.lint_warning.ruleid
              ttRuleDir.subdir = prolintdb.lint_warning.subdir.
    END.
    ASSIGN ttRuleDir.numwarnings = ttRuleDir.numwarnings + 1
           ttRuleDir.severity    = MAXIMUM(ttRuleDir.severity,prolintdb.lint_warning.severity)
           ttRuleDir.score       = ttRuleDir.Score + prolintdb.lint_warning.severity.
           
    /* now each include file */ 
    IF prolintdb.lint_warning.compunit <> prolintdb.lint_warning.sourcefile THEN DO:
       FIND ttInc WHERE ttInc.sourcefile = prolintdb.lint_warning.sourcefile
                    AND ttInc.ruleid     = prolintdb.lint_warning.ruleid
                  NO-ERROR.
       IF NOT AVAILABLE ttInc THEN DO:
          CREATE ttInc. 
          ASSIGN ttInc.sourcefile = prolintdb.lint_warning.sourcefile
                 ttInc.ruleid     = prolintdb.lint_warning.ruleid.
       END.
       ASSIGN ttInc.numwarnings = ttInc.numwarnings + 1
              ttInc.score       = ttInc.Score + prolintdb.lint_warning.severity.
       IF NOT CAN-FIND(ttDistinct WHERE ttDistinct.sourcefile = ttInc.sourcefile
                                    AND ttDistinct.ruleid     = ttInc.ruleid
                                    AND ttDistinct.linenum    = prolintdb.lint_warning.linenumber                                    AND ttDistinct.ruledesc   = prolintdb.lint_warning.comment) THEN DO:
          CREATE ttDistinct.
          ASSIGN ttDistinct.sourcefile = ttInc.sourcefile
                 ttDistinct.ruleid     = ttInc.ruleid
                 ttDistinct.linenum    = prolintdb.lint_warning.linenumber
                 ttDistinct.ruledesc   = prolintdb.lint_warning.comment.
        END. /* not yet recorded */
    END. /* special handling of include files */
    
    /* now be friendly with others */
    IF loopcount MODULO 1000 = 0 THEN PROCESS EVENTS.
  END. /* while true loop to get each lint_warning */
END PROCEDURE. /* build-stats */

PROCEDURE publish-stats:
    DEFINE VARIABLE l-count AS INTEGER    NO-UNDO.
    /* OK, let's sum up the 'distincts' for includes */
    
    DO TRANSACTION:
      FOR EACH ttRule:
        CREATE prolintdb.lint_stat_rule.
        BUFFER-COPY ttRule TO prolintdb.lint_stat_rule
                ASSIGN prolintdb.lint_stat_rule.score = ttRule.severity * ttRule.numwarnings. 
      END.
    END. /* transaction */  
    PROCESS EVENTS.

    DO TRANSACTION:
      FOR EACH ttRuleDir:
          CREATE prolintdb.lint_stat_ruledir.
          BUFFER-COPY ttRuleDir TO prolintdb.lint_stat_ruledir
            ASSIGN prolintdb.lint_stat_ruledir.score = ttRuleDir.score / ttRuledir.numwarnings.
      END.
    END. /* transaction */  
    PROCESS EVENTS.

    DO TRANSACTION:
      FOR EACH ttinc:
          /* now get 'distinct' stats */
          l-count = 0.
          FOR EACH ttDistinct WHERE ttDistinct.sourcefile = ttInc.sourcefile
                                AND ttDistinct.ruleid     = ttInc.ruleid:
            l-count = l-count + 1.
          END.
          IF l-count = 0 THEN ASSIGN l-count = 1. /* avoid /0 problems */
          CREATE prolintdb.lint_stat_inc.
          BUFFER-COPY ttinc TO prolintdb.lint_stat_inc
            ASSIGN prolintdb.lint_stat_inc.score            = ttInc.score 
/*                                                            / ttInc.numwarnings*/
                   prolintdb.lint_stat_inc.distinctwarnings = l-count
                   prolintdb.lint_stat_inc.scoredistinct    = prolintdb.lint_stat_inc.score
                                                            / l-count.
      END.
    END. /* transaction */  
    PROCESS EVENTS.

    DO TRANSACTION:
      FOR EACH ttSubDir:
          CREATE prolintdb.lint_stat_subdir.
          BUFFER-COPY ttSubDir TO prolintdb.lint_stat_subdir
            ASSIGN prolintdb.lint_stat_SubDir.score = ttSubDir.score / ttSubDir.numwarnings.
      END.
    END. /* transaction */    
    PROCESS EVENTS.
    
    EMPTY TEMP-TABLE ttInc.
    EMPTY TEMP-TABLE ttRule.
    EMPTY TEMP-TABLE ttRuleDir.
    EMPTY TEMP-TABLE ttSubDir.
    EMPTY TEMP-TABLE ttDistinct.
    
END PROCEDURE. /* publish-stats */

PROCEDURE publish-detail:
    DEFINE INPUT PARAMETER pi-rule AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pi-sub  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hQuery    AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hTable    AS HANDLE     NO-UNDO.
    DEFINE QUERY    qWarnings FOR prolintdb.lint_warning.
  
  PUBLISH "Prolint_Status_Profile" ("prolintdb").
  PUBLISH "Prolint_InitializeResults" (TRUE).

  ASSIGN hQuery = QUERY qWarnings:HANDLE.
  CREATE BUFFER hTable FOR TABLE "lint_session" NO-ERROR.
    
  OPEN QUERY qWarnings FOR EACH prolintdb.lint_warning NO-LOCK
                          WHERE prolintdb.lint_warning.ruleid = pi-rule 
                            AND prolintdb.lint_warning.subdir = pi-sub.
  IF VALID-HANDLE(hTable) THEN DO:
     hQuery:QUERY-PREPARE("for each prolintdb.lint_warning no-lock "
                          + " where prolintdb.lint_warning.sessionid = 0 "
                          + " and prolintdb.lint_warning.ruleid = '" + pi-rule 
                          + "' and prolintdb.lint_warning.subdir = '" + pi-sub + "'").
     hQuery:QUERY-OPEN().
  END.
    
  QUERYLOOP: DO WHILE TRUE:
    GET NEXT qWarnings.
    IF QUERY-OFF-END("qWarnings") THEN LEAVE QUERYLOOP.
    PUBLISH "Prolint_AddResult":U (prolintdb.lint_warning.compunit,
                                   prolintdb.lint_warning.sourcefile,
                                   prolintdb.lint_warning.linenumber,
                                   prolintdb.lint_warning.comment,
                                   prolintdb.lint_warning.ruleid,
                                   prolintdb.lint_warning.severity).      
  END.

  PUBLISH "Prolint_FinalizeResults".

END PROCEDURE. /* publish-detail */

PROCEDURE publish-distinct:
    DEFINE INPUT PARAMETER pi-rule   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pi-source AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hQuery    AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hTable    AS HANDLE     NO-UNDO.
    DEFINE QUERY    qWarnings FOR prolintdb.lint_warning.
  
  PUBLISH "Prolint_Status_Profile" ("prolintdb").
  PUBLISH "Prolint_InitializeResults" (TRUE).

  ASSIGN hQuery = QUERY qWarnings:HANDLE.
  CREATE BUFFER hTable FOR TABLE "lint_session" NO-ERROR.
    
  OPEN QUERY qWarnings FOR EACH prolintdb.lint_warning NO-LOCK
                          WHERE prolintdb.lint_warning.ruleid     = pi-rule 
                            AND prolintdb.lint_warning.sourcefile = pi-source.
  IF VALID-HANDLE(hTable) THEN DO:
     hQuery:QUERY-PREPARE("for each prolintdb.lint_warning no-lock "
                          + " where prolintdb.lint_warning.sessionid = 0 "
                          + " and prolintdb.lint_warning.ruleid = '" + pi-rule 
                          + "' and prolintdb.lint_warning.sourcefile = '" + pi-source + "'").
     hQuery:QUERY-OPEN().
  END.
    
  QUERYLOOP: DO WHILE TRUE:
    GET NEXT qWarnings.
    IF QUERY-OFF-END("qWarnings") THEN LEAVE QUERYLOOP.
    IF NOT CAN-FIND(ttDistinct WHERE ttDistinct.sourcefile = prolintdb.lint_warning.sourcefile
                                 AND ttDistinct.linenum    = prolintdb.lint_warning.linenumber
                                 AND ttDistinct.ruleid     = prolintdb.lint_warning.ruleid
                                 AND ttDistinct.ruledesc   = prolintdb.lint_warning.comment) THEN DO:
      PUBLISH "Prolint_AddResult":U (prolintdb.lint_warning.compunit,
                                     prolintdb.lint_warning.sourcefile,
                                     prolintdb.lint_warning.linenumber,
                                     prolintdb.lint_warning.comment,
                                     prolintdb.lint_warning.ruleid,
                                     prolintdb.lint_warning.severity).
                                     
      CREATE ttDistinct.
      ASSIGN ttDistinct.sourcefile = prolintdb.lint_warning.sourcefile
             ttDistinct.ruleid     = prolintdb.lint_warning.ruleid
             ttDistinct.linenum    = prolintdb.lint_warning.linenumber
             ttDistinct.ruledesc   = prolintdb.lint_warning.comment.
    END. /* this is distinct */      
  END.
  
  EMPTY TEMP-TABLE ttDistinct.

  PUBLISH "Prolint_FinalizeResults".

END PROCEDURE. /* publish-distinct */

PROCEDURE purge-inc-rule:
    DEFINE INPUT PARAMETER pi-rule   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pi-source AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hQuery    AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hTable    AS HANDLE     NO-UNDO.
    DEFINE QUERY    qWarnings FOR prolintdb.lint_warning.
  
  ASSIGN hQuery = QUERY qWarnings:HANDLE.
  CREATE BUFFER hTable FOR TABLE "lint_session" NO-ERROR.
    
  OPEN QUERY qWarnings FOR EACH prolintdb.lint_warning EXCLUSIVE-LOCK
                          WHERE prolintdb.lint_warning.ruleid     = pi-rule 
                            AND prolintdb.lint_warning.sourcefile = pi-source.
  IF VALID-HANDLE(hTable) THEN DO:
     hQuery:QUERY-PREPARE("for each prolintdb.lint_warning exclusive-lock "
                          + " where prolintdb.lint_warning.sessionid = 0 "
                          + " and prolintdb.lint_warning.ruleid = '" + pi-rule 
                          + "' and prolintdb.lint_warning.sourcefile = '" + pi-source + "'").
     hQuery:QUERY-OPEN().
  END.
    
  QUERYLOOP: DO WHILE TRUE:
    GET NEXT qWarnings.
    IF QUERY-OFF-END("qWarnings") THEN LEAVE QUERYLOOP.
    DELETE prolintdb.lint_warning.
  END.
  
  FOR EACH prolintdb.lint_stat_inc WHERE prolintdb.lint_stat_inc.sourcefile = pi-source
                                     AND prolintdb.lint_stat_inc.ruleid     = pi-rule
                                   EXCLUSIVE-LOCK:
    ASSIGN prolintdb.lint_stat_inc.numwarnings      = 0
           prolintdb.lint_stat_inc.score            = 0
           prolintdb.lint_stat_inc.distinctwarnings = 0
           prolintdb.lint_stat_inc.scoredistinct    = 0.0.
  END. /* each inc/rule */
  
END PROCEDURE. /* publish-distinct */


PROCEDURE Prolint_Stat_Heartbeat:
    DEFINE OUTPUT PARAMETER po_alive AS LOGICAL INITIAL TRUE.
END.

PROCEDURE DestroyMe:
    DEFINE VARIABLE l-AnyoneCare AS LOGICAL NO-UNDO INITIAL ?.
    PUBLISH "Prolint_Stats_Subscribers" (OUTPUT l-AnyoneCare).
    
    IF l-AnyoneCare = ? THEN 
       DELETE PROCEDURE THIS-PROCEDURE.
END.