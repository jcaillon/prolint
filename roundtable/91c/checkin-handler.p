/* =======================================================================================
   file    : prolint/roundtable/91c/checkin-handler.p
   purpose : outputhandler, launched when checking-in objects in RTB.
             This is a very weird outputhandler! It only works if managed by
             prolint/roundtable/91c/checkin-event.p
   by      : Jurjen Dijkstra
    -----------------------------------------------------------------

    Copyright (C) 2003 Jurjen Dijkstra

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
{rtb/g/rtbglobl.i}


DEFINE TEMP-TABLE tt_lint NO-UNDO
   FIELD ttDescription AS CHAR
   FIELD ttSeverity    AS INTEGER
   FIELD ttCompUnit    AS CHAR
   FIELD ttSource      AS CHAR
   FIELD ttLine        AS INTEGER
   FIELD ttRuleID      AS CHAR
   FIELD ttError       AS LOGICAL
   INDEX tt1 AS PRIMARY ttCompUnit ttSource ttLine.

DEFINE TEMP-TABLE tt_includefiles NO-UNDO
   FIELD sourcefile AS CHARACTER
   FIELD mine       AS LOGICAL    /* TRUE if you are somehow responsible for this file */
   INDEX idx1 AS PRIMARY UNIQUE sourcefile.

DEFINE TEMP-TABLE tt_ini NO-UNDO
   FIELD fSection AS CHARACTER
   FIELD fKey     AS CHARACTER
   FIELD fValue   AS CHARACTER
   INDEX idx1 AS PRIMARY UNIQUE fSection fKey.

DEFINE VARIABLE TaskNumber            AS INTEGER NO-UNDO.
DEFINE VARIABLE CurrentProfile        AS CHARACTER NO-UNDO.
DEFINE VARIABLE IncludeFileToWatchFor AS CHARACTER NO-UNDO.
DEFINE VARIABLE ModuleToWatchFor      AS CHARACTER NO-UNDO.
DEFINE VARIABLE my-inc                AS CHARACTER NO-UNDO INITIAL "programmer,manager,compltd-by":U.

SUBSCRIBE TO "Prolint_AddResult" ANYWHERE.
SUBSCRIBE TO "Prolint_Status_Profile" ANYWHERE.
   
RETURN.


FUNCTION  IsModuleFile RETURNS LOGICAL (sourcefile AS CHARACTER) :
/* --------------------------------------------------------------------
   purpose : determine if sourcefile belongs to ModuleToWatchFor
   -------------------------------------------------------------------- */
   DEFINE VARIABLE v-object AS CHARACTER NO-UNDO.
   DEFINE BUFFER bobject FOR rtb.rtb_object.

   IF R-INDEX(sourcefile, '/':U)=0 THEN
      v-object = sourcefile.
   ELSE
      v-object = SUBSTRING(sourcefile, 1 + R-INDEX(sourcefile, '/':U)).

   /* find object and module.
      added wspace-id and obj-type to WHERE clause for fast indexing */
   FIND FIRST bobject NO-LOCK
                      WHERE bobject.wspace-id = grtb-wspace-id
                        AND bobject.obj-type  = "PCODE":U
                        AND bobject.OBJECT    = v-object
                        AND bobject.module    = ModuleToWatchFor
                      NO-ERROR.
   RETURN AVAILABLE bobject.

END FUNCTION.


FUNCTION  IsTaskFile RETURNS LOGICAL (sourcefile AS CHARACTER) :
/* --------------------------------------------------------------------
   purpose : determine if sourcefile belongs to selected task
   -------------------------------------------------------------------- */
   DEFINE VARIABLE v-object AS CHARACTER NO-UNDO.
   DEFINE BUFFER bver FOR rtb.rtb_ver.

   IF R-INDEX(sourcefile, '/':U)=0 THEN
      v-object = sourcefile.
   ELSE
      v-object = SUBSTRING(sourcefile, 1 + R-INDEX(sourcefile, '/':U)).

   FIND FIRST bver NO-LOCK
                   WHERE bver.task-num = TaskNumber
                     AND bver.OBJECT   = v-object
                   NO-ERROR.
   RETURN AVAILABLE bver.

END FUNCTION.


FUNCTION ListsMatch RETURNS LOGICAL (shortlist AS CHARACTER, longlist AS CHARACTER) :
/* --------------------------------------------------------------------------
   purpose : test if any entry of shortlist also appears in longlist.
             iow test if any of the programmers is responsible (see IsMyFile)
   -------------------------------------------------------------------------- */
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DO i=1 TO NUM-ENTRIES(shortlist,';':U) :
      IF LOOKUP(ENTRY(i,shortlist,';':U),longlist,';':U)>0 THEN
         RETURN TRUE.
   END.
   RETURN FALSE.
END FUNCTION.


FUNCTION  IsMyFile RETURNS LOGICAL (sourcefile AS CHARACTER) :
/* --------------------------------------------------------------------
   purpose : determine if you are responsible for (or owner of) this
             sourcefile.
             Returns YES if the programmer of *this* task has ever been
             involved with this sourcefile before.
   -------------------------------------------------------------------- */
   DEFINE VARIABLE v-object      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE myfile        AS LOGICAL   NO-UNDO INITIAL FALSE.
   DEFINE VARIABLE v-programmers AS CHARACTER NO-UNDO.
   DEFINE VARIABLE v-responsible AS CHARACTER NO-UNDO.
   DEFINE VARIABLE i             AS INTEGER NO-UNDO.

   /* look in the cache */
   FIND tt_includefiles WHERE tt_includefiles.sourcefile = tt_lint.ttSource NO-ERROR.
   IF AVAILABLE tt_includefiles THEN
      RETURN tt_includefiles.mine.

   FIND rtb_task WHERE rtb_task.task-num = TaskNumber NO-LOCK NO-ERROR.
   v-programmers = rtb_task.programmer.  /* ';'-separated list */
      
   FOR EACH tt_ini WHERE tt_ini.fSection="wildcards:my-includes":U
                     AND sourcefile MATCHES tt_ini.fKey :
       DO i=1 TO NUM-ENTRIES(v-programmers, ";":U) :
          IF CAN-DO(tt_ini.fValue, ENTRY(i,v-programmers,";":U)) THEN
             RETURN TRUE.
       END.
   END.

   FOR EACH tt_ini WHERE tt_ini.fSection="wildcards:not-my-includes":U
                     AND sourcefile MATCHES tt_ini.fKey :
       DO i=1 TO NUM-ENTRIES(v-programmers, ";":U) :
          IF CAN-DO(tt_ini.fValue, ENTRY(i,v-programmers,";":U)) THEN
             RETURN FALSE.
       END.
   END.
   
   /* split object from relative path */
   IF R-INDEX(sourcefile, '/':U)=0 THEN
      v-object = sourcefile.
   ELSE
      v-object = SUBSTRING(sourcefile, 1 + R-INDEX(sourcefile, '/':U)).

   /* browse history of object. See if you have been involved */
   FIND FIRST rtb_ver WHERE rtb_ver.obj-type="PCODE":U AND rtb_ver.object = v-object NO-LOCK NO-ERROR.
   DO WHILE AVAILABLE rtb_ver AND  NOT myfile :
       FIND rtb_task WHERE rtb_task.task-num = rtb_ver.task-num NO-LOCK NO-ERROR.
       IF AVAILABLE rtb_task THEN DO:
           IF LOOKUP("programmer":U,my-inc)>0 THEN
              v-responsible = rtb_task.programmer.
           IF LOOKUP("manager":U,my-inc)>0 THEN
              v-responsible = v-responsible + ";":U + rtb_task.manager.
           IF LOOKUP("compltd-by":U,my-inc)>0 THEN
              v-responsible = v-responsible + ";":U + rtb_task.compltd-by.
           v-responsible = TRIM(v-responsible,";":U).
           IF ListsMatch(v-programmers,v-responsible) THEN myfile = TRUE.
       END.
       FIND NEXT rtb_ver WHERE rtb_ver.obj-type="PCODE":U AND rtb_ver.object = v-object NO-LOCK NO-ERROR.
   END.

   CREATE tt_includefiles.
   ASSIGN tt_includefiles.sourcefile = tt_lint.ttSource
          tt_includefiles.mine       = myfile.
   
   RETURN myfile.

END FUNCTION.


PROCEDURE SetConfiguration :
/* --------------------------------------------------------------------
   purpose : receive contents from INI file.
             This file has already been parsed by checkin-event.p
   -------------------------------------------------------------------- */
   DEFINE INPUT PARAMETER TABLE FOR tt_ini.

   FIND tt_ini WHERE tt_ini.fSection="behaviour":U AND tt_ini.fKey="my-inc":U NO-ERROR.
   IF AVAILABLE tt_ini THEN
      my-inc = tt_ini.fValue.

   FIND tt_ini WHERE tt_ini.fSection="runtime":U AND tt_ini.fKey="tasknumber":U.
   TaskNumber = INTEGER(tt_ini.fValue).

END PROCEDURE.


PROCEDURE WatchForIncludefile :
/* --------------------------------------------------------------------
   purpose : allow for special processing of compilation-units that you are
             linting to validate a specific includefile.
   -------------------------------------------------------------------- */
   DEFINE INPUT PARAMETER pIncludefile AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER pModule      AS CHARACTER NO-UNDO.

   ASSIGN
      IncludeFileToWatchFor = pIncludefile
      ModuleToWatchFor      = pModule.

   IF IncludeFileToWatchFor <> '*':U THEN DO:
      /* we can already cache this one */
      FIND tt_includefiles WHERE tt_includefiles.sourcefile = IncludeFileToWatchFor NO-ERROR.
      IF NOT AVAILABLE tt_includefiles THEN DO:
         CREATE tt_includefiles.
         ASSIGN tt_includefiles.sourcefile = IncludeFileToWatchFor.
      END.
      ASSIGN tt_includefiles.mine = TRUE.
   END.

END PROCEDURE.

PROCEDURE Prolint_Status_Profile :
/*------------------------------------------------------------------------------
  Purpose:     remember profile, only to pass it on to logwin.w
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pProfile AS CHAR NO-UNDO.
                                                
  ASSIGN 
     CurrentProfile = pProfile.                                              

END PROCEDURE.


PROCEDURE Prolint_AddResult :              
/*------------------------------------------------------------------------------
  Purpose:     create a new record tt_lint
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pCompilationUnit  AS CHAR    NO-UNDO.  /* the sourcefile we're parsing          */
  DEFINE INPUT PARAMETER pSourcefile       AS CHAR    NO-UNDO.  /* may be an includefile                 */
  DEFINE INPUT PARAMETER pLineNumber       AS INTEGER NO-UNDO.  /* line number in pSourceFile            */
  DEFINE INPUT PARAMETER pDescription      AS CHAR    NO-UNDO.  /* human-readable hint                   */
  DEFINE INPUT PARAMETER pRuleID           AS CHAR    NO-UNDO.  /* defines rule-program and maps to help */
  DEFINE INPUT PARAMETER pSeverity         AS INTEGER NO-UNDO.  /* importance of this rule, scale 0-9    */

  DEFINE VARIABLE IsError AS LOGICAL NO-UNDO.


  IF (IncludeFileToWatchFor='*':U) OR (IncludeFileToWatchFor=pSourcefile) THEN DO:

      /* is this an error or just a warning? */
      FIND tt_ini WHERE tt_ini.fSection="error-rules":U AND tt_ini.fKey=pRuleId NO-ERROR.
      IF NOT AVAILABLE tt_ini THEN
         IsError = FALSE.
      ELSE DO:
         IF pCompilationUnit = pSourceFile THEN
            IsError = TRUE.
         ELSE
            IF IncludeFileToWatchFor=pSourcefile THEN
               IsError = TRUE.
            ELSE DO:
               IsError = FALSE.
               IF LOOKUP("all":U, tt_ini.fValue)>0 THEN
                  IsError = TRUE.
               IF IsError=FALSE AND LOOKUP("module":U, tt_ini.fValue)>0 THEN
                  IF IsModuleFile(pSourcefile) OR IsTaskFile(pSourcefile) THEN
                     IsError = TRUE.
               IF IsError=FALSE AND LOOKUP("my":U, tt_ini.fValue)>0 THEN
                  IF IsMyFile(pSourcefile) THEN
                     IsError = TRUE.
            END.
      END.

      CREATE tt_lint.
      ASSIGN tt_lint.ttCompUnit    = pCompilationUnit
             tt_lint.ttSource      = pSourcefile
             tt_lint.ttLine        = pLineNumber
             tt_lint.ttDescription = IF IsError THEN 'ERROR: ':U + pDescription ELSE pDescription
             tt_lint.ttruleID      = pRuleID
             tt_lint.ttSeverity    = pSeverity
             tt_lint.ttError       = IsError.

  END.

END PROCEDURE.


PROCEDURE JudgeResults :
/* --------------------------------------------------------------------
   purpose : tell checkin-event if you found any errors.
             show these errors (and warnings) in logwin.w
   -------------------------------------------------------------------- */
   DEFINE OUTPUT PARAMETER lintresult AS INTEGER NO-UNDO.

  DEFINE VARIABLE LogwinRunning AS LOGICAL NO-UNDO.
  DEFINE VARIABLE hw AS HANDLE NO-UNDO.
  DEFINE VARIABLE prevFile AS CHARACTER NO-UNDO.

  UNSUBSCRIBE TO ALL.

  /* launch logwin if it is not already running */
  LogwinRunning = FALSE.
  hw = SESSION:FIRST-CHILD.
  DO WHILE VALID-HANDLE(hw) AND LogwinRunning=FALSE :
     IF hw:PRIVATE-DATA = "prolint_outputhandler_logwin.w":U THEN
        LogwinRunning = TRUE.
     ELSE
        hw = hw:NEXT-SIBLING.
  END.
  IF NOT LogwinRunning THEN
     RUN VALUE(DYNAMIC-FUNCTION("ProlintProperty", "outputhandlers.resultwindow")) PERSISTENT.

  /* show all messages in the logwin */
  PUBLISH "Prolint_Status_Profile" ("CurrentProfile").
  PUBLISH "Prolint_InitializeResults" (TRUE).

  FOR EACH tt_lint :

       IF prevFile <> tt_lint.ttCompUnit THEN DO:
          prevFile = tt_lint.ttCompUnit.
          PUBLISH "Prolint_Status_FileStart" (prevFile).
       END.

       PUBLISH "Prolint_AddResult":U (tt_lint.ttCompUnit,
                                      tt_lint.ttSource,
                                      tt_lint.ttLine,
                                      tt_lint.ttDescription,
                                      tt_lint.ttRuleID,
                                      tt_lint.ttSeverity).

  END.

  PUBLISH "Prolint_FinalizeResults".
   

  /* finally determine the return value.
      0 = no warnings or errors found
      1 = warnings found but no errors
      2 = errors found
   */
   IF NOT CAN-FIND (FIRST tt_lint) THEN DO:
      lintresult = 0.
      RETURN.
   END.

   IF CAN-FIND (FIRST tt_lint WHERE tt_lint.ttError) THEN DO:
      lintresult = 2.
      RETURN.
   END.

   lintresult = 1.

END PROCEDURE.

