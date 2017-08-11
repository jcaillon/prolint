/*  =========================================================================
    file    : prolint/roundtable/101b/checkin-event.p
    purpose : validate objects on check-in or on task-complete
    
             Thomas Hansen, appSolutions 02/2008:
             Updated code to use RTB event model and variables from RTB 10.1B
    
    by      : Jurjen Dijkstra
    -------------------------------------------------------------------------

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
    ========================================================================= */

DEFINE INPUT PARAMETER pSimulation AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER pTaskNumber AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER rcObject    AS ROWID   NO-UNDO.
DEFINE OUTPUT PARAMETER Allowed    AS LOGICAL NO-UNDO INITIAL TRUE.

DEFINE VARIABLE gcRtbCurrentWorkspace  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE giRTBCurrentTaskNum    AS INTEGER    NO-UNDO.

DEFINE TEMP-TABLE tt_sourcefiles NO-UNDO
   FIELD CompUnit     AS CHARACTER
   FIELD Includefile  AS CHARACTER
   FIELD module       AS CHARACTER
   INDEX idx_id       AS PRIMARY UNIQUE CompUnit.

DEFINE TEMP-TABLE tt_xref NO-UNDO
   FIELD ref AS RECID.

DEFINE TEMP-TABLE tt_ini NO-UNDO
   FIELD fSection AS CHARACTER
   FIELD fKey     AS CHARACTER
   FIELD fValue   AS CHARACTER
   INDEX idx1 AS PRIMARY UNIQUE fSection fKey.

DEFINE VARIABLE vTaskNumber       AS INTEGER   NO-UNDO.
DEFINE VARIABLE outputhandler     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lintresult        AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE number-where-used AS INTEGER   NO-UNDO INITIAL 3.
DEFINE VARIABLE orig-propath      AS CHARACTER NO-UNDO.

/* don't do anything if ini file is missing */
IF DYNAMIC-FUNCTION("fnRtbFindProlintIniFile":U)=? THEN
   RETURN.
   
   PUBLISH "evRtbGetCurrentWorkspace":U (OUTPUT gcRtbCurrentWorkspace).
   PUBLISH "evRtbGetCurrentTask":U (OUTPUT giRTBCurrentTaskNum).

DEFINE VARIABLE filemasks AS CHARACTER NO-UNDO.
DEFINE VARIABLE propsrunning AS LOGICAL NO-UNDO INITIAL FALSE.
PUBLISH "IsProlintPropertiesRunning":U (OUTPUT propsrunning).
IF NOT propsrunning THEN
   RUN prolint/core/propsuper.p PERSISTENT.
RUN IncrementProlintPropertySubscribers.
filemasks = DYNAMIC-FUNCTION ("ProlintProperty", "compilationunit.filename.mask").
RUN DecrementProlintPropertySubscribers.

vTaskNumber = pTaskNumber.
RUN ReadConfiguration.

/* get a list of sourcefiles to lint */
IF pTaskNumber<>? THEN
   RUN GetTaskSourcefiles.

IF rcObject<>? THEN
   RUN GetObjectSourcefiles.

IF NOT CAN-FIND (FIRST tt_sourcefiles) THEN
   RETURN.

/* run Prolint for each compilation unit.
   logwin.w will be reset after each prolint run, only our special checkin-handler will
   accumulate all results. */

RUN prolint/roundtable/101b/checkin-handler.p PERSISTENT SET outputhandler.

CREATE tt_ini.
ASSIGN tt_ini.fSection = "runtime":U
       tt_ini.fKey     = "tasknumber":U
       tt_ini.fValue   = STRING(vTaskNumber).
RELEASE tt_ini.
RUN SetConfiguration IN outputhandler (INPUT TABLE tt_ini).

/* Roundtable changes the PROPATH during the OBJECT-CHECK-IN-BEFORE event.
   This may result in compile errors if prolint.p wants to compile an object.
   So we need to temporarily fix the propath. */
orig-propath = PROPATH.
FIND rtb.rtb_wspace WHERE rtb.rtb_wspace.wspace-id = gcRtbCurrentWorkspace NO-LOCK NO-ERROR.
IF (AVAILABLE rtb.rtb_wspace) AND (rtb.rtb_wspace.wspace-path<>'') THEN
   PROPATH = rtb.rtb_wspace.wspace-path + ',':U + orig-propath.

FOR EACH tt_sourcefiles :
   RUN WatchForIncludefile IN outputhandler (tt_sourcefiles.IncludeFile, tt_sourcefiles.module).
   RUN prolint/core/prolint.p (tt_sourcefiles.CompUnit,
                          ?,
                          "roundtable check-in":U,
                          TRUE).
END.

/* restore original propath */
IF orig-propath<>PROPATH THEN
   PROPATH = orig-propath.

/* now let our special handler determine which results are warnings and which are errors.
   it will also show those errors in logwin.w */
RUN JudgeResults IN outputhandler (OUTPUT lintresult).
DELETE PROCEDURE outputhandler.

CASE lintresult :
  WHEN 0 THEN allowed = TRUE.

  WHEN 1 THEN
               IF NOT pSimulation THEN DO:
                 IF pTasknumber<>? THEN
                    MESSAGE "Prolint found warnings, do you still want to complete this task?":T
                            VIEW-AS ALERT-BOX QUESTION
                            BUTTONS YES-NO
                            UPDATE allowed.
                 ELSE
                    MESSAGE "Prolint found warnings, do you still want to check-in?":T
                            VIEW-AS ALERT-BOX QUESTION
                            BUTTONS YES-NO
                            UPDATE allowed.
              END.

  OTHERWISE   DO:
                 IF pTasknumber<>? THEN
                    MESSAGE "Prolint found errors, the task will not be completed until you've fixed them.":T SKIP
                            '(Look for descriptions matching "ERROR*")':T
                            VIEW-AS ALERT-BOX WARNING.
                 ELSE
                    MESSAGE "Prolint found errors, the object will not be checked-in until you've fixed them.":T SKIP
                            '(Look for descriptions matching "ERROR*")':T
                            VIEW-AS ALERT-BOX WARNING.
                 Allowed = FALSE.
              END.
END CASE.

RETURN.


FUNCTION RelativeFileName RETURNS CHARACTER
  ( pFileName AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  return the shortest possible relative filename
------------------------------------------------------------------------------*/
   DEFINE VARIABLE fullpath  AS CHAR NO-UNDO.
   DEFINE VARIABLE shortpath AS CHAR NO-UNDO.
   DEFINE VARIABLE i         AS INTEGER NO-UNDO.

   /* first replace all backslashes by forwardslashes */
   pFileName = REPLACE(pFileName, '~\':U, '/':U).
     
   FILE-INFO:FILE-NAME = pFileName.
   fullpath = FILE-INFO:FULL-PATHNAME.
                
   shortpath = ENTRY(NUM-ENTRIES(pFileName, '/':U), pFileName, '/':U).
   DO i=NUM-ENTRIES(pFileName, '/':U) TO 2 BY -1 :
      FILE-INFO:FILE-NAME = shortpath.
      IF FILE-INFO:FULL-PATHNAME = fullpath THEN
         RETURN shortpath.                          
      shortpath = ENTRY(i - 1, pFileName, '/':U) + '/':U + shortpath.   
   END.
   
   RETURN pFileName.

END FUNCTION.


PROCEDURE GetObjectSourcefiles :
/*------------------------------------------------------------------------------
  Purpose:  want to check-in a single object?  Lint that object.
            if it happens to be an includefile then lint some more.
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vPathname AS CHARACTER NO-UNDO.

    DEFINE BUFFER Brtb_object     FOR rtb.rtb_object.

    /* you can only check-in an object if you have a current task, right? */
    vTaskNumber = giRTBCurrentTaskNum.

    FIND Brtb_object NO-LOCK WHERE ROWID(Brtb_object) = rcObject NO-ERROR.
    IF NOT AVAILABLE Brtb_object THEN RETURN.
    IF Brtb_object.obj-type <> "PCODE":U THEN RETURN.

    ASSIGN vPathname = RelativeFilename(DYNAMIC-FUNCTION("fnRtbGetLintFile":U,
                                        ROWID(Brtb_object))).  

    IF CAN-DO(filemasks, vPathname) THEN
       RUN AddFile (vPathname, '*', Brtb_object.module).

    /* includefiles cannot compile (well, most can't anyway).
       find a couple of objects where this object is included, and lint them instead.
       some .w's are included in other .w's too (smart data objects for example) */
    IF number-where-used>0 THEN
       RUN WhereUsed (vPathname, RECID(Brtb_object), Brtb_object.module).

END PROCEDURE.

         
PROCEDURE GetTaskSourcefiles :
/*------------------------------------------------------------------------------
  Purpose:  Want to complete a task?
            Get a list of sourcefiles in this roundtable task.
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vPathname AS CHARACTER NO-UNDO.

    /* create a list of compilation units in this task */
    FOR EACH rtb.rtb_ver NO-LOCK
          WHERE rtb.rtb_ver.task-num   = vTaskNumber
            AND rtb.rtb_ver.obj-status = "W":U :
        
        FIND rtb.rtb_object NO-LOCK
                            WHERE rtb.rtb_object.wspace-id = gcRtbCurrentWorkspace
                              AND rtb.rtb_object.obj-type  = "PCODE":U
                              AND rtb.rtb_object.object    = rtb.rtb_ver.object
                            NO-ERROR.
        IF AVAILABLE rtb.rtb_object THEN DO:
            ASSIGN vPathname = RelativeFilename(DYNAMIC-FUNCTION("fnRtbGetLintFile":U,
                                           ROWID(rtb.rtb_object))).
            IF CAN-DO(filemasks, vPathname) THEN
               RUN AddFile (vPathname, '*':U, rtb.rtb_object.module).

            /* includefiles cannot compile (well, most can't anyway).
               find a couple of objects where this object is included, and lint them instead.
               some .w's are included in other .w's too (smart data objects for example) */
            IF number-where-used>0 THEN
              RUN WhereUsed (vPathname, RECID(rtb.rtb_object), rtb.rtb_object.module).
        END.
    END.
END PROCEDURE.


PROCEDURE WhereUsed :
/* --------------------------------------------------------------------------
   purpose : find some random objects where vIncludefile is included in
   -------------------------------------------------------------------------- */
   DEFINE INPUT PARAMETER vIncludefile AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER vIncRecid    AS RECID     NO-UNDO.
   DEFINE INPUT PARAMETER vModule      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vPathname       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vCount          AS INTEGER   NO-UNDO INITIAL 0.
    DEFINE VARIABLE i               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE rownum          AS INTEGER   NO-UNDO.
    DEFINE BUFFER   whereusedobject FOR rtb.rtb_object.
    DEFINE BUFFER   Brtb_xref       FOR rtb.rtb_xref.
    DEFINE QUERY    qxref           FOR tt_xref SCROLLING.

    FOR EACH Brtb_xref NO-LOCK
                       WHERE Brtb_xref.ref-recid1 = vIncRecid
                         AND Brtb_xref.ref-type   = 5 :
        CREATE tt_xref.
        ASSIGN tt_xref.ref =  brtb_xref.src-recid.
        vCount = vCount + 1.
    END.

    /* pick a number of random records from the WHERE-USED list: */
    loop_getrandom:
    DO i=1 TO number-where-used :
       IF vCount=0 THEN
          LEAVE loop_getrandom.

       IF vCount>1 THEN
          rownum = RANDOM(1,vCount).
       ELSE
          rownum = vCount.

       OPEN QUERY qxref FOR EACH tt_xref.
       REPOSITION qxref TO ROW rownum.
       GET NEXT qxref.
       IF AVAILABLE tt_xref THEN DO:
            FIND whereusedobject NO-LOCK
                                 WHERE RECID(whereusedobject) = tt_xref.ref
                                 NO-ERROR.
            IF AVAILABLE whereusedobject THEN DO:
               ASSIGN vPathname = RelativeFilename(DYNAMIC-FUNCTION("fnRtbGetLintFile":U,
                                              ROWID(whereusedobject))). 
               IF CAN-DO(filemasks, vPathname) THEN
                  RUN AddFile (vPathname, vIncludefile, vModule).
            END.
            /* prevent picking the same record again: */
            DELETE tt_xref.
            vCount = vCount - 1.
       END.
       CLOSE QUERY qxref.
    END.

    /* clean up for next call */
    FOR EACH tt_xref :
        DELETE tt_xref.
    END.

END PROCEDURE.


PROCEDURE AddFile :
/*------------------------------------------------------------------------------
  Purpose:     add file to tt_sourcefiles if it doesn't already exist.
               tt_sourcefiles is the list of things to lint.
  Parameters:  newfile = filename (compilation unit)
               include = includefiles to watch for ('*'=all)
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER newfile AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER include AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER module  AS CHAR NO-UNDO.

  FIND tt_sourcefiles WHERE tt_sourcefiles.CompUnit=newfile NO-ERROR.

  IF NOT AVAILABLE tt_sourcefiles THEN DO:
     CREATE tt_sourcefiles.
     ASSIGN tt_sourcefiles.CompUnit    = newfile
            tt_sourcefiles.Includefile = include.
  END.
  ELSE DO:
     IF (include='*':U) AND (tt_sourcefiles.Includefile<>'*':U) THEN
        tt_sourcefiles.Includefile = '*':U.
     tt_sourcefiles.module = module.
  END.

END PROCEDURE.


PROCEDURE ReadConfiguration :
/*------------------------------------------------------------------------------
  Purpose:     read ini file
------------------------------------------------------------------------------*/
   DEFINE VARIABLE vLine AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCurrentSection AS CHARACTER NO-UNDO.

   INPUT FROM VALUE (DYNAMIC-FUNCTION("fnRtbFindProlintIniFile":U)).
   loop_readline:
   REPEAT:
      IMPORT UNFORMATTED vLine.
      vLine = TRIM(vLine).
      IF vLine="" THEN NEXT loop_readline.
      IF vLine MATCHES "#*":U THEN NEXT loop_readline.
      IF vLine MATCHES "[*]*":U THEN
         vCurrentSection = TRIM(SUBSTRING(vLine,2,INDEX(vLine,']':U) - 2)).
      IF (vLine MATCHES "*=*":U) AND (vCurrentSection<>"") THEN DO:
         CREATE tt_ini.
         ASSIGN tt_ini.fSection = vCurrentSection
                tt_ini.fKey     = TRIM(ENTRY(1, vLine, "=":U))
                tt_ini.fValue   = TRIM(SUBSTRING(vLine,INDEX(vLine,"=":U) + 1)).
      END.
   END.
   INPUT CLOSE.

   FIND tt_ini WHERE tt_ini.fSection = "behaviour":U
                 AND tt_ini.fKey     = "number-where-used":U
               NO-ERROR.
   IF AVAILABLE tt_ini THEN
      ASSIGN number-where-used = INTEGER(tt_ini.fValue) NO-ERROR.

END PROCEDURE.

