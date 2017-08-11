/* ============================================================================
   file    : protools/rtb/lintobj.p
   by      : Jurjen Dijkstra
   purpose : determine which object is currently selected in the RTB Tabletop,
             then invoke Prolint to check this current object.
             This procedure is called from the "on choose" event of the
             menu-item "Prolint current object" or button "Prolint object" on
             the Roundtable Tabletop.
             
             GWINNING(02/17/02): Add call to new function "fnGetLintFile"
             to dermine the source file name - the new function handles
             objects in task directories and .html compiles (for html, 
             {prolint/roundtable/101b/rt_customFunc.i} must be included in rtb_evnt.p.
             
             Thomas Hansen, appSolutions 02/2008:
             Updated code to use RTB event model and variables from RTB 10.1B
             
   ============================================================================ */
              
/*{rtb/g/rtbglobl.i}*/

DEFINE VARIABLE ver_recid                 AS RECID      NO-UNDO.
DEFINE VARIABLE rRtbVer                   AS ROWID      NO-UNDO.
DEFINE VARIABLE vcSourceName              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE gcRtbCurrentWorkspace     AS CHARACTER  NO-UNDO.

DEFINE BUFFER   Brtb_object     FOR rtb.rtb_object.
DEFINE BUFFER   Brtb_ver        FOR rtb.rtb_ver.

/* if check-in validation is enabled in this workspace, then
   simulate a check-in. (test what you would test if you were going to check-in this object) */
IF DYNAMIC-FUNCTION("fnRtbFindProlintIniFile":U)<>? THEN DO:
   RUN prolint/roundtable/101b/checkobj.p.
   RETURN.
END.
/* else, simply run prolint using profile "roundtable run" */

  /* Get he current workspace and current object verison record */
  PUBLISH "evRtbGetCurrentWorkspace":U (OUTPUT gcRtbCurrentWorkspace).
  PUBLISH "evRtbGetCurrentVersionRowid":U (OUTPUT rRtbVer).

FIND Brtb_ver NO-LOCK WHERE ROWID(Brtb_ver) = rRtbVer NO-ERROR.
IF AVAILABLE Brtb_ver THEN 
DO:
  /* if the object is not an includefile or binary or pfield etc,
     you can't pass it to prolint */
  IF NOT (Brtb_ver.compiles) THEN 
     MESSAGE "This object cannot be checked by Prolint" SKIP 
             "because the 'Compiles'-flag on the Roundtable Config tab is not set"
             VIEW-AS ALERT-BOX.
  ELSE 
  DO:
     FIND Brtb_object NO-LOCK 
                         WHERE Brtb_object.wspace-id = gcRtbCurrentWorkspace
                           AND Brtb_object.obj-type  = "PCODE":U
                           AND Brtb_object.object    = Brtb_ver.object
                         NO-ERROR.
     IF NOT AVAILABLE Brtb_object THEN
         MESSAGE "Brtb_object not found" VIEW-AS ALERT-BOX.
     ELSE
     DO:
        vcSourceName = DYNAMIC-FUNCTION("fnRtbGetLintFile":U, 
                                        ROWID(Brtb_object)).

        IF vcSourceName = "" THEN
        DO:
          MESSAGE "Source File Name could not be determined"
            VIEW-AS ALERT-BOX
            ERROR.
          RETURN.
        END.  

        RUN prolint/core/prolint.p (vcSourceName,
                               ?,
                               "roundtable run":U,
                               TRUE).  
     END.
  END.
END.           

        
