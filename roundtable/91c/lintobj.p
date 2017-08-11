/* ============================================================================
   file    : prolint/roundtable/91c/lintobj.p
   by      : Jurjen Dijkstra
   purpose : determine which object is currently selected in the RTB Tabletop,
             then invoke Prolint to check this current object.
             This procedure is called from the "on choose" event of the
             menu-item "Prolint current object" or button "Prolint object" on
             the Roundtable Tabletop.
             
             GWINNING(02/17/02): Add call to new function "fnGetLintFile"
             to dermine the source file name - the new function handles
             objects in task directories and .html compiles (for html, 
             {prolint/roundtable/91c/rt_customFunc.i} must be included in rtb_evnt.p.
   ============================================================================ */
              
{rtb/g/rtbglobl.i}                         

DEFINE VARIABLE ver_recid     AS RECID NO-UNDO.
DEFINE VARIABLE vcSourceName  AS CHARACTER  NO-UNDO.
DEFINE BUFFER brtb_object     FOR rtb.rtb_object.
DEFINE BUFFER brtb_ver        FOR rtb.rtb_ver.

/* if check-in validation is enabled in this workspace, then
   simulate a check-in. (test what you would test if you were going to check-in this object) */
IF DYNAMIC-FUNCTION("fnRtbFindProlintIniFile":U)<>? THEN DO:
   RUN prolint/roundtable/91c/checkobj.p.
   RETURN.
END.
/* else, simply run prolint using profile "roundtable run" */

IF VALID-HANDLE(Grtb-proc-handle) THEN 
   RUN choose_object IN Grtb-proc-handle ("dummy window title", 
                                          FALSE, /* not interactive */
                                          OUTPUT ver_recid).

FIND brtb_ver NO-LOCK WHERE RECID(brtb_ver)=ver_recid NO-ERROR.
IF AVAILABLE brtb_ver THEN DO:
  /* if the object is not an includefile or binary or pfield etc,
     you can't pass it to prolint */
  IF NOT (brtb_ver.compiles) THEN 
     MESSAGE "This object cannot be checked by Prolint" SKIP 
             "because the 'Compiles'-flag on the Roundtable Config tab is not set"
             VIEW-AS ALERT-BOX.
  ELSE DO:
     FIND brtb_object NO-LOCK 
                         WHERE brtb_object.wspace-id = Grtb-wspace-id
                           AND brtb_object.obj-type  = "PCODE":U
                           AND brtb_object.object    = brtb_ver.object
                         NO-ERROR.
     IF NOT AVAILABLE brtb_object THEN
         MESSAGE "brtb_object not found" VIEW-AS ALERT-BOX.
     ELSE
     DO:
       vcSourceName = DYNAMIC-FUNCTION("fnRtbGetLintFile":U, 
                                       RECID(brtb_object)).

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

        
