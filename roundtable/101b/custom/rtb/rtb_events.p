&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : rtb_events.p
    Description : Roundtable event handler

    Author(s)   :
    Created     :
    Notes       : Backwards-compatible with rtb_evnt.p customizations.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE Muse-legacy AS LOGICAL    NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE vhRtbCustFunc AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE viRtbCustFuncId AS INTEGER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE glob_completingtask AS INTEGER NO-UNDO INITIAL 0.

&SCOPED-DEFINE PRODUCT "Roundtable":U

/*

Roundtable User Events

rtb_events.p replaces rtb_evnt.p.  However, you can still use rtb_evnt.p.
Copy rtb_evnt.p to the root of your Roundtable installtion directory.  This
program will detect it's existence, translate the events, and pass them on
to your legacy event handler.  See the Main-Block of this procedure.

"addObjectBefore"
  Pcontext = ""
  Pother   = Object Name
  Pok      = False will cancel

"addObject"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name

"deleteObjectBefore"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name
  Pok      = False will cancel

"deleteObject"
  Pcontext = WIP version of the deleted object
  Pother   = Object Name

"createObjectVariantBefore"
  Pother   = Object Name
  Pok      = False will cancel

"createObjectVariant"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name

"assignObjectBefore"
  Pother   = Object Name, Object Type, Product Module, Version
  Pok      = False will cancel

"assignObject"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name

"compileObjectBefore"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name
  Pok      = False will cancel

"compileObject"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name

"checkinObjectBefore"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name
  Pok      = False will cancel

"checkinObject"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name

"checkoutObjectBefore"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name, Level ("version","revision","patch")
  Pok      = False will cancel

"checkoutObject"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name

"makeDeploymentBefore"
  Pcontext = rtb_site ROWID string value
  Pother   = rtb_deploy ROWID string value
  Pok      = False will cancel

"makeDeployment"
  Pcontext = rtb_site ROWID string value
  Pother   = rtb_deploy ROWID string value

"createReleaseBefore"
  Pother   = Workspce ID
  Pok      = False will cancel

"createRelease"
  Pother   = Workspace ID, Release Number

"updateSchemaBefore"
  Pother   = Workspace ID
  Pok      = False will cancel

"updateSchema"
  Pother   = Workspace ID

"createTaskBefore"
  Pcontext = CHR(1)-delimited list of column/value pairs
  Pother   = Workspace ID
  Pok      = False will cancel

"createTask"
  Pcontext = rtb_task ROWID string value
  Pother   = Task Number

"completeTaskBefore"
  Pother   = Task Number
  Pok      = False will cancel

"completeTask"
  Pother   = Task Number

"createSiteBefore"
  Pother   = Workspace ID, License Type, Site Code
  Pok      = False will cancel

"createSite"
  Pother   = Workspace ID, License Type, Site Code

"processImportBefore"
  Pother   = Workspace ID
  Pok      = False will cancel

"processImport"
  Pother   = Workspace ID

"changeWorkspaceBefore"
  Pcontext = Desktop handle string value
  Pother   = From-Workspace, To-Workspace
  Pok      = False will cancel

"changeWorkspace"
  Pcontext = Desktop handle string value
  Pother   = Workspace ID

"moveToWebBefore"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name, Object Type, Pmod, Module
  Pok      = False will cancel

"moveToWeb"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name

"changeTask"
  Pother   = Task Number

"loadPartnerBefore"
  Pcontext = Workspace ID
  Pother   = Workspace Path
  Pok      = False will cancel

"loadPartner"
  Pcontext = rtb_wspace ROWID string value
  Pother   = Object Name

"changePropathBefore"
    Pcontext = rtb_object ROWID string value
    Pother   = "BEFORE-COMPILE" or "AFTER-COMPILE" or "BEFORE-XREF" or "AFTER-XREF"

"changePropath"
  Pcontext = rtb_object ROWID string value
  Pother   = "BEFORE-COMPILE" or "AFTER-COMPILE" or "BEFORE-XREF" or "AFTER-XREF"

"changeObjectShareStatusBefore"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name, Old Status, New Status, WorkspacePath, Task Path
  Pok      = False will cancel

"changeObjectShareStatus"
  Pcontext = rtb_object ROWID string value
  Pother   = Object Name, Old Status, New Status, WorkspacePath, Task Path

"roundtableStartup"
  Pcontext = Tabletop procedure handle string value
  Pother   = Tabletop window handle string value

"roundtableShutdown"

"sessionStartup"
  Pcontext = Session ID string value
  Pok      = False will cancel

"sessionShutdown"
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.52
         WIDTH              = 49.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

SUBSCRIBE TO "evRtbUserEvent":U ANYWHERE.


/*
 Does legacy event handler exist?

 ** Set this to FALSE if you do not want to use rtb_evnt.p
     customizations
*/
Muse-legacy = SEARCH("rtb_evnt.p":U) <> ?.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-evRtbUserEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE evRtbUserEvent Procedure 
PROCEDURE evRtbUserEvent :
/*------------------------------------------------------------------------------
  Purpose:     Roundtable Event Handler 
  Parameters:  Pevent
               Pcontext
               Pother
               Pok
  Notes:       Common procedure for all events
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER Pevent   AS CHARACTER.
  DEFINE INPUT  PARAMETER Pcontext AS CHARACTER.
  DEFINE INPUT  PARAMETER Pother   AS CHARACTER.
  DEFINE OUTPUT PARAMETER Pok      AS LOGICAL INIT YES.

  IF Muse-legacy THEN DO:
    /*
     Event specific processing for legacy code
    */
    RUN rtb_legacy_events (INPUT Pevent,
                           INPUT Pcontext,
                           INPUT Pother,
                           OUTPUT Pok).
  END.
  ELSE DO:
    /*
    MESSAGE "Event:   " Pevent SKIP
            "Context: " Pcontext SKIP
            "Other:   " Pother
      VIEW-AS ALERT-BOX
      BUTTONS OK-CANCEL
      TITLE "Roundtable Event"
      UPDATE Pok.
    */  
      {prolint/roundtable/101b/custom_events.i}
            
  END.

   RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-rtb_legacy_events) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rtb_legacy_events Procedure 
PROCEDURE rtb_legacy_events :
/*------------------------------------------------------------------------------
  Purpose:    Provide backwards-compatibility for the legacy event handler
  Parameters: Pevent
              Pcontext
              Pother
              Pok
  Notes:  This procedure mainly transposes ROWIDs to RECIDs passed through Pcontext
          and converts the event names to their legacy names.       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER Pevent   AS CHARACTER.
  DEFINE INPUT  PARAMETER Pcontext AS CHARACTER.
  DEFINE INPUT  PARAMETER Pother   AS CHARACTER.
  DEFINE OUTPUT PARAMETER Pok      AS LOGICAL.

  DEFINE VARIABLE MlegacyEvent AS CHARACTER NO-UNDO.

  CASE Pevent:

  WHEN "addObjectBefore":U THEN DO:
    MlegacyEvent = "OBJECT-ADD-BEFORE":U.
  END.

  WHEN "addObject":U THEN DO:
    MlegacyEvent = "OBJECT-ADD":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)).
  END.

  WHEN "deleteObjectBefore":U THEN DO:
    MlegacyEvent = "OBJECT-DELETE-BEFORE":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)).
  END.

  WHEN "deleteObject":U THEN DO:
    MlegacyEvent = "OBJECT-DELETE":U.
  END.

  WHEN "createObjectVariantBefore":U THEN DO:
    MlegacyEvent = "CREATE-CV-BEFORE":U.
  END.

  WHEN "createObjectVariant":U THEN DO:
    MlegacyEvent = "CREATE-CV":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)).
  END.

  WHEN "assignObjectBefore":U THEN DO:
    MlegacyEvent = "ASSIGN-OBJECT-BEFORE":U.
  END.

  WHEN "assignObject":U THEN DO:
    MlegacyEvent = "ASSIGN-OBJECT":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)).
  END.

  WHEN "compileObjectBefore":U THEN DO:
    MlegacyEvent = "OBJECT-COMPILE-BEFORE":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)). 
  END.

  WHEN "compileObject":U THEN DO:
    MlegacyEvent = "OBJECT-COMPILE":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)).
  END.

  WHEN "checkinObjectBefore":U THEN DO:
    MlegacyEvent = "OBJECT-CHECK-IN-BEFORE":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)).
  END.

  WHEN "checkinObject":U THEN DO:
    MlegacyEvent = "OBJECT-CHECK-IN":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)).
  END.

  WHEN "checkoutObjectBefore":U THEN DO:
    MlegacyEvent = "OBJECT-CHECK-OUT-BEFORE":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)).
  END.

  WHEN "checkoutObject":U THEN DO:
    MlegacyEvent = "OBJECT-CHECK-OUT":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)).
  END.

  WHEN "makeDeploymentBefore":U THEN DO:
    MlegacyEvent = "DEPLOY-BEFORE":U.  
    FIND rtb.rtb_site WHERE ROWID(rtb.rtb_site) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_site)).
    FIND rtb.rtb_deploy WHERE ROWID(rtb.rtb_deploy) = TO-ROWID(Pother) NO-LOCK.
    Pother = STRING(RECID(rtb.rtb_deploy)).
  END.

  WHEN "makeDeployment" THEN DO:
    MlegacyEvent = "DEPLOY":U.
    FIND rtb.rtb_site WHERE ROWID(rtb.rtb_site) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_site)).
    FIND rtb.rtb_deploy WHERE ROWID(rtb.rtb_deploy) = TO-ROWID(Pother) NO-LOCK.
    Pother = STRING(RECID(rtb.rtb_deploy)).
  END.

  WHEN "createReleaseBefore":U THEN DO:
    MlegacyEvent = "RELEASE-CREATE-BEFORE":U.
  END.

  WHEN "createRelease":U THEN DO:
    MlegacyEvent = "RELEASE-CREATE":U.
  END.

  WHEN "updateSchemaBefore":U THEN DO:
    MlegacyEvent = "SCHEMA-UPDATE-BEFORE":U.
  END.

  WHEN "updateSchema":U THEN DO:
    MlegacyEvent = "SCHEMA-UPDATE":U.
  END.

  WHEN "createTaskBefore":U THEN DO:
    MlegacyEvent = "TASK-CREATE-BEFORE":U.
  END.

  WHEN "createTask":U THEN DO:
    MlegacyEvent = "TASK-CREATE":U.
    FIND rtb.rtb_task WHERE ROWID(rtb.rtb_task) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_task)).
  END.

  WHEN "completeTaskBefore":U THEN DO:
    MlegacyEvent = "TASK-COMPLETE-BEFORE":U.
  END.

  WHEN "completeTask":U THEN DO:
    MlegacyEvent = "TASK-COMPLETE":U.
  END.

  WHEN "createSiteBefore":U THEN DO:
    MlegacyEvent = "DEPLOY-SITE-CREATE-BEFORE":U.
  END.

  WHEN "createSite":U THEN DO:
    MlegacyEvent = "DEPLOY-SITE-CREATE":U.
  END.

  WHEN "processImportBefore":U THEN DO:
    MlegacyEvent = "IMPORT-BEFORE":U.
  END.

  WHEN "processImport":U THEN DO:
    MlegacyEvent = "IMPORT":U.
  END.

  WHEN "changeWorkspaceBefore":U THEN DO:
    MlegacyEvent = "BEFORE-CHANGE-WORKSPACE":U.
  END.

  WHEN "changeWorkspace":U THEN DO:
    MlegacyEvent = "CHANGE-WORKSPACE":U.
  END.

  WHEN "moveToWebBefore":U THEN DO:
    MlegacyEvent = "MOVE-TO-WEB-BEFORE":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)). 
  END.

  WHEN "moveToWeb":U THEN DO:
    MlegacyEvent = "MOVE-TO-WEB":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)). 
  END.

  WHEN "changeTask":U THEN DO:
    MlegacyEvent = "TASK-CHANGE":U.
  END.

  WHEN "loadPartnerBefore":U THEN DO:
    MlegacyEvent = "PARTNER-LOAD-BEFORE":U.
  END.

  WHEN "loadPartner":U THEN DO:
    MlegacyEvent = "PARTNER-LOAD":U.
    FIND rtb.rtb_wspace WHERE ROWID(rtb.rtb_wspace) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_wspace)). 
  END.

  WHEN "changePropathBefore":U THEN DO:
    MlegacyEvent = "PROPATH-CHANGE-BEFORE":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)). 
  END.

  WHEN "changePropath":U THEN DO:
    MlegacyEvent = "PROPATH-CHANGE":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)). 
  END.

  WHEN "changeObjectShareStatusBefore":U THEN DO:
    MlegacyEvent = "OBJECT-CHANGE-SHARE-STATUS-BEFORE":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)). 
    Pother = REPLACE(Pother,CHR(1),",").
  END.

  WHEN "changeObjectShareStatus":U THEN DO:
    MlegacyEvent = "OBJECT-CHANGE-SHARE-STATUS":U.
    FIND rtb.rtb_object WHERE ROWID(rtb.rtb_object) = TO-ROWID(Pcontext) NO-LOCK.
    Pcontext = STRING(RECID(rtb.rtb_object)).
    Pother = REPLACE(Pother,CHR(1),",").
  END.

  WHEN "roundtableStartup":U THEN DO:
    MlegacyEvent = "ROUNDTABLE-STARTUP":U.
  END.

  WHEN "roundtableShutdown":U THEN DO:
    MlegacyEvent = "ROUNDTABLE-SHUTDOWN":U.
  END.

  WHEN "sessionStartup":U THEN DO:
  END.                            

  WHEN "sessionShutdown":U THEN DO:
    MlegacyEvent = "SESSION-SHUTDOWN":U.
  END.

  END CASE.

  /*
   Run the legacy event handler..
  */
  RUN rtb_evnt.p (INPUT {&PRODUCT},
                  INPUT MlegacyEvent,
                  INPUT Pcontext,
                  INPUT Pother,
                  OUTPUT Pok).

  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

