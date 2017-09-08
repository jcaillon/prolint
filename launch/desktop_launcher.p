&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 

/* ***************************  Definitions  ************************** */

/* ******************************************************************** */
/* *** Temp-Table Definitions                                       *** */
/* ******************************************************************** */


/* ******************************************************************** */
/* *** Buffers Definitions                                          *** */
/* ******************************************************************** */


/* ******************************************************************** */
/* *** Parameters Definitions                                       *** */
/* ******************************************************************** */


/* ******************************************************************** */
/* *** Local Variable Definitions                                   *** */
/* ******************************************************************** */


/* ******************************************************************** */
/* *** Streams Definitions                                          *** */
/* ******************************************************************** */



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
         HEIGHT             = 13.43
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 

/* ***************************  Main Block  *************************** */

RUN pi_main NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    MESSAGE (IF (ERROR-STATUS:NUM-MESSAGES > 0) THEN ERROR-STATUS:GET-MESSAGE(1) ELSE (IF RETURN-VALUE <> ? AND RETURN-VALUE > "" THEN RETURN-VALUE ELSE "Error when launching the application")) VIEW-AS ALERT-BOX ERROR TITLE "Prolint".

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_main Procedure
PROCEDURE pi_main PRIVATE:
/*------------------------------------------------------------------------------
  Summary    : Main entry point of the program
  Parameters : <none>
  Returns    : 
  Remarks    :     
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lc_programDir AS CHARACTER NO-UNDO.

    /* Where are we running this from? */
    ASSIGN
        FILE-INFO:FILE-NAME = THIS-PROCEDURE:FILE-NAME
        lc_programDir = REPLACE(FILE-INFO:FULL-PATHNAME, "~/", "~\")
        lc_programDir = SUBSTRING(lc_programDir, 1, R-INDEX(lc_programDir, '~\'))
        lc_programDir = lc_programDir + "..\..\".
        
        /* Make sure to find the assemblies needed for prolint (proparse assemblies) */
    RUN ChangeAssembliesPath (INPUT lc_programDir + "proparse.net\") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR (IF (ERROR-STATUS:NUM-MESSAGES > 0) THEN ERROR-STATUS:GET-MESSAGE(1) ELSE (IF RETURN-VALUE <> ? THEN RETURN-VALUE ELSE "")).

    /* add the prolint directory to the propath */
    PROPATH = PROPATH + "," + lc_programDir.
        
    RUN prolint/launch/desktop.w NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR (IF (ERROR-STATUS:NUM-MESSAGES > 0) THEN ERROR-STATUS:GET-MESSAGE(1) ELSE (IF RETURN-VALUE <> ? THEN RETURN-VALUE ELSE "")).

    RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeAssembliesPath Procedure
PROCEDURE ChangeAssembliesPath PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: This procedure allows to modify the assemblies path dynamically, it is equivalent to
    start a prowin process with the parameter : -assemblies "my path"
    Parameters:
    ipc_newPath = new assemblies path
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ipc_newPath AS CHARACTER NO-UNDO.

    DEFINE VARIABLE assemblyStore AS Progress.ClrBridge.AssemblyStore NO-UNDO.
    DEFINE VARIABLE lc_oldAssemblyDir AS CHARACTER NO-UNDO.

    assemblyStore = Progress.ClrBridge.AssemblyStore:Instance.
    lc_oldAssemblyDir = assemblyStore:AssembliesPath.

    IF LENGTH(ipc_newPath) > 0 THEN DO:
        assemblyStore:AssembliesPath = ipc_newPath.
    END.

    assemblyStore:Load() NO-ERROR.
    assemblyStore:AssembliesPath  = lc_oldAssemblyDir.

    IF VALID-OBJECT(assemblyStore) THEN
        DELETE OBJECT assemblyStore.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR-STATUS:GET-MESSAGE(1).

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
