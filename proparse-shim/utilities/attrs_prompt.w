&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: attrs_prompt.w

  Description:
      Prompt for node attributes.
      Used by launcher.w to get a list of node attributes to
      display in tokenlister.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Input-Output:
      CHARACTER - comma separated list of attributes.
      Input the default selection.

  Author: John Green, Joanju Limited

  Created: 2001
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER attrList AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK showLine showColumn showFile ~
showStoretype showStatehead showState2 showDirectives 
&Scoped-Define DISPLAYED-OBJECTS showLine showColumn showFile showStoretype ~
showStatehead showState2 showDirectives 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE VARIABLE showColumn AS LOGICAL INITIAL no 
     LABEL "Show Column Position" 
     VIEW-AS TOGGLE-BOX
     SIZE 56 BY .77 NO-UNDO.

DEFINE VARIABLE showDirectives AS LOGICAL INITIAL no 
     LABEL "Show 'proparsedirective'" 
     VIEW-AS TOGGLE-BOX
     SIZE 55 BY .77 NO-UNDO.

DEFINE VARIABLE showFile AS LOGICAL INITIAL no 
     LABEL "Show File Names" 
     VIEW-AS TOGGLE-BOX
     SIZE 55 BY .81 NO-UNDO.

DEFINE VARIABLE showLine AS LOGICAL INITIAL no 
     LABEL "Show Line Numbers" 
     VIEW-AS TOGGLE-BOX
     SIZE 55 BY .81 NO-UNDO.

DEFINE VARIABLE showState2 AS LOGICAL INITIAL no 
     LABEL "Show 'state2' (secondary statement type)" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE showStatehead AS LOGICAL INITIAL no 
     LABEL "Show 'statehead' (if node is statement head)" 
     VIEW-AS TOGGLE-BOX
     SIZE 55 BY .81 NO-UNDO.

DEFINE VARIABLE showStoretype AS LOGICAL INITIAL no 
     LABEL "Show 'storetype'" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.54 COL 49
     showLine AT ROW 2.88 COL 5
     showColumn AT ROW 3.96 COL 5
     showFile AT ROW 5.04 COL 5
     showStoretype AT ROW 6.12 COL 5
     showStatehead AT ROW 7.19 COL 5
     showState2 AT ROW 8.27 COL 5
     showDirectives AT ROW 9.35 COL 5
     SPACE(5.13) SKIP(0.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Attributes Selection"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Attributes Selection */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:

DEFINE VARIABLE sep AS CHARACTER NO-UNDO.

ASSIGN attrList = "".

IF INPUT showLine THEN ASSIGN
  attrList = attrList + sep + "linenum":U
  sep = ",":U.

IF INPUT showColumn THEN ASSIGN
  attrList = attrList + sep + "column":U
  sep = ",":U.

IF INPUT showFile THEN ASSIGN
  attrList = attrList + sep + "filename":U
  sep = ",":U.

IF INPUT showStoretype THEN ASSIGN
  attrList = attrList + sep + "storetype":U
  sep = ",":U.

IF INPUT showStatehead THEN ASSIGN
  attrList = attrList + sep + "statehead":U
  sep = ",":U.

IF INPUT showState2 THEN ASSIGN
  attrList = attrList + sep + "state2":U
  sep = ",":U.

IF INPUT showDirectives THEN ASSIGN
  attrList = attrList + sep + "proparsedirective":U
  sep = ",":U.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


ASSIGN
  showColumn     = CAN-DO(attrList, "column":U)
  showDirectives = CAN-DO(attrList, "proparsedirective":U)
  showLine       = CAN-DO(attrList, "linenum":U)
  showFile       = CAN-DO(attrList, "filename":U)
  showStoretype  = CAN-DO(attrList, "storetype":U)
  showState2     = CAN-DO(attrList, "state2":U)
  showStatehead  = CAN-DO(attrList, "statehead":U).


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY showLine showColumn showFile showStoretype showStatehead showState2 
          showDirectives 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK showLine showColumn showFile showStoretype showStatehead 
         showState2 showDirectives 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

