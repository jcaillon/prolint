&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt_rules NO-UNDO
   FIELD RuleID       AS CHARACTER  FORMAT "x(15)":U
   FIELD severity     AS INTEGER    FORMAT "9":U
   FIELD useproparse  AS LOGICAL    FORMAT "yes/no":U
   FIELD uselisting   AS LOGICAL    FORMAT "yes/no":U
   FIELD usexref      AS LOGICAL    FORMAT "yes/no":U
   FIELD useproclist  AS LOGICAL    FORMAT "yes/no":U
   FIELD ignoreUIB    AS LOGICAL    FORMAT "yes/no":U
   FIELD description  AS CHARACTER  FORMAT "x(60)":U
   FIELD category     AS CHARACTER
INDEX idx_id   AS PRIMARY RuleID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ed_ruleid ed_description ed_severity ~
cb_category tg_proparse tg_xref tg_proclist tg_forgetab Btn_OK Btn_Cancel ~
Btn_Help RECT-1 
&Scoped-Define DISPLAYED-OBJECTS ed_ruleid ed_description ed_severity ~
cb_category tg_proparse tg_xref tg_proclist tg_forgetab FILL-IN-7 FILL-IN-8 ~
FILL-IN-9 FILL-IN-10 FILL-IN-11 FILL-IN-12 FILL-IN-13 FILL-IN-14 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cb_category AS CHARACTER FORMAT "X(256)":U 
     LABEL "category" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE ed_description AS CHARACTER FORMAT "X(60)":U 
     LABEL "description" 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE ed_ruleid AS CHARACTER FORMAT "X(15)":U 
     LABEL "rule-id" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE ed_severity AS INTEGER FORMAT "9":U INITIAL 5 
     LABEL "severity" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "default severity, number between 0 to 9 (inclusive)" 
      VIEW-AS TEXT 
     SIZE 48 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U INITIAL "toggle ~"on~" if this rule cannot run without Proparse" 
      VIEW-AS TEXT 
     SIZE 51 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS CHARACTER FORMAT "X(256)":U INITIAL "toggle ~"on~" if this rule needs to read XREF file" 
      VIEW-AS TEXT 
     SIZE 49 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-13 AS CHARACTER FORMAT "X(256)":U INITIAL "toggle ~"on~" if this rule needs a list of startnodes for each ip/udf" 
      VIEW-AS TEXT 
     SIZE 60 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-14 AS CHARACTER FORMAT "X(256)":U INITIAL "toggle ~"on~" if warnings for AB-generated code should be surpressed" 
      VIEW-AS TEXT 
     SIZE 65 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U INITIAL "This program helps you insert a new entry to prolint/rules/rules.d" 
      VIEW-AS TEXT 
     SIZE 76 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "rule-id is name of sourcefile without ~".p~"" 
      VIEW-AS TEXT 
     SIZE 39 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL "short description for browse in profile configuration dialog" 
      VIEW-AS TEXT 
     SIZE 55 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 1.67.

DEFINE VARIABLE tg_forgetab AS LOGICAL INITIAL no 
     LABEL "ignore AB-generated code" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tg_proclist AS LOGICAL INITIAL no 
     LABEL "uses ProcedureListGet" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE tg_proparse AS LOGICAL INITIAL yes 
     LABEL "requires Proparse" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tg_xref AS LOGICAL INITIAL no 
     LABEL "requires compile xref" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ed_ruleid AT ROW 2.67 COL 14 COLON-ALIGNED
     ed_description AT ROW 5.05 COL 14.2 COLON-ALIGNED
     ed_severity AT ROW 7.19 COL 14 COLON-ALIGNED
     cb_category AT ROW 8.86 COL 14 COLON-ALIGNED
     tg_proparse AT ROW 10.52 COL 16
     tg_xref AT ROW 12.67 COL 16
     tg_proclist AT ROW 14.57 COL 16
     tg_forgetab AT ROW 16.48 COL 16
     Btn_OK AT ROW 19.1 COL 39
     Btn_Cancel AT ROW 19.1 COL 56
     Btn_Help AT ROW 19.1 COL 73
     FILL-IN-7 AT ROW 1.48 COL 1 COLON-ALIGNED NO-LABEL
     FILL-IN-8 AT ROW 3.76 COL 14 COLON-ALIGNED NO-LABEL
     FILL-IN-9 AT ROW 6.1 COL 14 COLON-ALIGNED NO-LABEL
     FILL-IN-10 AT ROW 7.38 COL 24 COLON-ALIGNED NO-LABEL
     FILL-IN-11 AT ROW 11.33 COL 17.8 COLON-ALIGNED NO-LABEL
     FILL-IN-12 AT ROW 13.48 COL 17.8 COLON-ALIGNED NO-LABEL
     FILL-IN-13 AT ROW 15.38 COL 17.8 COLON-ALIGNED NO-LABEL
     FILL-IN-14 AT ROW 17.29 COL 17.8 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 18.86 COL 1
     SPACE(0.19) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Add a new rule to rules.d"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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

/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-12 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-13 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-14 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add a new rule to rules.d */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO:

  RUN prolint/core/openhtml.p ( "http://oehive.org/node/254":U ).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN ed_ruleid
         ed_description
         ed_severity
         tg_proparse
         tg_xref
         tg_proclist
         tg_forgetab
         cb_category.

  IF ed_ruleid = "" THEN DO:
     MESSAGE "please enter a rule-id"
             VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.

  IF ed_description = "" THEN DO:
   MESSAGE "please enter a description"
           VIEW-AS ALERT-BOX.
   RETURN NO-APPLY.
  END.

  IF cb_category = "" THEN DO:
   MESSAGE "please choose a category"
           VIEW-AS ALERT-BOX.
   RETURN NO-APPLY.
  END.

  IF ed_severity<0 OR ed_severity>9 THEN DO:
     MESSAGE "severity should be at least 0, not greater than 9"
             VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.

  FIND tt_rules WHERE tt_rules.RuleID = ed_ruleid NO-LOCK NO-ERROR.
  IF AVAILABLE tt_rules THEN DO:
      MESSAGE SUBSTITUTE("rule &1 already exists", ed_ruleid)
              VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.


  CREATE tt_rules.
  ASSIGN tt_rules.RuleId = ed_ruleid
         tt_rules.severity = ed_severity
         tt_rules.useproparse = tg_proparse
         tt_rules.uselisting = FALSE
         tt_rules.usexref = tg_xref
         tt_rules.useproclist = tg_proclist
         tt_rules.ignoreUIB = tg_forgetab
         tt_rules.DESCRIPTION = ed_description
         tt_rules.category = cb_category.
  RELEASE tt_rules.

  RUN WriteRules.

  MESSAGE "Now that you have created a new rule, would you please consider sharing it with the Prolint Open Source project?"
          VIEW-AS ALERT-BOX QUESTION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN ReadRules.
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
  DISPLAY ed_ruleid ed_description ed_severity cb_category tg_proparse tg_xref 
          tg_proclist tg_forgetab FILL-IN-7 FILL-IN-8 FILL-IN-9 FILL-IN-10 
          FILL-IN-11 FILL-IN-12 FILL-IN-13 FILL-IN-14 
      WITH FRAME Dialog-Frame.
  ENABLE ed_ruleid ed_description ed_severity cb_category tg_proparse tg_xref 
         tg_proclist tg_forgetab Btn_OK Btn_Cancel Btn_Help RECT-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadRules Dialog-Frame 
PROCEDURE ReadRules :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE rules_d AS CHARACTER NO-UNDO.
   DEFINE VARIABLE categorylist AS CHARACTER NO-UNDO.
 

   FOR EACH tt_rules :
       DELETE tt_rules.
   END.

   FILE-INFO:FILE-NAME = "prolint/rules/rules.d":U.
   IF FILE-INFO:FULL-PATHNAME <> ? THEN
      rules_d = FILE-INFO:FULL-PATHNAME.
   ELSE DO:
       MESSAGE "Cannot find prolint/rules/rules.d"
               VIEW-AS ALERT-BOX ERROR.
       RETURN.
   END.

   INPUT FROM VALUE(rules_d).    
   REPEAT:
     CREATE tt_rules.
     IMPORT tt_rules.
   END.
   INPUT CLOSE.
   FOR EACH tt_rules WHERE tt_rules.RuleID = "" :
       DELETE tt_rules.
   END.

   categorylist = "".
   FOR EACH tt_rules WHERE tt_rules.category<>"" :
      IF LOOKUP(tt_rules.category, categorylist)=0 THEN
         categorylist = categorylist + ",":U + tt_rules.category.
   END.
   categorylist = TRIM(categorylist,",":U).
   
   DO WITH FRAME {&FRAME-NAME} :
      cb_category:LIST-ITEMS = categorylist.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WriteRules Dialog-Frame 
PROCEDURE WriteRules :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE rules_d AS CHARACTER NO-UNDO.

   FILE-INFO:FILE-NAME = "prolint/rules/rules.d":U.
   IF FILE-INFO:FULL-PATHNAME <> ? THEN
      rules_d = FILE-INFO:FULL-PATHNAME.
   ELSE DO:
       MESSAGE "Cannot find prolint/rules/rules.d"
               VIEW-AS ALERT-BOX ERROR.
       RETURN.
   END.

   OUTPUT TO VALUE(rules_d).    
   FOR EACH tt_rules :
     EXPORT tt_rules.
   END.
   OUTPUT CLOSE.

   MESSAGE "rule is now added to prolint/rules/rules.d"
           VIEW-AS ALERT-BOX INFORMATION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

