&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File:         prolint/desktop.w
  Description:  easy access to several Prolint features.
  Author:       Jurjen Dijkstra
  Created:      June 2002.
  ------------------------------------------------------------------------

    Copyright (C) 2002 Jurjen Dijkstra

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
                 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{prolint/core/dlc-version.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE propsrunning AS LOGICAL NO-UNDO INITIAL FALSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 Btn_selectfiles ~
Btn_config Btn_results Btn_current Btn_Open Btn_update Btn_dbsubdir ~
Btn_dbrules Btn_dbsinc Btn_dbsession Btn_NewRule Btn_regression btn_tokenlister ~
Btn_protools Btn_Help lbl_release 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-3 EDITOR-2 EDITOR-1 EDITOR-4 ~
EDITOR-11 EDITOR-5 EDITOR-12 EDITOR-9 EDITOR-10 lbl_release 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_config 
     LABEL "Configure" 
     SIZE 19 BY 1.14.

DEFINE BUTTON Btn_current 
     LABEL "Lint Active AB" 
     SIZE 19 BY 1.14.

DEFINE BUTTON Btn_dbrules 
     LABEL "By Rule" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn_dbsinc 
     LABEL "By includefile" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn_dbsubdir 
     LABEL "By subdir" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn_dbsession 
     LABEL "By session" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn_Help 
     LABEL "Help" 
     SIZE 19 BY 1.14.

DEFINE BUTTON Btn_NewRule 
     LABEL "Add new rule" 
     SIZE 19 BY 1.14.

DEFINE BUTTON Btn_Open 
     LABEL "Lint Open AB" 
     SIZE 19 BY 1.14.

DEFINE BUTTON Btn_protools 
     LABEL "Add to protools" 
     SIZE 19 BY 1.14.

DEFINE BUTTON Btn_regression 
     LABEL "Regression-test" 
     SIZE 19 BY 1.14.

DEFINE BUTTON Btn_results 
     LABEL "Results Window" 
     SIZE 19 BY 1.14.

DEFINE BUTTON Btn_selectfiles 
     LABEL "Lint files..." 
     SIZE 19 BY 1.14.

DEFINE BUTTON btn_tokenlister 
     LABEL "Tokenlister" 
     SIZE 19 BY 1.14.

DEFINE BUTTON Btn_update 
     LABEL "Check for updates" 
     SIZE 19 BY 1.14.

DEFINE VARIABLE EDITOR-1 AS CHARACTER INITIAL "Show the Results Window from where you can do all sorts of things." 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 52 BY 1.29
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE EDITOR-10 AS CHARACTER INITIAL "List the Proparse Tokens of a sourcefile" 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 52 BY 1.43
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE EDITOR-11 AS CHARACTER INITIAL "Lint all sources that are open in Appbuilder" 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 52 BY 1.05
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE EDITOR-12 AS CHARACTER INITIAL "Add a new rule to the list" 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 52 BY .71
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE EDITOR-2 AS CHARACTER INITIAL "Create, modify or delete ~"Profiles~"" 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 52 BY 1.05
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE EDITOR-3 AS CHARACTER INITIAL "Select a profile, select some sourcefiles and run Prolint" 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 52 BY 1.1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE EDITOR-4 AS CHARACTER INITIAL "Lint the source that is currently active in Appbuilder" 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 52 BY 1.05
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE EDITOR-5 AS CHARACTER INITIAL "See if there is a newer Prolint release available on the internet." 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 52 BY 1.29
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE EDITOR-9 AS CHARACTER INITIAL "Run regression-tests for Prolint." 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 52 BY 1.33
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE lbl_release AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 20 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 75 BY 9.29
     BGCOLOR 8 FGCOLOR 8 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 75 BY 5.1
     BGCOLOR 8 FGCOLOR 8 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 75 BY 2
     BGCOLOR 8 FGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_selectfiles AT ROW 2.57 COL 5
     EDITOR-3 AT ROW 2.76 COL 25 NO-LABEL
     Btn_config AT ROW 4.1 COL 5
     EDITOR-2 AT ROW 4.33 COL 25 NO-LABEL
     Btn_results AT ROW 5.52 COL 5
     EDITOR-1 AT ROW 5.52 COL 25 NO-LABEL
     Btn_current AT ROW 6.95 COL 5
     EDITOR-4 AT ROW 7.19 COL 25 NO-LABEL
     Btn_Open AT ROW 8.38 COL 5
     EDITOR-11 AT ROW 8.62 COL 25 NO-LABEL
     EDITOR-5 AT ROW 9.81 COL 25 NO-LABEL
     Btn_update AT ROW 9.86 COL 5
     Btn_dbsubdir AT ROW 12.81 COL 5
     Btn_dbrules AT ROW 12.81 COL 23
     Btn_dbsinc AT ROW 12.81 COL 41
     Btn_dbsession AT ROW 12.81 COL 59
     Btn_NewRule AT ROW 15.52 COL 5
     EDITOR-12 AT ROW 15.76 COL 25 NO-LABEL
     Btn_regression AT ROW 17 COL 5
     EDITOR-9 AT ROW 17 COL 25 NO-LABEL
     btn_tokenlister AT ROW 18.43 COL 5
     EDITOR-10 AT ROW 18.43 COL 25 NO-LABEL
     Btn_protools AT ROW 20.76 COL 39
     Btn_Help AT ROW 20.76 COL 59
     lbl_release AT ROW 21 COL 1 COLON-ALIGNED NO-LABEL
     "Query Prolintdb:" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 11.71 COL 3
          FONT 6
     "Developing Prolint:" VIEW-AS TEXT
          SIZE 22.8 BY .62 AT ROW 14.57 COL 3
          FONT 6
     "Using Prolint:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.57 COL 3
          FONT 6
     RECT-6 AT ROW 2.19 COL 3
     RECT-7 AT ROW 15.19 COL 3
     RECT-8 AT ROW 12.33 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Prolint Desktop"
         HEIGHT             = 21.38
         WIDTH              = 80
         MAX-HEIGHT         = 23.29
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 23.29
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("prolint/images/prolint.ico":U) THEN
    MESSAGE "Unable to load icon: prolint/images/prolint.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-10 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-10:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-11 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-11:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-12 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-12:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-3:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-4:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-5:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR EDITOR-9 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDITOR-9:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Prolint Desktop */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Prolint Desktop */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_config
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_config C-Win
ON CHOOSE OF Btn_config IN FRAME DEFAULT-FRAME /* Configure */
DO:
  RUN prolint/core/lintcfg.w  ("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_current
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_current C-Win
ON CHOOSE OF Btn_current IN FRAME DEFAULT-FRAME /* Lint Active AB */
DO:
  RUN prolint/launch/lintcurrent.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_dbrules
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_dbrules C-Win
ON CHOOSE OF Btn_dbrules IN FRAME DEFAULT-FRAME /* By Rule */
DO:
  IF CONNECTED("prolintdb":U) THEN
     RUN prolint/prolintdb/rulestats.w.
  ELSE 
      MESSAGE "Database prolintdb is not connected." SKIP
              "See help-topic prolintdb to learn about this feature."
              VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_dbsinc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_dbsinc C-Win
ON CHOOSE OF Btn_dbsinc IN FRAME DEFAULT-FRAME /* By includefile */
DO:
  IF CONNECTED("prolintdb":U) THEN
     RUN prolint/prolintdb/incstats.w.
  ELSE 
      MESSAGE "Database prolintdb is not connected." SKIP
              "See help-topic prolintdb to learn about this feature."
              VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_dbsubdir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_dbsubdir C-Win
ON CHOOSE OF Btn_dbsubdir IN FRAME DEFAULT-FRAME /* By subdir */
DO:
  IF CONNECTED("prolintdb":U) THEN
     RUN prolint/prolintdb/dirstats.w.
  ELSE 
      MESSAGE "Database prolintdb is not connected." SKIP
              "See help-topic prolintdb to learn about this feature."
              VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_dbsession
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_dbsession C-Win
ON CHOOSE OF Btn_dbsession IN FRAME DEFAULT-FRAME /* By subdir */
DO:
  IF CONNECTED("prolintdb":U) THEN
     RUN prolint/prolintdb/lintstats.p NO-ERROR.
  ELSE 
      MESSAGE "Database prolintdb is not connected." SKIP
              "See help-topic prolintdb to learn about this feature."
              VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
DO:
   RUN prolint/core/openhtml.p( "http://oehive.org/node/240":U ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_NewRule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_NewRule C-Win
ON CHOOSE OF Btn_NewRule IN FRAME DEFAULT-FRAME /* Add new rule */
DO:
  RUN prolint/core/dnewrule.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Open C-Win
ON CHOOSE OF Btn_Open IN FRAME DEFAULT-FRAME /* Lint Open AB */
DO:
  RUN prolint/launch/lintopen.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_protools
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_protools C-Win
ON CHOOSE OF Btn_protools IN FRAME DEFAULT-FRAME /* Add to protools */
DO:
  DEFINE VARIABLE done AS LOGICAL NO-UNDO.
  RUN prolint/core/add2protools.p ("ADD":U, OUTPUT done).
  IF done THEN 
     Btn_protools:VISIBLE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_regression
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_regression C-Win
ON CHOOSE OF Btn_regression IN FRAME DEFAULT-FRAME /* Regression-test */
DO:
  RUN prolint/launch/test.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_results
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_results C-Win
ON CHOOSE OF Btn_results IN FRAME DEFAULT-FRAME /* Results Window */
DO:
  DEFINE VARIABLE hpProperties AS HANDLE NO-UNDO.
  DEFINE VARIABLE progname AS CHARACTER NO-UNDO.
  progname = DYNAMIC-FUNCTION("ProlintProperty", "outputhandlers.resultwindow").
  RUN VALUE(progname) PERSISTENT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_selectfiles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_selectfiles C-Win
ON CHOOSE OF Btn_selectfiles IN FRAME DEFAULT-FRAME /* Lint files... */
DO:
  RUN prolint/launch/start.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_tokenlister
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_tokenlister C-Win
ON CHOOSE OF btn_tokenlister IN FRAME DEFAULT-FRAME /* Proparse launcher */
DO:
  DEFINE VARIABLE filename    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE tokenlister AS HANDLE    NO-UNDO.
  DEFINE VARIABLE go          AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER NO-UNDO INITIAL "linenum,filename":U.

  IF OPSYS = 'UNIX':U THEN DO:  
    RUN proparse/utilities/textprompt.w
        (INPUT "Enter the path and filename of the program to parse and list tokens for.~n",
         INPUT-OUTPUT filename
        ).
    IF RETURN-VALUE = "cancel" THEN
       RETURN.
  
    FILE-INFO:FILE-NAME = filename.
    IF FILE-INFO:FILE-TYPE = ?
    OR INDEX(FILE-INFO:FILE-TYPE, "F":U) = 0 THEN DO:
      MESSAGE "Invalid file name" VIEW-AS ALERT-BOX.
      RETURN.
    END.
  END. /* opsys = unix */
  
  ELSE DO: 
    ASSIGN go = NO.
    SYSTEM-DIALOG GET-FILE filename MUST-EXIST TITLE "Tokenlister" UPDATE go.
    IF NOT go THEN RETURN.
  END. /* opsys <> unix */

  /* Prompt for the node attributes to display */
  RUN prolint/proparse-shim/utilities/attrs_prompt.w (INPUT-OUTPUT attrList).

  &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN HIDE ALL. 
  &ENDIF

  RUN prolint/proparse-shim/utilities/tokenlister.p PERSISTENT SET tokenlister.
  RUN setParseFile IN tokenlister (filename).
  RUN setDispAttr IN tokenlister (attrList).
  RUN main IN tokenlister.
  APPLY "CLOSE":U TO tokenlister.

  &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN VIEW FRAME default-frame. 
  &ENDIF
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_update C-Win
ON CHOOSE OF Btn_update IN FRAME DEFAULT-FRAME /* Check for updates */
DO:
  RUN prolint/core/checkrelease.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   RUN disable_UI.
   PUBLISH "IsProlintPropertiesRunning":U (OUTPUT propsrunning).
   IF propsrunning THEN
      RUN DecrementProlintPropertySubscribers.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  PUBLISH "IsProlintPropertiesRunning":U (OUTPUT propsrunning).
  IF NOT propsrunning THEN
     RUN prolint/core/propsuper.p PERSISTENT.
  RUN IncrementProlintPropertySubscribers.

  DEFINE VARIABLE AlreadyInThere AS LOGICAL NO-UNDO.
  RUN prolint/core/add2protools.p ("CHECK":U, OUTPUT AlreadyInThere).
  IF AlreadyInThere THEN 
     Btn_protools:VISIBLE = FALSE.

  RUN DisplayRelease.

  RUN CheckAdecomm.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckAdecomm C-Win 
PROCEDURE CheckAdecomm :
/*------------------------------------------------------------------------------
  Purpose:     logwin.w requires adecomm.pl to be extracted
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF SEARCH("adecomm/adestds.i":U)=? OR SEARCH("adecomm/peditor.i":U)=? THEN DO:
   /* Only give error if not compiled */
   IF SEARCH("prolint/desktop.r":U)=? THEN DO:
       MESSAGE "Sorry you will need to extract src/adecomm.pl" SKIP 
               "I will now show you a help file with instructions"
               VIEW-AS ALERT-BOX ERROR.
                                
       RUN prolint/core/openhtml.p( "http://oehive.org/node/229":U ).
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRelease C-Win 
PROCEDURE DisplayRelease :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE cRelease  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cLine     AS CHARACTER NO-UNDO.

   FILE-INFO:FILE-NAME = "prolint/core/release.ini".

   INPUT FROM VALUE(file-info:FULL-PATHNAME).
   REPEAT:
      IMPORT UNFORMATTED cLine NO-ERROR.
      IF ENTRY(1, cLine, "=") = "prolint" THEN
         cRelease = ENTRY(2, cLine, "=").
   END.
   INPUT CLOSE.

   lbl_release = "Prolint release ":U + TRIM(cRelease).
   DISPLAY lbl_release WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY EDITOR-3 EDITOR-2 EDITOR-1 EDITOR-4 EDITOR-11 EDITOR-5 EDITOR-12 
          EDITOR-9 EDITOR-10 lbl_release 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 RECT-8 Btn_selectfiles Btn_config Btn_results 
         Btn_current Btn_Open Btn_update Btn_dbsubdir Btn_dbrules Btn_dbsinc Btn_dbsession  
         Btn_NewRule Btn_regression btn_tokenlister Btn_protools Btn_Help 
         lbl_release 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

