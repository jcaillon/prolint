&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

    File:       prolint/outputhandlers/logwin.w

    Copyright (C) 2003 Ildefonzo Arocha - 17.October.2003

    Based on prolint/outputhandlers/logwin8.w by Jurjen Dijkstra
    
    ******* Changes History *******
    * 17.October.2003 - Ildefonzo Arocha
    * User Interface redesign:
      - Added a Tree View to display warnings by source file and ruleID.  
        Treeview nodes are displayed in colors depending on the severity
        of the warning/error.
      - Added a List View to display details of each warning
        ListView sorts upon clicking on column header
      - Replace Text buttons with Image buttons
    * New Functionality
      - Added Code Preview Window
      - Resize Possibility
    * Dropped Functionality
      - Saved files are not saved using the same sort order as displayed
      - Removed all version 8 compatibility &IF's 
    * Notes:
      - Tested with Progress 9.1c, does not work for sure with Versions
        prior to 9, but not sure if it works with 9.1a or 9.1b
      - TreeView and ListView version info: Windows 2000 - SP4

  Update by: Ildefonzo Arocha
  Update on: 12.Mar.2004
    Changes: - Moved code for saving user settings of the CodePreview window
             - Top-Only option is also saved as user setting
             - Bug fix: User pressed "Lint this CU again" and the Code Preview
               window was not getting Refreshed.
             Supported changes in codepreview.w, please check the update comments
             in this file.
-------------------------------------------------------------------------
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{prolint/core/dlc-version.i}

DEFINE TEMP-TABLE tt_lint NO-UNDO
   FIELD ttDescription AS CHAR    LABEL "description":T FORMAT "x(150)":U
   FIELD ttSeverity    AS INTEGER LABEL "severity":T    FORMAT "9":U
   FIELD ttCompUnit    AS CHAR    LABEL "compilation unit":T   FORMAT "x(64)":U
   FIELD ttSource      AS CHAR    LABEL "source":T      FORMAT "x(64)":U
   FIELD ttLine        AS INTEGER LABEL "line":T        FORMAT ">>>>9":U
   FIELD ttRuleID      AS CHAR    LABEL "rule":T        FORMAT "x(15)":U
   INDEX tt1 AS PRIMARY ttCompUnit ttSource ttLine.

DEFINE VARIABLE hStatusbar     AS HANDLE    NO-UNDO.
DEFINE VARIABLE iStatusFields  AS INTEGER   NO-UNDO.
DEFINE VARIABLE numWarnings    AS INTEGER   NO-UNDO.
DEFINE VARIABLE starttime      AS INTEGER   NO-UNDO.
DEFINE VARIABLE elapsedtime    AS INTEGER   NO-UNDO.
DEFINE VARIABLE CurrentProfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE sessionaware   AS LOGICAL   NO-UNDO INITIAL FALSE.

/* filters (WHERE-clause for tt_lint) */
DEFINE VARIABLE fltDesc        AS CHARACTER NO-UNDO INITIAL "*":U.
DEFINE VARIABLE fltMaxSeverity AS INTEGER   NO-UNDO INITIAL 9.
DEFINE VARIABLE fltMinSeverity AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE fltCompUnit    AS CHARACTER NO-UNDO INITIAL "*":U.
DEFINE VARIABLE fltSource      AS CHARACTER NO-UNDO INITIAL "*":U.
DEFINE VARIABLE fltRule        AS CHARACTER NO-UNDO INITIAL "*":U.
DEFINE VARIABLE fltPersistent  AS LOGICAL NO-UNDO INITIAL FALSE.

/* external source editor: */
DEFINE VARIABLE cfgExtEditorProgram    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cfgExtEditorParameters AS CHARACTER NO-UNDO.

/* Holds the OCX handles */
DEFINE VARIABLE chTree        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chList        AS COM-HANDLE NO-UNDO.
/* Code Preview Window */
DEFINE VARIABLE ghCodePreview AS HANDLE     NO-UNDO.

/* Show or Hide the tree */
DEFINE VARIABLE glShowTree    AS LOGICAL NO-UNDO INITIAL TRUE.

DEFINE VARIABLE isLocked AS INTEGER NO-UNDO INITIAL 0.

/* required by triggers */
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE chItem       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE rRowID       AS ROWID      NO-UNDO.
DEFINE VARIABLE lAnswer      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE propsrunning AS LOGICAL NO-UNDO INITIAL FALSE.


/* make this window ADEPersistent */
{prolint/core/_adetool.i}
{prolint/outputhandlers/logwin.i}

&GLOBAL-DEFINE WHERE_TT WHERE tt_lint.ttDescription MATCHES fltDesc ~
                          AND tt_lint.ttSeverity    <=      fltMaxSeverity ~
                          AND tt_lint.ttSeverity    >=      fltMinSeverity ~
                          AND tt_lint.ttCompUnit    MATCHES fltCompunit ~
                          AND tt_lint.ttSource      MATCHES fltSource ~
                          AND tt_lint.ttRuleId      MATCHES fltRule 

&GLOBAL-DEFINE WHERE_TMP WHERE tmp_lint.ttDescription MATCHES fltDesc ~
                           AND tmp_lint.ttSeverity    <=      fltMaxSeverity ~
                           AND tmp_lint.ttSeverity    >=      fltMinSeverity ~
                           AND tmp_lint.ttCompUnit    MATCHES fltCompunit ~
                           AND tmp_lint.ttSource      MATCHES fltSource ~
                           AND tmp_lint.ttRuleId      MATCHES fltRule 

/* OS-COMMAND, but better: */
PROCEDURE ShellExecuteA EXTERNAL "shell32.dll":U PERSISTENT:
   DEFINE INPUT PARAMETER HWND         AS LONG NO-UNDO.
   DEFINE INPUT PARAMETER lpOperation  AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER lpFile       AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER lpParameters AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER lpDirectory  AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER nShowCmd     AS LONG NO-UNDO.
   DEFINE RETURN PARAMETER hInstance   AS LONG NO-UNDO.
END.

PROCEDURE SendMessageA EXTERNAL "user32" :
  DEFINE INPUT  PARAMETER hwnd        AS LONG NO-UNDO.
  DEFINE INPUT  PARAMETER umsg        AS LONG NO-UNDO.
  DEFINE INPUT  PARAMETER wparam      AS LONG NO-UNDO.
  DEFINE INPUT  PARAMETER lparam      AS LONG NO-UNDO.
  DEFINE RETURN PARAMETER ReturnValue AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER hWndLock AS LONG.
  DEFINE RETURN PARAMETER IsLocked AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_codepreview RECT-1 RECT-2 RECT-3 ~
btn_load btn_stats tg_top ed_description ed_compunit Btn_Delete btn_again2 ~
ed_sourcefile btn_edit ed_rule Btn_Filter ed_severity btn_helprule btn_hide ~
Btn_Import Btn_save Btn_Help btn_desktop btn_more 
&Scoped-Define DISPLAYED-OBJECTS tg_top ed_description ed_compunit ~
ed_sourcefile ed_rule ed_severity 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD filterDefined C-Win 
FUNCTION filterDefined RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentSelectionRowID C-Win 
FUNCTION getCurrentSelectionRowID RETURNS ROWID
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFilterWhereExpression C-Win 
FUNCTION getFilterWhereExpression RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame-List AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-List AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-Tree AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-Tree AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_again2 
     LABEL "Lint this c.u. &Again":T 
     SIZE 28 BY 1.

DEFINE BUTTON Btn_codepreview 
     IMAGE-UP FILE "prolint/images/preview.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Show/Hide code preview window" 
     SIZE 4.8 BY 1.14 TOOLTIP "Opens the code preview window"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Delete 
     IMAGE-UP FILE "prolint/images/delete.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "&Delete":T 
     SIZE 4.8 BY 1.14 TOOLTIP "Deletes all displayed warnings".

DEFINE BUTTON btn_desktop 
     IMAGE-UP FILE "prolint/images/desktop.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Desk&Top":T 
     SIZE 4.8 BY 1.14 TOOLTIP "Open ProLint desktop".

DEFINE BUTTON btn_edit 
     LABEL "Open sourcefile in &Editor":T 
     SIZE 28 BY 1.

DEFINE BUTTON Btn_Filter 
     IMAGE-UP FILE "prolint/images/filter.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "&Filter":T 
     SIZE 4.8 BY 1.14 TOOLTIP "Opens filter window".

DEFINE BUTTON Btn_Help 
     IMAGE-UP FILE "prolint/images/help.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help":T 
     SIZE 4.8 BY 1.14 TOOLTIP "HELP !!".

DEFINE BUTTON btn_helprule 
     LABEL "Help on this &Rule":T 
     SIZE 28 BY 1.

DEFINE BUTTON btn_hide 
     IMAGE-UP FILE "prolint/images/hidetree.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Show/Hide Tree" 
     SIZE 4.6 BY 1.14 TOOLTIP "Shows or Hides the Tree pane"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Import 
     IMAGE-UP FILE "prolint/images/openlog.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "&Import log":T 
     SIZE 4.8 BY 1.14 TOOLTIP "Imports an existing log".

DEFINE BUTTON btn_load 
     IMAGE-UP FILE "prolint/images/rollback.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Load" 
     SIZE 4.8 BY 1.14 TOOLTIP "Load results from prolintdb database".

DEFINE BUTTON btn_more 
     IMAGE-UP FILE "prolint/images/prolint.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lint Files":T 
     SIZE 4.8 BY 1.14 TOOLTIP "Lint files".

DEFINE BUTTON Btn_save 
     IMAGE-UP FILE "prolint/images/savelog.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save log As":T 
     SIZE 4.8 BY 1.14 TOOLTIP "Saves the current log to disk".

DEFINE BUTTON btn_stats 
     IMAGE-UP FILE "prolint/images/report.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Stats" 
     SIZE 4.8 BY 1.14 TOOLTIP "Show statistics from prolintdb database".

DEFINE VARIABLE ed_compunit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Compilation unit":T 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1 NO-UNDO.

DEFINE VARIABLE ed_description AS CHARACTER FORMAT "X(256)":U 
     LABEL "Description":T 
     VIEW-AS FILL-IN 
     SIZE 95 BY 1 NO-UNDO.

DEFINE VARIABLE ed_rule AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rule":T 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE ed_severity AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Severity":T 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ed_sourcefile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sourcefile":T 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 128 BY 1.48.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE .4 BY 1.48.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE .4 BY 1.48.

DEFINE VARIABLE tg_top AS LOGICAL INITIAL no 
     LABEL "Top&Only":T 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_codepreview AT ROW 1.24 COL 36
     btn_load AT ROW 1.24 COL 47.2
     btn_stats AT ROW 1.24 COL 53
     tg_top AT ROW 1.33 COL 114.4
     ed_description AT ROW 13.38 COL 23 COLON-ALIGNED
     ed_compunit AT ROW 14.57 COL 23 COLON-ALIGNED
     Btn_Delete AT ROW 1.24 COL 28.6
     btn_again2 AT ROW 14.57 COL 92
     ed_sourcefile AT ROW 15.76 COL 23 COLON-ALIGNED
     btn_edit AT ROW 15.76 COL 92
     ed_rule AT ROW 16.95 COL 23 COLON-ALIGNED
     Btn_Filter AT ROW 1.24 COL 23.6
     ed_severity AT ROW 16.95 COL 75 COLON-ALIGNED
     btn_helprule AT ROW 16.95 COL 92
     btn_hide AT ROW 1.24 COL 41.6
     Btn_Import AT ROW 1.24 COL 18.6
     Btn_save AT ROW 1.24 COL 13.6
     Btn_Help AT ROW 1.24 COL 59
     btn_desktop AT ROW 1.24 COL 1.8
     btn_more AT ROW 1.24 COL 6.8
     RECT-1 AT ROW 1.05 COL 1
     RECT-2 AT ROW 1.05 COL 12.2
     RECT-3 AT ROW 1.05 COL 34.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 128 BY 17.24.


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
         TITLE              = "Prolint results":T
         HEIGHT             = 18.24
         WIDTH              = 128
         MAX-HEIGHT         = 19.81
         MAX-WIDTH          = 200
         VIRTUAL-HEIGHT     = 19.81
         VIRTUAL-WIDTH      = 200
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
  VISIBLE,T,RUN-PERSISTENT                                              */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       ed_compunit:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ed_description:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ed_rule:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ed_severity:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ed_sourcefile:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-Tree ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.62
       COLUMN          = 1
       HEIGHT          = 10.57
       WIDTH           = 46.8
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-List ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.62
       COLUMN          = 47.8
       HEIGHT          = 10.57
       WIDTH           = 81
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame-Tree OCXINFO:CREATE-CONTROL from: {C74190B6-8589-11D1-B16A-00C0F0283628} type: TreeView */
/* CtrlFrame-List OCXINFO:CREATE-CONTROL from: {BDD1F04B-858B-11D1-B16A-00C0F0283628} type: ListView */
      CtrlFrame-Tree:MOVE-AFTER(tg_top:HANDLE IN FRAME DEFAULT-FRAME).
      CtrlFrame-List:MOVE-AFTER(CtrlFrame-Tree).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Prolint results */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Prolint results */
DO:
  RUN saveUserSettings.
  /* This event will close the window and terminate the procedure.  */
  IF VALID-HANDLE( ghCodePreview ) THEN DO:
    RUN destroyObject IN ghCodePreview.
    ghCodePreview = ?.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Prolint results */
DO:
  RUN resizeWindow( {&WINDOW-NAME}:HEIGHT-PIXELS , {&WINDOW-NAME}:WIDTH-PIXELS ).
  IF VALID-HANDLE( ghCodePreview ) THEN
    RUN parentResized IN ghCodePreview.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_again2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_again2 C-Win
ON CHOOSE OF btn_again2 IN FRAME DEFAULT-FRAME /* Lint this c.u. Again */
DO:
  DEFINE VARIABLE cAction AS CHARACTE NO-UNDO.
  IF VALID-HANDLE( ghCodePreview ) THEN DO:
    RUN checkForChanges IN ghCodePreview ( OUTPUT cAction ).
    IF cAction = "CANCEL":U THEN RETURN.
  END.
  RUN BrowseLintCurrent.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_codepreview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_codepreview C-Win
ON CHOOSE OF Btn_codepreview IN FRAME DEFAULT-FRAME /* Show/Hide code preview window */
DO:
  RUN openCodePreview.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete C-Win
ON CHOOSE OF Btn_Delete IN FRAME DEFAULT-FRAME /* Delete */
DO:
   /* Changed this behaviour to delete the results shown on the 
      ListView, I think it makes more sense with this new user interface.
      Any ways, I left the old code commented in case someone prefers
      the old behaviour 

   DEFINE VARIABLE answer AS LOGICAL NO-UNDO INITIAL NO.

   MESSAGE "Do you really want to delete all the results" SKIP 
           "that match the current filter, i.e. all visible results?"
           VIEW-AS ALERT-BOX QUESTION
           BUTTONS YES-NO
           UPDATE answer.

   IF answer = YES THEN DO:
     FOR EACH tt_lint {&WHERE_TT} :
       DELETE tt_lint.
     END.
     /* reset filter and show remaining records: */
     RUN ClearFilter.
     RUN populateTree.
     /* how many records left? */
     numWarnings = 0.
     FOR EACH tt_lint :
         numWarnings = numWarnings + 1.
     END.
     RUN adecomm/_statdsp.p (hStatusBar, 3, STRING(numWarnings) + " warnings":U).
   END.
   */
  MESSAGE "Do you really want to delete all the results" SKIP 
          "displayed on the list ?"
          VIEW-AS ALERT-BOX QUESTION
          BUTTONS YES-NO
          UPDATE lAnswer.

  IF NOT lAnswer OR chList:ListItems:COUNT = 0 THEN   /* Empty ? do nothing */
    RETURN.
  
  DO iCount = 1 TO chList:ListItems:COUNT:
    chItem = chList:ListItems:ITEM( iCount ).
    rRowID = TO-ROWID( chItem:KEY ).
    FIND FIRST tt_lint WHERE ROWID( tt_lint ) = rRowID NO-LOCK NO-ERROR.
    IF AVAILABLE tt_lint THEN DO:
      DELETE tt_lint.
      /* Decrease the number of records */
      numWarnings = numWarnings - 1.
    END.
  END.
  /* reset filter and show remaining records: */
  RUN ClearFilter.
  RUN populateTree.
  RUN adecomm/_statdsp.p (hStatusBar, 3, STRING(numWarnings) + " warnings":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_desktop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_desktop C-Win
ON CHOOSE OF btn_desktop IN FRAME DEFAULT-FRAME /* DeskTop */
DO:
  RUN prolint/desktop.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_edit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_edit C-Win
ON CHOOSE OF btn_edit IN FRAME DEFAULT-FRAME /* Open sourcefile in Editor */
DO:
    RUN BrowseDoubleClick.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Filter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Filter C-Win
ON CHOOSE OF Btn_Filter IN FRAME DEFAULT-FRAME /* Filter */
DO:
   DEFINE VARIABLE numVisible AS INTEGER NO-UNDO INITIAL 0.

   RUN prolint/outputhandlers/dlgfilter.w (INPUT-OUTPUT fltDesc,
                                           INPUT-OUTPUT fltMaxSeverity,
                                           INPUT-OUTPUT fltMinSeverity,
                                           INPUT-OUTPUT fltCompunit,
                                           INPUT-OUTPUT fltSource,
                                           INPUT-OUTPUT fltRule,
                                           INPUT-OUTPUT fltPersistent).

   /* show number of visible warnings in the status bar */
   IF NOT filterDefined() THEN
       RUN adecomm/_statdsp.p (hStatusBar, 3, STRING(numWarnings) + " warnings":U).
   ELSE DO:
       FOR EACH tt_lint {&WHERE_TT} :
           numVisible = numVisible + 1.
       END.
       RUN adecomm/_statdsp.p (hStatusBar, 3, STRING(numVisible) + " / " + STRING(numWarnings)).
   END.

   RUN populateTree.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
DO:
   RUN ShowHelp ("../logwin":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_helprule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_helprule C-Win
ON CHOOSE OF btn_helprule IN FRAME DEFAULT-FRAME /* Help on this Rule */
DO:
    RUN BrowseHelp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_hide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_hide C-Win
ON CHOOSE OF btn_hide IN FRAME DEFAULT-FRAME /* Show/Hide Tree */
DO:
  glShowTree = NOT glShowTree.
  RUN hideTree.
  RUN checkButtonState.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Import
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Import C-Win
ON CHOOSE OF Btn_Import IN FRAME DEFAULT-FRAME /* Import log */
DO:
  RUN Import-tabfile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_load
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_load C-Win
ON CHOOSE OF btn_load IN FRAME DEFAULT-FRAME /* Load */
DO:
  /* check to see if DB is connected, otherwise no-go */
  DEFINE VARIABLE hSessionTbl AS HANDLE NO-UNDO.
  CREATE BUFFER hSessionTbl FOR TABLE "lint_session" NO-ERROR.
  IF NOT CONNECTED("prolintdb") OR NOT VALID-HANDLE(hSessionTbl) THEN DO:
    DELETE OBJECT hSessionTbl NO-ERROR.
    MESSAGE "Database prolintdb is not connected." SKIP
            "See help-topic prolintdb to learn about this feature."
            VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END.
  DELETE OBJECT hSessionTbl no-error.
  
  APPLY "CHOOSE" TO btn_delete IN FRAME {&FRAME-NAME}.
  IF NOT lAnswer /* Defined in delete trigger */ THEN RETURN NO-APPLY.
  RUN prolint/prolintdb/findsessions.w ("Load":U ) NO-ERROR.
  /* this will publish the list of errors */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_more
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_more C-Win
ON CHOOSE OF btn_more IN FRAME DEFAULT-FRAME /* Lint Files */
DO:
   RUN prolint/launch/start.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_save C-Win
ON CHOOSE OF Btn_save IN FRAME DEFAULT-FRAME /* Save log As */
DO:
  RUN export-tabfile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_stats
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_stats C-Win
ON CHOOSE OF btn_stats IN FRAME DEFAULT-FRAME /* Stats */
DO:
  /* check to see if DB is connected, otherwise no-go */
  DEFINE VARIABLE hSessionTbl AS HANDLE NO-UNDO.
  CREATE BUFFER hSessionTbl FOR TABLE "lint_session" NO-ERROR.
  IF NOT CONNECTED("prolintdb") OR NOT VALID-HANDLE(hSessionTbl) THEN DO:
    DELETE OBJECT hSessionTbl NO-ERROR.
    MESSAGE "Database prolintdb is not connected." SKIP
            "See help-topic prolintdb to learn about this feature."
            VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END.
  DELETE OBJECT hSessionTbl no-error.
  
  RUN prolint/prolintdb/lintstats.p NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-List C-Win OCX.ColumnClick
PROCEDURE CtrlFrame-List.ListView.ColumnClick .
/*------------------------------------------------------------------------------
  Purpose:     Fires when user clicks on a column header List View.
  Parameters:  Required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-ColumnHeader AS COM-HANDLE NO-UNDO.

/* If column is already sorted by this key then invert the sort order */
IF chList:SortKey = p-ColumnHeader:INDEX - 1 THEN
  chList:SortOrder = IF chList:SortOrder = 1 THEN 0 ELSE 1.  /* Invert order */
ELSE
  ASSIGN
    chList:SortKEy   = p-ColumnHeader:INDEX - 1
    chList:SortOrder = 0.  /* Ascending */

RUN enableCodePreviewButtons.

RELEASE OBJECT p-ColumnHeader.
p-ColumnHeader  = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-List C-Win OCX.DblClick
PROCEDURE CtrlFrame-List.ListView.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     Fires when user double clicks on a item on the List View.
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

RUN BrowseDoubleClick.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-List C-Win OCX.ItemClick
PROCEDURE CtrlFrame-List.ListView.ItemClick .
/*------------------------------------------------------------------------------
  Purpose:     Fires when user clicks on a item on the List View.
  Parameters:  Required for OCX.
    Item
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-Item AS COM-HANDLE NO-UNDO.
RUN itemClick( p-item ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-Tree
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-Tree C-Win OCX.NodeClick
PROCEDURE CtrlFrame-Tree.TreeView.NodeClick .
/*------------------------------------------------------------------------------
  Purpose:     Fire when the user clicks on a node a the TreeView
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.

RUN showResults( p-Node:TAG ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_top
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_top C-Win
ON VALUE-CHANGED OF tg_top IN FRAME DEFAULT-FRAME /* TopOnly */
DO:
  ASSIGN tg_top.
  c-win:TOP-ONLY = tg_top.
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
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  /* Llock the window so we dont see any ugly window movements */
  IF isLocked NE 0 THEN RUN LockWindowUpdate(FRAME {&FRAME-NAME}:HWND, OUTPUT isLocked).

  RUN enable_UI. 
    
  /* Set the smallest window size */
  ASSIGN
    {&WINDOW-NAME}:MIN-WIDTH-PIXELS  = 640
    {&WINDOW-NAME}:MIN-HEIGHT-PIXELS = 383.
    {&WINDOW-NAME}:MAX-HEIGHT-PIXELS = 383.
  /* Store the handles of the OCX controls */
  ASSIGN
    chList = chCtrlFrame-List:ListView
    chTree = chCtrlFrame-Tree:TreeView.
  /* Move the Hide Button to Top */
  btn_Hide:MOVE-TO-TOP().
     
  /* set private-data to a unique value, so Prolint can determine if this window is already running.
     if prolint can't find a window with this private data, it will lauch one. */
  /* Notice logwin8.w and logwin.w both set the same private-data. This is not a bug. */
  c-win:PRIVATE-DATA = "prolint_outputhandler_logwin.w":U.

  RUN adecomm/_status.p (c-win:HANDLE, "40,30,10,20":U, FALSE, ?, OUTPUT hStatusbar, OUTPUT iStatusFields).
  hStatusBar:VISIBLE = YES.

  SUBSCRIBE TO "Prolint_InitializeResults" ANYWHERE.
  SUBSCRIBE TO "Prolint_AddResult" ANYWHERE.
  SUBSCRIBE TO "Prolint_FinalizeResults" ANYWHERE.
  SUBSCRIBE TO "Prolint_Status_action" ANYWHERE.
  SUBSCRIBE TO "Prolint_Status_FileStart" ANYWHERE.
  SUBSCRIBE TO "Prolint_Status_Profile" ANYWHERE.
  SUBSCRIBE TO "Prolint_Status_Progress" ANYWHERE.

  SUBSCRIBE TO "Prolint_Status_StartTimer" ANYWHERE.
  SUBSCRIBE TO "Prolint_Status_StopTimer" ANYWHERE.

  SUBSCRIBE TO "Prolint_SendLogWin_Ed4Win":U ANYWHERE.
  
  RUN loadUserSettings.
  IF isLocked NE 0 THEN RUN LockWindowUpdate(FRAME {&FRAME-NAME}:HWND, OUTPUT isLocked).
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseDoubleClick C-Win 
PROCEDURE BrowseDoubleClick :
/*------------------------------------------------------------------------------
  Purpose:     open sourcefile in external editor
               for example in UltraEdit-32 or Ed4Win
  Parameters:  <none>
------------------------------------------------------------------------------*/
DEFINE VARIABLE hInstance              AS INTEGER    NO-UNDO.
              
  IF cfgExtEditorProgram="":U THEN 
     RUN ReadCfgEditor.

  IF cfgExtEditorProgram="":U THEN DO:
     {&_proparse_ prolint-nowarn(message)}
     MESSAGE "No external editor specified. See help for prolint/settings/exteditor.cfg":T
             VIEW-AS ALERT-BOX.
     RETURN.
  END.

  FIND FIRST tt_lint WHERE ROWID( tt_lint ) = getCurrentSelectionRowID() NO-LOCK NO-ERROR.
  
  IF AVAILABLE tt_lint THEN DO:
     FILE-INFO:FILE-NAME = tt_lint.ttSource.
     RUN ShellExecuteA (0,
                        "open":U, 
                        SUBSTITUTE(cfgExtEditorProgram,FILE-INFO:FULL-PATHNAME,STRING(tt_lint.ttLine)),
                        SUBSTITUTE(cfgExtEditorParameters,FILE-INFO:FULL-PATHNAME,STRING(tt_lint.ttLine)),
                        "",
                        1,
                        OUTPUT hInstance).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseHelp C-Win 
PROCEDURE BrowseHelp :
/*------------------------------------------------------------------------------
  Purpose:     show help for the current browse row
------------------------------------------------------------------------------*/
  FIND FIRST tt_lint WHERE ROWID( tt_lint ) = getCurrentSelectionRowID() NO-LOCK NO-ERROR.
  
  IF NOT AVAILABLE tt_lint THEN
     RUN ShowHelp ("../logwin":U).
  ELSE 
     RUN ShowHelp (tt_lint.ttRuleID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseLintCurrent C-Win 
PROCEDURE BrowseLintCurrent :
/*------------------------------------------------------------------------------
  Purpose:     lint the compilation unit on the current browse row again
------------------------------------------------------------------------------*/
  FIND FIRST tt_lint WHERE ROWID( tt_lint ) = getCurrentSelectionRowID() NO-LOCK NO-ERROR.
  IF AVAILABLE tt_lint THEN
     RUN prolint/core/prolint.p (ed_compunit, ?, CurrentProfile, FALSE).
  ELSE 
     RUN prolint/launch/start.p.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkButtonState C-Win 
PROCEDURE checkButtonState :
/*------------------------------------------------------------------------------
  Purpose:     Checks all button states
  Parameters:  <none>
  Notes:       This procedure was initially meant to "set" the button
               pushed status.  It works fine, it only behaves strange
               when the button's FLAT property is set, because of some
               reason the "Pushed" state is lost after you hover over
               a pushed button, and gets reset to "un-pushed"
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  RUN toggleButton( Btn_Filter:HANDLE , filterDefined() ).
  
  RUN toggleButton( btn_CodePreview:HANDLE , VALID-HANDLE( ghCodePreview ) ).
  RUN toggleButton( btn_Hide:HANDLE , glShowTree ).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearFilter C-Win 
PROCEDURE ClearFilter :
/*------------------------------------------------------------------------------
  Purpose:     Reset the WHERE-clause; show all warnings
------------------------------------------------------------------------------*/

   ASSIGN 
      fltDesc        = "*":U
      fltMaxSeverity = 9
      fltMinSeverity = 0
      fltCompUnit    = "*":U
      fltSource      = "*":U
      fltRule        = "*":U.

   RUN adecomm/_statdsp.p (hStatusBar, 3, STRING(numWarnings) + " warnings":U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE colorNode C-Win 
PROCEDURE colorNode :
/*------------------------------------------------------------------------------
  Purpose:     Colors a node depending on its severity, the node past as parameter
               must always be the last child of the tree structure
  Parameters:  <none>
  Notes:       Recursive procedure !
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pchNode    AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE chNode    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE iSeverity AS INTEGER    NO-UNDO.

chNode = pchNode.
DO WHILE VALID-HANDLE( chNode ):
  iSeverity = INTEGER( ENTRY( 3 , chNode:TAG , "¦":U ) ).
  IF iSeverity >= {&NODE_HIGH_START_LIMIT}  THEN 
    ASSIGN chNode:ForeColor = {&NODE_HIGH_COLOR}
           chNode:Bold      = TRUE.  /* Bold HIGH warnings for easy display */
  ELSE
    IF iSeverity >= {&NODE_MED_START_LIMIT} THEN chNode:ForeColor = {&NODE_MEDIUM_COLOR}.
    ELSE
      IF iSeverity <  {&NODE_MED_START_LIMIT} THEN chNode:ForeColor = {&NODE_LOW_COLOR}.
  
  IF chNode:Children <> 0 THEN
    RUN colorNode( chNode:Child ).

  chNode = chNode:NEXT.
END.

RELEASE OBJECT chNode  NO-ERROR.
RELEASE OBJECT pchNode NO-ERROR.
ASSIGN chNode = ? pchNode = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "logwin.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame-List = CtrlFrame-List:COM-HANDLE
    UIB_S = chCtrlFrame-List:LoadControls( OCXFile, "CtrlFrame-List":U)
    CtrlFrame-List:NAME = "CtrlFrame-List":U
    chCtrlFrame-Tree = CtrlFrame-Tree:COM-HANDLE
    UIB_S = chCtrlFrame-Tree:LoadControls( OCXFile, "CtrlFrame-Tree":U)
    CtrlFrame-Tree:NAME = "CtrlFrame-Tree":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "logwin.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createRuleNodes C-Win 
PROCEDURE createRuleNodes :
/*------------------------------------------------------------------------------
  Purpose:     Creates all childs of the "Rules" node.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER        pchMainNode   AS COM-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER        pcCompUnit    AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER piKeyNum      AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER       piMaxSeverity AS INTEGER    NO-UNDO.

DEFINE VARIABLE chSourceNode      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chRuleNode        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chTempNode        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE cBaseWhere        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBreakGroupQuery  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBreakDetailQuery AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTemp             AS CHARACTER  NO-UNDO.

DEFINE BUFFER tt_lint## FOR tt_lint.

DEFINE VARIABLE hGroupQuery  AS HANDLE NO-UNDO.
DEFINE VARIABLE hDetailQuery AS HANDLE NO-UNDO.

DEFINE VARIABLE iMaxSourceSeverity AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxRuleSeverity   AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxNodeSeverity   AS INTEGER NO-UNDO.

&SCOPED SORT-EXP  BY tt_lint.ttSource  BY tt_lint.ttLine
&SCOPED SORT-EXP1 BY tt_lint.ttRuleID  BY tt_lint.ttSource BY tt_lint.ttLine

cBaseWhere = getFilterWhereExpression( ).

/* Add Rules */
ASSIGN 
  chSourceNode     = chTree:Nodes:ADD( pchMainNode:INDEX , 4 , "KEY=":U + STRING( piKeyNum ) , "Rule" )
  chSourceNode:TAG = SUBSTITUTE( "tt_lint.ttCompUnit = '&1' {&SORT-EXP1}¦ttCompUnit¦0":U , pcCompUnit )
  piKeyNum         = piKeyNum + 1
  iMaxNodeSeverity = 0
  cBreakGroupQuery = ?
  iMaxRuleSeverity = 0.

/* Now add all rules for the Complation Unit */
CREATE QUERY hGroupQuery.
hGroupQuery:SET-BUFFERS( BUFFER tt_lint:HANDLE ).
hGroupQuery:QUERY-PREPARE( SUBSTITUTE( "FOR EACH tt_lint 
                                           WHERE tt_lint.ttCompUnit = '&1' AND &2 
                                              BY tt_lint.ttRuleID" , pcCompUnit , cBaseWhere ) ).
hGroupQuery:QUERY-OPEN().
hGroupQuery:GET-NEXT( NO-LOCK ).

DO WHILE ( NOT hGroupQuery:QUERY-OFF-END ):
  
  IF cBreakGroupQuery <> tt_lint.ttRuleID THEN DO:
    cBreakGroupQuery = tt_lint.ttRuleID.
    
    /* Now add all sources to the Complation Unit */
    ASSIGN
      chRuleNode = chTree:Nodes:ADD( chSourceNode:INDEX , 4 , "KEY=":U + STRING( piKeyNum ) , tt_lint.ttRuleID )
      chRuleNode:TAG = SUBSTITUTE( "tt_lint.ttCompUnit = '&1' AND tt_lint.ttRuleiD = '&2' {&SORT-EXP1}":U , tt_lint.ttCompUnit , tt_lint.ttRuleID ) + 
                       "¦ttCompUnit,ttRuleId¦0":U
      piKeyNum        = piKeyNum + 1.

    CREATE QUERY hDetailQuery.
    hDetailQuery:SET-BUFFERS( BUFFER tt_lint##:HANDLE ).
    hDetailQuery:QUERY-PREPARE( SUBSTITUTE( "FOR EACH tt_lint## 
                                                WHERE tt_lint##.ttCompUnit = '&1'  
                                                  AND tt_lint##.ttRuleiD = '&2' AND &3 
                                                   BY tt_lint##.ttSource 
                                                   BY tt_lint##.ttLine" , 
                                            tt_lint.ttCompUnit , tt_lint.ttRuleID ,  cBaseWhere ) ).
    hDetailQuery:QUERY-OPEN().
    hDetailQuery:GET-NEXT( NO-LOCK ).
    cBreakDetailQuery  = ?.
    iMaxSourceSeverity = 0.
    
    /* Create the rule nodes for each source */
    DO WHILE ( NOT hDetailQuery:QUERY-OFF-END ):
      /* Create the source nodes for each rule */
      IF cBreakDetailQuery <> tt_lint##.ttSource THEN DO:
        ASSIGN
          cBreakDetailQuery  = tt_lint##.ttSource
          chTempNode         = chTree:Nodes:ADD( chRuleNode:INDEX , 4 , "KEY=":U + STRING( piKeyNum ) , tt_lint##.ttSource )
          chTempNode:TAG     = SUBSTITUTE( "tt_lint.ttCompUnit = '&1' AND tt_lint.ttRuleID = '&2' AND tt_lint.ttSource = '&3' {&SORT-EXP1}":U , tt_lint##.ttCompUnit , tt_lint##.ttRuleID , tt_lint##.ttSource ) +
                              "¦ttCompUnit,ttRuleId,ttSource¦":U + STRING( tt_lint##.ttSeverity )
          iMaxSourceSeverity = MAXIMUM( tt_lint##.ttSeverity , iMaxSourceSeverity ).
          piKeyNum           = piKeyNum + 1.
      END.
      hDetailQuery:GET-NEXT( NO-LOCK ).
    END.
    IF VALID-HANDLE( hDetailQuery ) THEN DELETE OBJECT hDetailQuery.
    
    ASSIGN
      iMaxRuleSeverity           = MAXIMUM( INT( ENTRY( 3 , chRuleNode:TAG , "¦":U ) ) , iMaxSourceSeverity )
      iMaxNodeSeverity           = MAXIMUM( iMaxNodeSeverity , iMaxRuleSeverity )
      cTemp                      = chRuleNode:Tag
      ENTRY( 3 , cTemp , "¦":U ) = STRING( iMaxRuleSeverity )
      chRuleNode:TAG             = cTemp.
  
  END.
  hGroupQuery:GET-NEXT( NO-LOCK ).
END. /* DO WHILE hGroupQuery */
IF VALID-HANDLE( hGroupQuery ) THEN DELETE OBJECT hGroupQuery.

ASSIGN
  cTemp                      = chSourceNode:Tag
  ENTRY( 3 , cTemp , "¦":U ) = STRING( iMaxNodeSeverity )
  chSourceNode:TAG           = cTemp
  /* Set the main node color */
  cTemp                      = pchMainNode:TAG
  ENTRY( 3 , cTemp , "¦":U ) = STRING( MAXIMUM( INTEGER( ENTRY( 3 , pchMainNode:tag , "¦":U ) ) , iMaxNodeSeverity ) )
  pchMainNode:TAG            = cTemp
  piMaxSeverity              = INT( ENTRY( 3 , cTemp , "¦":U ) ).

RELEASE OBJECT chSourceNode NO-ERROR.
RELEASE OBJECT chRuleNode   NO-ERROR.
RELEASE OBJECT chTempNode   NO-ERROR.
ASSIGN 
  chSourceNode = ?
  chRuleNode   = ?
  chTempNode   = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createSourceNodes C-Win 
PROCEDURE createSourceNodes :
/*------------------------------------------------------------------------------
  Purpose:     Creates all childs of the "Source" node.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER        pchMainNode   AS COM-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER        pcCompUnit    AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER piKeyNum      AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER       piMaxSeverity AS INTEGER    NO-UNDO.

DEFINE VARIABLE chSourceNode      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chRuleNode        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chTempNode        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE cBaseWhere        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBreakGroupQuery  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBreakDetailQuery AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTemp             AS CHARACTER  NO-UNDO.

DEFINE BUFFER tt_lint## FOR tt_lint.

DEFINE VARIABLE hGroupQuery  AS HANDLE NO-UNDO.
DEFINE VARIABLE hDetailQuery AS HANDLE NO-UNDO.

DEFINE VARIABLE iMaxRuleSeverity    AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxSourceSeverity  AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxNodeSeverity    AS INTEGER NO-UNDO.

&SCOPED SORT-EXP  BY tt_lint.ttSource  BY tt_lint.ttLine
&SCOPED SORT-EXP1 BY tt_lint.ttRuleID  BY tt_lint.ttSource BY tt_lint.ttLine

cBaseWhere = getFilterWhereExpression( ).

/* Add Source */
ASSIGN 
  chSourceNode       = chTree:Nodes:ADD( pchMainNode:INDEX , 4 , "KEY=":U + STRING( piKeyNum ) ,"Source" )
  chSourceNode:TAG   = SUBSTITUTE( "tt_lint.ttCompUnit = '&1' {&SORT-EXP}¦ttCompUnit¦0":U , pcCompUnit )
  piKeyNum           = piKeyNum + 1
  cBreakGroupQuery   = ?
  iMaxNodeSeverity   = 0
  iMaxSourceSeverity = 0.

CREATE QUERY hGroupQuery.
hGroupQuery:SET-BUFFERS( BUFFER tt_lint:HANDLE ).
hGroupQuery:QUERY-PREPARE( SUBSTITUTE( "FOR EACH tt_lint 
                                           WHERE tt_lint.ttCompUnit = '&1' AND &2 
                                              BY tt_lint.ttSource":U , pcCompUnit , cBaseWhere ) ).
hGroupQuery:QUERY-OPEN().
hGroupQuery:GET-NEXT( NO-LOCK ).

DO WHILE ( NOT hGroupQuery:QUERY-OFF-END ):
  /* Now add all sources to the Complation Unit */
  
  IF cBreakGroupQuery <> tt_lint.ttSource THEN DO:
    cBreakGroupQuery = tt_lint.ttSource.
    
    ASSIGN
      chRuleNode = chTree:Nodes:ADD( chSourceNode:INDEX , 4 , "KEY=":U + STRING( piKeyNum ) , tt_lint.ttSource )
      chRuleNode:TAG = SUBSTITUTE( "tt_lint.ttCompUnit = '&1' AND tt_lint.ttSource = '&2' {&SORT-EXP}":U , tt_lint.ttCompUnit , tt_lint.ttSource ) +
                       "¦ttCompUnit,ttSource¦0":U
      piKeyNum        = piKeyNum + 1.

    CREATE QUERY hDetailQuery.
    hDetailQuery:SET-BUFFERS( BUFFER tt_lint##:HANDLE ).
    hDetailQuery:QUERY-PREPARE( SUBSTITUTE( "FOR EACH tt_lint## 
                                                WHERE tt_lint##.ttCompUnit = '&1'  
                                                  AND tt_lint##.ttSource = '&2' AND &3 
                                                   BY tt_lint##.ttRuleID BY ttLine" , 
                                            tt_lint.ttCompUnit , 
                                            tt_lint.ttSource ,  
                                            cBaseWhere ) ).
    hDetailQuery:QUERY-OPEN().
    hDetailQuery:GET-NEXT( NO-LOCK ).
    cBreakDetailQuery  = ?.
    iMaxRuleSeverity = 0.
    
    /* Create the rule nodes for each source */
    DO WHILE ( NOT hDetailQuery:QUERY-OFF-END ):
      IF cBreakDetailQuery <> tt_lint##.ttRuleID THEN DO:
        ASSIGN
          cBreakDetailQuery  = tt_lint##.ttRuleID
          chTempNode         = chTree:Nodes:ADD( chRuleNode:INDEX , 4 , "KEY=":U + STRING( piKeyNum ) , tt_lint##.ttRuleID )
          chTempNode:TAG     = SUBSTITUTE( "tt_lint.ttCompUnit = '&1' AND tt_lint.ttSource = '&2' AND tt_lint.ttRuleID = '&3' {&SORT-EXP}":U , tt_lint##.ttCompUnit , tt_lint##.ttSource , tt_lint##.ttRuleID ) + 
                               "¦ttCompUnit,ttSource,ttRuleId¦":U + STRING( tt_lint##.ttSeverity )
          iMaxRuleSeverity   = MAXIMUM( tt_lint##.ttSeverity , iMaxRuleSeverity ).
          piKeyNum           = piKeyNum + 1.
      END.
      hDetailQuery:GET-NEXT( NO-LOCK ).
    END.
    
    ASSIGN
      iMaxSourceSeverity         = MAXIMUM( INT( ENTRY( 3 , chRuleNode:TAG , "¦":U ) ) , iMaxRuleSeverity )
      iMaxNodeSeverity           = MAXIMUM( iMaxNodeSeverity , iMaxSourceSeverity )
      cTemp                      = chRuleNode:Tag
      ENTRY( 3 , cTemp , "¦":U ) = STRING( iMaxSourceSeverity )
      chRuleNode:TAG             = cTemp.
    
    IF VALID-HANDLE( hDetailQuery ) THEN DELETE OBJECT hDetailQuery.
  END.
  hGroupQuery:GET-NEXT( NO-LOCK ).
END. /* DO WHILE hGroupQuery */
IF VALID-HANDLE( hGroupQuery ) THEN DELETE OBJECT hGroupQuery.

ASSIGN
  cTemp                      = chSourceNode:Tag
  ENTRY( 3 , cTemp , "¦":U ) = STRING( iMaxNodeSeverity )
  chSourceNode:TAG           = cTemp
  /* Set the main node color */
  cTemp                      = pchMainNode:TAG
  ENTRY( 3 , cTemp , "¦":U ) = STRING( MAXIMUM( INTEGER( ENTRY( 3 , pchMainNode:tag , "¦":U ) ) , iMaxNodeSeverity ) )
  pchMainNode:TAG            = cTemp
  piMaxSeverity              = INT( ENTRY( 3 , cTemp , "¦":U ) ).

RELEASE OBJECT chSourceNode NO-ERROR.
RELEASE OBJECT chRuleNode   NO-ERROR.
RELEASE OBJECT chTempNode   NO-ERROR.
ASSIGN 
  chSourceNode = ?
  chRuleNode   = ?
  chTempNode   = ?.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableCodePreviewButtons C-Win 
PROCEDURE enableCodePreviewButtons :
/*------------------------------------------------------------------------------
  Purpose:     This procedure enables and disables the first/previous/next and last
               buttons of the code preview window.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE chCurrentItem AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE iCurrentIndex AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTotalItems   AS INTEGER    NO-UNDO.

DEFINE VARIABLE lEnabled      AS LOGICAL    NO-UNDO EXTENT 4.  /* Each element indicates if enable or disabe */

IF NOT VALID-HANDLE( ghCodePreview ) THEN
  RETURN.

ASSIGN
  lEnabled      = TRUE                  /* Set all to enabled by default   */
  chCurrentItem = chList:SelectedItem   /* Get the currently selected item */
  iTotalItems   = chList:ListItems:COUNT()
  iCurrentIndex = chCurrentItem:INDEX NO-ERROR.

IF NOT VALID-HANDLE( chCurrentItem ) OR 
   iTotalItems = 0 OR iCurrentIndex = 0 THEN
  lEnabled = FALSE.

IF iCurrentIndex = 1           THEN ASSIGN lEnabled[1] = FALSE lEnabled[2] = FALSE.
IF iCurrentIndex = iTotalItems THEN ASSIGN lEnabled[3] = FALSE lEnabled[4] = FALSE.
IF iTotalItems   = 1           THEN ASSIGN lEnabled = FALSE.

RUN setButtonsState IN ghCodePreview( lEnabled[ 1 ] ,
                                      lEnabled[ 2 ] ,   
                                      lEnabled[ 3 ] ,   
                                      lEnabled[ 4 ] ). 

RELEASE OBJECT chCurrentItem NO-ERROR.
chCurrentItem = ?.
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
  RUN control_load.
  DISPLAY tg_top ed_description ed_compunit ed_sourcefile ed_rule ed_severity 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Btn_codepreview RECT-1 RECT-2 RECT-3 btn_load btn_stats tg_top 
         ed_description ed_compunit Btn_Delete btn_again2 ed_sourcefile 
         btn_edit ed_rule Btn_Filter ed_severity btn_helprule btn_hide 
         Btn_Import Btn_save Btn_Help btn_desktop btn_more 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Export-tabfile C-Win 
PROCEDURE Export-tabfile :
/*------------------------------------------------------------------------------
  Purpose:   export tt_lint to a tab-delimited file
------------------------------------------------------------------------------*/
  define variable tabfilename as character no-undo.
  define variable hTabfile    as handle    no-undo.
  define variable dlgOk       as logical   no-undo.

  tabfilename = "prolint.tab":U.
  system-dialog get-file tabfilename
       filters "tab-delimited":U "*.tab":U
       ask-overwrite
       return-to-start-dir
       save-as
       use-filename
       update dlgOk.

  if not dlgOk then return.


  run prolint/outputhandlers/tabfile.p persistent set hTabfile.
  run SetLogfilename in hTabfile (tabfilename).

  DEFINE BUFFER tmp_lint FOR tt_lint.
  DEFINE QUERY  qtmp FOR tmp_lint.

  OPEN QUERY qtmp FOR EACH tmp_lint NO-LOCK {&WHERE_TMP} BY ttSource BY ttLine BY ttCompUnit.

  GET FIRST qtmp.
  DO WHILE AVAILABLE tmp_lint :
      RUN Prolint_AddResult IN hTabfile ( tmp_lint.ttCompUnit,
                                          tmp_lint.ttSource,
                                          tmp_lint.ttLine,
                                          tmp_lint.ttDescription,
                                          tmp_lint.ttruleID,
                                          tmp_lint.ttSeverity ).
      GET NEXT qtmp.
  END.

  RUN Prolint_FinalizeResults in hTabfile.
  MESSAGE "export completed":U view-as alert-box.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideTree C-Win 
PROCEDURE hideTree :
/*------------------------------------------------------------------------------
  Purpose:     Hides/Shows the TreeView
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF glShowTree THEN DO:
  ASSIGN
    CtrlFrame-Tree:HIDDEN       = FALSE
    CtrlFrame-List:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS - CtrlFrame-Tree:X - CtrlFrame-Tree:WIDTH-PIXELS.
    CtrlFrame-List:X            = CtrlFrame-Tree:X + CtrlFrame-Tree:WIDTH-PIXELS.
END.
ELSE DO:
  ASSIGN
    CtrlFrame-Tree:HIDDEN       = TRUE
    CtrlFrame-List:X            = CtrlFrame-Tree:X
    CtrlFrame-List:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS .
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Import-tabfile C-Win 
PROCEDURE Import-tabfile :
/*------------------------------------------------------------------------------
  Purpose:   import a previously saved tabfile
------------------------------------------------------------------------------*/
  define variable tabfilename as character no-undo.
  define variable cLine       as character no-undo.
  define variable delim       as character no-undo.
  define variable dlgOk       as logical   no-undo.

  define variable iSourcefile  as integer   no-undo.
  define variable iLinenumber  as integer   no-undo.
  define variable iDescription as integer   no-undo.
  define variable iRule        as integer   no-undo.
  define variable iCompunit    as integer   no-undo.
  define variable iSeverity    as integer   no-undo.


  delim = CHR(9). /* keep in sync with outputhandlers/tabfile.p! */

  /* ask for name/location of tabfile. */
  tabfilename = "prolint.tab":U.
  system-dialog get-file tabfilename
       filters "tab-delimited (*.tab)":U "*.tab":U
       return-to-start-dir
       use-filename
       update dlgOk.

  if not dlgOk then return.

  /* verify if tabfile exists */
  file-info:file-name = tabfilename.
  if file-info:full-pathname = ? then do:
     message substitute("file &1 not found":U, tabfilename) view-as alert-box.
     return.
  end.

  /* clear all current results */
  EMPTY TEMP-TABLE tt_lint.
  numWarnings = ?.

  RUN ClearFilter.

  /* import the file */
  INPUT FROM value(file-info:full-pathname).
  REPEAT :
     import unformatted cLine no-error.
     if numWarnings=? then
        assign
           iSourcefile  = lookup("sourcefile":U,cLine,delim)
           iLineNumber  = lookup("linenumber":U,cLine,delim)
           iDescription = lookup("description":U,cLine,delim)
           iRule        = lookup("rule":U,cLine,delim)
           iCompUnit    = lookup("comp.unit":U,cLine,delim)
           iSeverity    = lookup("severity":U,cLine,delim)
           numWarnings  = 0
           .
     else do:
        numWarnings = numWarnings + 1.
        create tt_lint.
        assign tt_lint.ttCompUnit    = entry(iCompUnit,cLine,delim)
               tt_lint.ttSource      = entry(iSourcefile,cLine,delim)
               tt_lint.ttLine        = integer(entry(iLineNumber,cLine,delim))
               tt_lint.ttDescription = entry(iDescription,cLine,delim)
               tt_lint.ttruleID      = entry(iRule,cLine,delim)
               tt_lint.ttSeverity    = integer(entry(iSeverity,cLine,delim)).
     end.
  END.
  INPUT CLOSE.

  RUN adecomm/_statdsp.p (hStatusBar, 3, STRING(numWarnings) + " warnings":U).

  RUN populateTree.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE itemClick C-Win 
PROCEDURE itemClick :
/*------------------------------------------------------------------------------
  Purpose:     This event fires when the user selects an item from the ListView
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pchItem AS COM-HANDLE NO-UNDO.

IF NOT VALID-HANDLE( pchItem ) THEN RETURN.

DEFINE VARIABLE rRowID AS ROWID NO-UNDO.

rRowID = TO-ROWID( pchItem:KEY ).

/* Display the selected record */
FIND tt_lint WHERE ROWID( tt_lint ) = rRowID NO-LOCK NO-ERROR.

IF AVAILABLE tt_lint THEN DO:
  ASSIGN 
      ed_description = tt_lint.ttDescription
      ed_compunit    = tt_lint.ttCompUnit
      ed_sourcefile  = SUBSTITUTE("&2 (line &1)":T, STRING(tt_lint.ttLine),tt_lint.ttSource)
      ed_rule        = tt_lint.ttRuleID
      ed_severity    = tt_lint.ttSeverity.
  IF VALID-HANDLE( ghCodePreview ) THEN DO:
    RUN loadFile IN ghCodePreview( tt_lint.ttSource ).
    RUN repositionToLine IN ghCodePreview( tt_lint.ttLine , tt_lint.ttDescription , tt_lint.ttSeverity ).
    RUN enableCodePreviewButtons.
  END.
END.
ELSE 
ASSIGN
    ed_description = ""
    ed_compunit    = ""
    ed_sourcefile  = ""
    ed_rule        = ""
    ed_severity    = 0.

DISPLAY 
    ed_description
    ed_compunit
    ed_sourcefile
    ed_rule
    ed_severity
WITH FRAME {&FRAME-NAME}.

RELEASE OBJECT pchItem NO-ERROR.
pchItem = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadUserSettings C-Win 
PROCEDURE loadUserSettings :
/*------------------------------------------------------------------------------
  Purpose:     Saves all user settings to the Registry
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cShowTree          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShowCodePreview   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cX                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cY                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSizeX             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSizeY             AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCodePreviewX     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCodePreviewY     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCodePreviewSizeX AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCodePreviewSizeY AS CHARACTER NO-UNDO.

DEFINE VARIABLE cFilterPersistent  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilterDesc        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilterMaxSeverity AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilterMinSeverity AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilterCompunit    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilterSource      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilterRule        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTopOnly           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWindowState       AS CHARACTER NO-UNDO.

LOAD "SOFTWARE":U BASE-KEY "HKEY_CURRENT_USER":U.
USE "SOFTWARE":U.

/* Display Properties */
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "ShowTree"        VALUE cShowTree    .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "ShowCodePreview" VALUE cShowCodePreview .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "TopOnly"         VALUE cTopOnly.

/* Main Window Properties */
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "X"           VALUE cX      .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "Y"           VALUE cY      .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "SizeX"       VALUE cSizeX  .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "SizeY"       VALUE cSizeY  .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "WindowState" VALUE cWindowState.

/* Filter Settings */
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterPersistent"  VALUE cFilterPersistent  .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterDesc"        VALUE cFilterDesc        .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterMaxSeverity" VALUE cFilterMaxSeverity .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterMinSeverity" VALUE cFilterMinSeverity .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterCompunit"    VALUE cFilterCompunit    .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterSource"      VALUE cFilterSource      .
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterRule"        VALUE cFilterRule        .

UNLOAD "SOFTWARE".

/* values are empty if the section did not exist yet. In that case return. */
IF cShowTree = "" OR cShowTree = ? THEN RETURN.

/* Now that we have the values, apply the user settings. */
/* Reposition the Main Window */
ASSIGN
  {&WINDOW-NAME}:X = INTEGER( cX )
  {&WINDOW-NAME}:Y = INTEGER( cY ).

/* Set the main Window Size */
IF INTEGER( cSizeX ) > 0 AND
   INTEGER( cSizeY ) > 0 THEN DO:
  ASSIGN 
    {&WINDOW-NAME}:WIDTH-PIXELS  = INTEGER( cSizeX )
    {&WINDOW-NAME}:HEIGHT-PIXELS = INTEGER( cSizeY ).
  RUN resizeWindow( INTEGER( cSizeY ) , INTEGER( cSizeX ) ).
END.

/* Show/Hide the Tree */
ASSIGN
  glShowTree        = ( cShowTree = "1":U ).
RUN hideTree.

IF cShowCodePreview = "1":U THEN DO:
  RUN openCodePreview.
END.

/* Filter Settings */
ASSIGN
  fltPersistent = ( cFilterPersistent = "1":U ).

IF fltPersistent THEN
   ASSIGN
     fltDesc        = cFilterDesc
     fltMaxSeverity = INTEGER( cFilterMaxSeverity )
     fltMinSeverity = INTEGER( cFilterMinSeverity )
     fltCompunit    = cFilterCompunit
     fltSource      = cFilterSource
     fltRule        = cFilterRule.

/* Top-Only Option */
tg_top = ( cTopOnly = "YES" ).
DISPLAY tg_top WITH FRAME {&FRAME-NAME}.
APPLY "value-changed":U TO tg_top.

/* Set the window to the previous state */
IF TRIM( cWindowState ) <> "" AND cWindowState <> ? THEN
  {&WINDOW-NAME}:WINDOW-STATE = INTEGER( cWindowState ).

RUN checkButtonState.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openCodePreview C-Win 
PROCEDURE openCodePreview :
/*------------------------------------------------------------------------------
  Purpose:     Opens the Code Preview Window
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE chCurrentItem AS COM-HANDLE NO-UNDO.

  /* If Already open we close it */
  IF VALID-HANDLE( ghCodePreview ) THEN DO:
    RUN destroyObject IN ghCodePreview.
    ghCodePreview = ?.
  END.
  ELSE DO:
    RUN prolint/outputhandlers/codepreview.w PERSISTENT SET ghCodePreview ( THIS-PROCEDURE ).
    SUBSCRIBE TO "Prolint_codePreviewClosed" IN ghCodePreview.

    /* If an item is selected display the source of that item */
    chCurrentItem = chList:SelectedItem.
    IF VALID-HANDLE( chCurrentItem ) THEN DO:
      RUN itemClick( chCurrentItem ).
    END.
  END.

  RUN checkButtonState.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populateTree C-Win 
PROCEDURE populateTree :
/*------------------------------------------------------------------------------
  Purpose:     Populates the contents of tt_lint on the tree view.
               This procedure applies the current filter to tt_lint.
  Parameters:  <none>
  Notes:       The TAG property of each node contains a pipe delimited list of the following entries:
                1.) Where clause for that node
                2.) Columns to exclude from the list view
                3.) Maximum severtiy found for all childs of that node
                
  Structure of the tree:
  
  <ProLint>
    + <CompilationUnit 1>
          +- Source
            +- <Source1>
                 +- <Rule1>
                 +- <Rule2>
                 +- <Rule ...n>
            +- <Source2>
            +- <Source ... n>
          +- Rule
            +- <Rule1 >
                +- <Source1>
                +- <Source2>
                +- <Source ... n>
            +- <Rule2 >
            +- <Rule ...n >
    +- <CompilationUnit ... n >
------------------------------------------------------------------------------*/
DEFINE VARIABLE chMainNode        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chRootNode        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE iKeyNum           AS INTEGER    NO-UNDO INITIAL 1.
DEFINE VARIABLE cBaseWhere        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBreakMainQuery   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iMaxRootSeverity  AS INTEGER    NO-UNDO INITIAL 0.
DEFINE VARIABLE iMaxSeverity      AS INTEGER    NO-UNDO INITIAL 0.

DEFINE BUFFER tt_lint  FOR tt_lint.

DEFINE VARIABLE hMainQuery   AS HANDLE NO-UNDO.

/* Clear controls */
chTree:Nodes:CLEAR().
chList:ListItems:CLEAR().

/* If the CodePreview window is open we blank its contents */
IF VALID-HANDLE( ghCodePreview ) THEN
  RUN resetObject IN ghCodePreview.

/* Returns the filter's where condition */
cBaseWhere = getFilterWhereExpression().

/* Root Node is always created */
ASSIGN
  chRootNode     = chTree:Nodes:ADD(  , 2 , "KEY=":U + STRING( iKeyNum ) , "ProLint":U )
  chRootNode:TAG = cBaseWhere + "¦¦0":U
  iKeyNum        = iKeyNum + 1.

CREATE QUERY hMainQuery.
hMainQuery:SET-BUFFERS( BUFFER tt_lint:HANDLE ).
hMainQuery:QUERY-PREPARE( SUBSTITUTE( "FOR EACH tt_lint WHERE &1 BY tt_lint.ttCompUnit":U , cBaseWhere  ) ).
hMainQuery:QUERY-OPEN().
hMainQuery:GET-NEXT( NO-LOCK ).
cBreakMainQuery = ?.

DO WHILE( NOT hMainQuery:QUERY-OFF-END ):

  /* BREAK BY - FIRST-OF - wow, didnt do this stuff since my Cobol days :) */
  IF cBreakMainQuery <> tt_lint.ttCompUnit  THEN DO:
    cBreakMainQuery =  tt_lint.ttCompUnit.

    ASSIGN 
      chMainNode     = chTree:Nodes:ADD( chRootNode:INDEX , 4 , "KEY=":U + STRING( iKeyNum ) , tt_lint.ttCompUnit )
      chMainNode:TAG = SUBSTITUTE( "tt_lint.ttCompUnit = '&1' {&SORT-EXP}":U , tt_lint.ttCompUnit ) + 
                       "¦ttCompUnit¦0":U
      iKeyNum        = iKeyNum + 1.
    
    RUN createSourceNodes( chMainNode , tt_lint.ttCompUnit , INPUT-OUTPUT iKeyNum , OUTPUT iMaxSeverity ).
    iMaxRootSeverity = MAX( iMaxRootSeverity , iMaxSeverity ).
    RUN createRuleNodes( chMainNode , tt_lint.ttCompUnit , INPUT-OUTPUT iKeyNum , OUTPUT iMaxSeverity ).
    iMaxRootSeverity = MAX( iMaxRootSeverity , iMaxSeverity ).
  END.
  hMainQuery:GET-NEXT( NO-LOCK ).
END.  /* DO WHILE hMainQuery */

IF VALID-HANDLE( hMainQuery ) THEN DELETE OBJECT hMainQuery.

/* Set Root node max severity */
ASSIGN
  cBaseWhere                      = chRootNode:TAG
  ENTRY( 3 , cBaseWhere , "¦":U ) = STRING( iMaxRootSeverity )
  chRootNode:TAG                  = cBaseWhere.

/* Color the nodes */
IF chTree:Nodes:COUNT > 0 THEN
  RUN colorNode( chTree:Nodes:ITEM(1):Root ).

/* By default we click on the root node to show all results */
RUN CtrlFrame-Tree.TreeView.NodeClick ( chTree:Nodes:ITEM(1):Root ).

RELEASE OBJECT chMainNode   NO-ERROR.
RELEASE OBJECT chRootNode   NO-ERROR.
ASSIGN
  chMainNode   = ?
  chRootNode   = ?.  

/* Set the button states  */
RUN checkButtonState.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_AddResult C-Win 
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

   CREATE tt_lint.
   ASSIGN tt_lint.ttCompUnit    = pCompilationUnit
          tt_lint.ttSource      = pSourcefile
          tt_lint.ttLine        = pLineNumber
          tt_lint.ttDescription = pDescription
          tt_lint.ttruleID      = pRuleID
          tt_lint.ttSeverity    = pSeverity.

   numWarnings = numWarnings + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_codePreviewClosed C-Win 
PROCEDURE Prolint_codePreviewClosed :
/*------------------------------------------------------------------------------
  Purpose:     Fires when the Code Preview window is closed.
               We store the window settings on some global variables
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE( ghCodePreview ) THEN DO:
  RUN destroyObject IN ghCodePreview.
  /* I realized that after allowing editing and display the navigation buttons
     on the code preview window, the main prolint window actually becomes ennoying.
     During my tests, most of the cases I always minimized the result window and
     only used the Code Preview window, after fixing warnings I closed the code
     preview window but because it is minimized, I had to restore and close.
     What I do now, is that if the code preview window is closed and the result
     window is open I close the result window also
  */
  IF {&WINDOW-NAME}:WINDOW-STATE = 2 THEN DO:
    APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_FinalizeResults C-Win 
PROCEDURE Prolint_FinalizeResults :
/*------------------------------------------------------------------------------
  Purpose:     Prolint is ready. Show tt_lint in the browse widget.
               Display status=ready in statusbar
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN adecomm/_statdsp.p (hStatusBar, 1, SUBSTITUTE("total time: &1 msec":T, TRIM(STRING(elapsedtime +  ETIME(NO) - starttime , ">>>,>>>,>>>,>>9":U)))).
  RUN adecomm/_statdsp.p (hStatusBar, 2, "").
  RUN adecomm/_statdsp.p (hStatusBar, 3, STRING(numWarnings) + " warnings":U).

  /* Clear and Build the tree */
  RUN populateTree.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_InitializeResults C-Win 
PROCEDURE Prolint_InitializeResults :
/*------------------------------------------------------------------------------
  Purpose:     start with an empty logfile.
               show in statusbar that prolint is busy
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pClearOutput AS LOGICAL NO-UNDO.
  
  elapsedtime = 0.
  starttime = ETIME(NO).

  IF NOT fltPersistent THEN
     RUN ClearFilter.

  IF pClearOutput THEN DO:
     EMPTY TEMP-TABLE tt_lint.
     numWarnings = 0.
  END.

  RUN adecomm/_statdsp.p (hStatusBAr, 1, "working...":T).
  RUN adecomm/_statdsp.p (hStatusBAr, 3, STRING(numWarnings) + " warnings":U).

END PROCEDURE.
   
   
PROCEDURE Prolint_Status_StartTimer :
   starttime = ETIME(NO).
END PROCEDURE.

PROCEDURE Prolint_Status_StopTimer :
   elapsedtime = ETIME(NO) - starttime.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_SendLogWin_Ed4Win C-Win 
PROCEDURE Prolint_SendLogWin_Ed4Win :
/*------------------------------------------------------------------------------
  Purpose:  send all currently selected records to "Ed for Windows"
            (using the current filter and current sorting)
------------------------------------------------------------------------------*/

  DEFINE VARIABLE hEd4win AS HANDLE NO-UNDO.

  RUN prolint/outputhandlers/ed4win.p PERSISTENT SET hEd4win.

  DEFINE BUFFER tmp_lint FOR tt_lint.
  DEFINE QUERY  qtmp FOR tmp_lint.

  OPEN QUERY qtmp FOR EACH tmp_lint NO-LOCK {&WHERE_TMP} BY ttSource BY ttLine.

  GET FIRST qtmp.
  DO WHILE AVAILABLE tmp_lint :
      RUN Prolint_AddResult IN hEd4win ( tmp_lint.ttCompUnit,
                                         tmp_lint.ttSource,
                                         tmp_lint.ttLine,
                                         tmp_lint.ttDescription,
                                         tmp_lint.ttruleID,
                                         tmp_lint.ttSeverity ).
      GET NEXT qtmp.
  END.

  RUN Prolint_FinalizeResults IN hEd4win.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_Status_action C-Win 
PROCEDURE Prolint_Status_action :
/*------------------------------------------------------------------------------
  Purpose:     show what prolint is doing, in second status panel
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pAction AS CHAR NO-UNDO.

  RUN adecomm/_statdsp.p (hStatusBAr, 2, pAction).
  RUN adecomm/_statdsp.p (hStatusBAr, 3, STRING(numWarnings) + " warnings":U).
  PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_Status_FileStart C-Win 
PROCEDURE Prolint_Status_FileStart :
/*------------------------------------------------------------------------------
  Purpose:     show status in first status panel
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pSourceFile AS CHAR NO-UNDO.

  RUN adecomm/_statdsp.p (hStatusBar, 1, pSourceFile).

  /* in case this sourcefile was already linted earlier, delete its existing results */  
  FOR EACH tt_lint WHERE tt_lint.ttCompUnit = pSourceFile :
      numWarnings = numWarnings - 1.
      DELETE tt_lint.
  END.
    
  PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_Status_Profile C-Win 
PROCEDURE Prolint_Status_Profile :
/*------------------------------------------------------------------------------
  Purpose:     show profile in statusbar
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pProfile AS CHAR NO-UNDO.
                                                
  ASSIGN 
     CurrentProfile = pProfile.                                              
  RUN adecomm/_statdsp.p (hStatusBar, 4, CurrentProfile).
  PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prolint_Status_Progress C-Win 
PROCEDURE Prolint_Status_Progress :
/*------------------------------------------------------------------------------
  Purpose:     show what prolint is doing, in second status panel
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pProgress AS CHAR NO-UNDO.

  RUN adecomm/_statdsp.p (hStatusBAr, 5, pProgress).
  PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadCfgEditor C-Win 
PROCEDURE ReadCfgEditor :
/*------------------------------------------------------------------------------
  Purpose:     find out how to open a sourcefile in an external editor,
               like (for example) Ed4win or UltraEdit 
  Parameters:  <none>
  Notes:       read cfgEditorProgram and cfgEditorParameters from 
               a configuration file or registry 
------------------------------------------------------------------------------*/

  file-info:FILE-NAME = "prolint/settings/exteditor.cfg":U.
  IF file-info:full-pathname<>? THEN DO:
     INPUT FROM VALUE(file-info:FULL-PATHNAME).
       IMPORT UNFORMATTED cfgExtEditorProgram.
       IMPORT UNFORMATTED cfgExtEditorParameters.
     INPUT CLOSE.
  END.             

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repositionTo C-Win 
PROCEDURE repositionTo :
/*------------------------------------------------------------------------------
  Purpose:     Repositions to the selected warning/error.
               This procedure is run when the used presses the
               first/prev/next or last button on the code preview window
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcAction AS CHARACTER NO-UNDO.

DEFINE VARIABLE chCurrentItem AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE iCurrentIndex AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTotalItems   AS INTEGER    NO-UNDO.

ASSIGN
  chCurrentItem = chList:SelectedItem   /* Get the currently selected item */
  iTotalItems   = chList:ListItems:COUNT()
  iCurrentIndex = chCurrentItem:INDEX NO-ERROR.

CASE pcAction:
  WHEN "FIRST" THEN chList:SelectedItem = chList:ListItems:ITEM( 1 ).
  WHEN "PREV"  THEN chList:SelectedItem = chList:ListItems:ITEM( iCurrentIndex - 1 ).
  WHEN "NEXT"  THEN chList:SelectedItem = chList:ListItems:ITEM( iCurrentIndex + 1 ).
  WHEN "LAST"  THEN chList:SelectedItem = chList:ListItems:ITEM( iTotalItems ).
END CASE.

RUN itemClick( chList:SelectedItem ).

RELEASE OBJECT chCurrentItem.
chCurrentItem = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeWindow C-Win 
PROCEDURE resizeWindow :
/*------------------------------------------------------------------------------
  Purpose:     Resizes the window
  Parameters:  New Height-Chars and Width Chars size.
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piNewHeight AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piNewWidth  AS INTEGER NO-UNDO.

DEFINE VARIABLE iCurrentWidth          AS INTEGER NO-UNDO.
DEFINE VARIABLE iCurrentHeight         AS INTEGER NO-UNDO.
DEFINE VARIABLE hFrame                 AS HANDLE  NO-UNDO. /* Avoid typing FRAME {&FRAME-NAME} */
DEFINE VARIABLE iHorizonalResizeFactor AS DECIMAL NO-UNDO.
DEFINE VARIABLE iNewHorizPosition      AS INTEGER NO-UNDO. /* How many pixels to shift the X positons of widgets */

ASSIGN 
  hFrame                 = FRAME {&FRAME-NAME}:HANDLE
  iCurrentWidth          = hFrame:WIDTH-PIXELS
  iCurrentHeight         = hFrame:WIDTH-PIXELS
  iNewHorizPosition      = ( piNewWidth - iCurrentWidth )
  iHorizonalResizeFactor = ( ( iNewHorizPosition * 100.00 ) / iCurrentWidth ) / 100.00.

/* Stretch horizontally */
IF piNewWidth > iCurrentWidth THEN
  ASSIGN
    hFrame:VIRTUAL-WIDTH-PIXELS = piNewWidth
    hFrame:WIDTH-PIXELS         = piNewWidth.

ASSIGN
  /* Note that even if the Tree is hidden it has to be resized !! */
  CtrlFrame-Tree:WIDTH-PIXELS = CtrlFrame-Tree:WIDTH-PIXELS + ( CtrlFrame-Tree:WIDTH-PIXELS * iHorizonalResizeFactor )
  CtrlFrame-List:X            = CtrlFrame-List:X            + ( CtrlFrame-List:X            * iHorizonalResizeFactor )
  CtrlFrame-List:WIDTH-PIXELS = CtrlFrame-List:WIDTH-PIXELS + ( CtrlFrame-List:WIDTH-PIXELS * iHorizonalResizeFactor )
  RECT-1:WIDTH-PIXELS         = RECT-1:WIDTH-PIXELS         + ( RECT-1:WIDTH-PIXELS         * iHorizonalResizeFactor )
  tg_top:X                    = tg_top:X                    + iNewHorizPosition.

IF piNewWidth < iCurrentWidth THEN
  ASSIGN
    hFrame:VIRTUAL-WIDTH-PIXELS = piNewWidth
    hFrame:WIDTH-PIXELS         = piNewWidth.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveUserSettings C-Win 
PROCEDURE saveUserSettings :
/*------------------------------------------------------------------------------
  Purpose:     Saves all user settings to the Registry
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
LOAD "SOFTWARE":U BASE-KEY "HKEY_CURRENT_USER":U.
USE "SOFTWARE":U.

/* Display Properties */
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "ShowTree"        VALUE ( IF glShowTree THEN "1" ELSE "0" )                    NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "ShowCodePreview" VALUE ( IF VALID-HANDLE( ghCodePreview ) THEN "1" ELSE "0" ) NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "TopOnly"         VALUE ( STRING( tg_top,"YES/NO") )                           NO-ERROR.

/* Main Window Properties */
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "X"           VALUE ( STRING( {&WINDOW-NAME}:X ) )              NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "Y"           VALUE ( STRING( {&WINDOW-NAME}:Y ) )              NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "SizeX"       VALUE ( STRING( {&WINDOW-NAME}:WIDTH-PIXELS ) )   NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "SizeY"       VALUE ( STRING( {&WINDOW-NAME}:HEIGHT-PIXELS ) )  NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "WindowState" VALUE ( STRING( {&WINDOW-NAME}:WINDOW-STATE  ) )  NO-ERROR.

/* Filter Settings */
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterPersistent"  VALUE ( IF fltPersistent THEN "1" ELSE "0" ) NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterDesc"        VALUE ( STRING( fltDesc        ) )     NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterMaxSeverity" VALUE ( STRING( fltMaxSeverity ) )     NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterMinSeverity" VALUE ( STRING( fltMinSeverity ) )     NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterCompunit"    VALUE ( STRING( fltCompunit    ) )     NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterSource"      VALUE ( STRING( fltSource      ) )     NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "FilterRule"        VALUE ( STRING( fltRule        ) )     NO-ERROR.
UNLOAD "SOFTWARE".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowHelp C-Win 
PROCEDURE ShowHelp :
/*------------------------------------------------------------------------------
  Purpose:     show help. For easy maintenance we will use separate HTM files 
               instead of a compiled helpfile
  Parameters:  pContext = ruleid
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pContext AS CHAR NO-UNDO.

  DEF VAR ReturnValue AS INTEGER NO-UNDO.
  DEF VAR fullpath    AS CHARACTER NO-UNDO.
                                
  /* try to locate custom help. 
     If not found, then try to locate default help */                              
  FILE-INFO:FILE-NAME = "prolint/custom/help/rules/":U + pContext + ".htm":U.
  IF FILE-INFO:FULL-PATHNAME<>? THEN
     fullpath = file-info:FULL-PATHNAME.
  ELSE
     fullpath = "http://oehive.org/prolint/rules/":U + pContext.

  RUN prolint/core/openhtml.p(fullpath).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showResults C-Win 
PROCEDURE showResults :
/*------------------------------------------------------------------------------
  Purpose:     Shows the selected node data
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcNodeTag AS CHARACTER NO-UNDO.

DEFINE VARIABLE cFilterWhere       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWhereClause       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcludeColumnList AS CHARACTER NO-UNDO.

DEFINE VARIABLE hQuery       AS HANDLE     NO-UNDO.
DEFINE VARIABLE hBuffer      AS HANDLE     NO-UNDO.
DEFINE VARIABLE hField       AS HANDLE     NO-UNDO.
DEFINE VARIABLE chItem       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE cDisplayList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE cString      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iColSizes    AS INTEGER    NO-UNDO EXTENT 20 .   /* Must be set to maximum number of columns */

/* The following three variable contains one entry for each fields of all valid columns, one list contains the field names of tt_list
   and the other contains the labels and the other the alignment of the column, they must be ALL in sync.  Note also that if the structure
   of tt_lint temp-table is changed, this procedure must be properly updated also */
DEFINE VARIABLE cValidColumns AS CHARACTER NO-UNDO INITIAL "ttCompUnit,ttSource,ttDescription,ttLine,ttSeverity,ttRuleID":U.
DEFINE VARIABLE cColumnLabels AS CHARACTER NO-UNDO INITIAL "Compilation Unit,Source,Description,Line,Severity,Rule":U.
DEFINE VARIABLE cColumnAlign  AS CHARACTER NO-UNDO INITIAL "0,0,0,1,1,2":U.

&SCOPED-DEFINE lvwColumnLeft   0
&SCOPED-DEFINE lvwColumnRight  1
&SCOPED-DEFINE lvwColumnCenter 2

cFilterWhere = getFilterWhereExpression( ).

IF NUM-ENTRIES( pcNodeTag , "¦":U ) <> 3 THEN 
  /* Empty ? Show everything */
  ASSIGN
    cWhereClause       = "TRUE":U
    cExcludeColumnList = "":U.
ELSE
  ASSIGN
    cWhereClause       = ENTRY( 1 , pcNodeTag , "¦":U )  /* Extract the where clause from the node tag */
    cExcludeColumnList = ENTRY( 2 , pcNodeTag , "¦":U ). /* Extract what columns not to show */

/* Easier for cleanup */
DO ON ERROR UNDO, LEAVE:
  /* Reset the List View */
  chList:ListItems:CLEAR().
  chList:ColumnHeaders:CLEAR().

  /* If you feel like changing the display order of the columns only move the order of the lines below */
  IF LOOKUP( "ttCompUnit":U    , cExcludeColumnList ) = 0 THEN cDisplayList = cDisplayList + MINIMUM( cDisplayList , "," ) + "ttCompUnit":U.
  IF LOOKUP( "ttSource":U      , cExcludeColumnList ) = 0 THEN cDisplayList = cDisplayList + MINIMUM( cDisplayList , "," ) + "ttSource":U.
  IF LOOKUP( "ttDescription":U , cExcludeColumnList ) = 0 THEN cDisplayList = cDisplayList + MINIMUM( cDisplayList , "," ) + "ttDescription":U.
  IF LOOKUP( "ttLine":U        , cExcludeColumnList ) = 0 THEN cDisplayList = cDisplayList + MINIMUM( cDisplayList , "," ) + "ttLine":U.
  IF LOOKUP( "ttSeverity":U    , cExcludeColumnList ) = 0 THEN cDisplayList = cDisplayList + MINIMUM( cDisplayList , "," ) + "ttSeverity":U.
  IF LOOKUP( "ttRuleID":U      , cExcludeColumnList ) = 0 THEN cDisplayList = cDisplayList + MINIMUM( cDisplayList , "," ) + "ttRuleID":U.
  
  /* Create Columns in the ListView control */
  DO iCount = 1 TO NUM-ENTRIES( cDisplayList ):
    chList:ColumnHeaders:ADD( , "KEY=" + STRING( iCount ) , 
                                ENTRY( LOOKUP( ENTRY( iCount , cDisplayList ) , cValidColumns ) , cColumnLabels  ) ,
                                ,
                                INTEGER( ENTRY( LOOKUP( ENTRY( iCount , cDisplayList ) , cValidColumns ) , cColumnAlign  ) )
                                ).
  END.
  
  CREATE QUERY hQuery.
  hBuffer = BUFFER tt_lint:HANDLE.
  hQuery:SET-BUFFERS( hBuffer ).
  /* Note that the filter is applied to the tree, in other words no need to add the filter condition
     to the query where expression below */
  hQuery:QUERY-PREPARE( SUBSTITUTE( "FOR EACH tt_lint NO-LOCK WHERE &1 AND &2" , cFilterWhere , cWhereClause ) ).
  hQuery:QUERY-OPEN().
  hQuery:GET-NEXT().
  
  /* Go through all records */
  DO WHILE( NOT hQuery:QUERY-OFF-END ):
    hBuffer = hQuery:GET-BUFFER-HANDLE(1).
    DO iCount = 1 TO NUM-ENTRIES( cDisplayList ):
      ASSIGN
        hField              = hBuffer:BUFFER-FIELD( ENTRY( iCount , cDisplayList ) )
        cString             = TRIM( STRING( hField:BUFFER-VALUE ) )
        iColSizes[ iCount ] = MAXIMUM( iColSizes[ iCount ] ,  FONT-TABLE:GET-TEXT-WIDTH-PIXELS( cString ) ).  /* Store maximum column width for resizing */
        
      IF iCount = 1 THEN DO:
        chItem = chList:ListItems:ADD( , STRING( hBuffer:ROWID ) , TRIM( STRING( hField:BUFFER-VALUE ) ) ).
        
        /* Nice to color the list view if tree is hidden */
        IF NOT glShowTree THEN
          IF tt_lint.ttSeverity >= {&NODE_HIGH_START_LIMIT} THEN
            chItem:ForeColor = {&NODE_HIGH_COLOR}.
          ELSE IF tt_lint.ttSeverity >= {&NODE_MED_START_LIMIT} THEN
            chItem:ForeColor = {&NODE_MEDIUM_COLOR}.
          ELSE
            chItem:ForeColor = {&NODE_LOW_COLOR}.
      END.
      ELSE
        chItem:SubItems( iCount - 1 ) = TRIM( STRING( hField:BUFFER-VALUE ) ).
    END.
    hQuery:GET-NEXT().
  END.
  
  /* Set the Column Sizes */
  DO iCount = 1 TO NUM-ENTRIES( cDisplayList ):
    chList:ColumnHeaders:ITEM( iCount ):WIDTH = 10 + MAXIMUM( iColSizes[ iCount ] , FONT-TABLE:GET-TEXT-WIDTH-PIXELS( chList:ColumnHeaders:ITEM( iCount ):TEXT ) ).
  END.
END.

/* Be default always select the first item on the ListView */
IF chList:ListItems:COUNT() > 0 THEN DO:
  chList:ListItems:ITEM( 1 ):SELECTED = TRUE.
  /* Select the item in case code-preview window is open */
  RUN itemClick( chList:ListItems:ITEM( 1 ) ).
END.

IF VALID-HANDLE( hQuery ) THEN DELETE OBJECT hQuery.
IF VALID-HANDLE( hBuffer ) THEN DELETE OBJECT hBuffer.
RELEASE OBJECT chItem NO-ERROR.
chItem = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE toggleButton C-Win 
PROCEDURE toggleButton :
/*------------------------------------------------------------------------------
  Purpose:     Sets the button state
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER phbutton AS HANDLE  NO-UNDO.
  DEFINE INPUT PARAMETER plState  AS LOGICAL NO-UNDO.
 
  DEFINE VARIABLE iReturnValue AS INTEGER NO-UNDO.
  
  &SCOPED BM_SETSTATE 243
 
  IF phButton:TYPE <> 'BUTTON':U THEN RETURN.
  RUN SendMessageA( phbutton:HWND, 
                   {&BM_SETSTATE},
                   IF plState THEN 1 ELSE 0 ,   /* FALSE, remove BST_PUSHED style */
                   0, 
                   OUTPUT iReturnValue).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION filterDefined C-Win 
FUNCTION filterDefined RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Returns if a filter is set
    Notes:  
------------------------------------------------------------------------------*/

  RETURN ( fltDesc        <> "*":U OR
           fltMaxSeverity <> 9     OR
           fltMinSeverity <> 0     OR
           fltCompunit    <> "*":U OR
           fltSource      <> "*":U OR
           fltRule        <> "*":U  ).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentSelectionRowID C-Win 
FUNCTION getCurrentSelectionRowID RETURNS ROWID
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Returns the ROWID of the currently select item on the list view.
            Returns ? if nothing is selected
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE chItem AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE rRowID AS ROWID      NO-UNDO.
  
  chItem = chList:SelectedItem.
  
  IF NOT VALID-HANDLE( chItem ) THEN
    rRowID = ?.
  ELSE
    rRowId = TO-ROWID( chItem:KEY ).
    
  RETURN rRowID.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFilterWhereExpression C-Win 
FUNCTION getFilterWhereExpression RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:     Creates a where expresion based on filter settings.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cWhereExp AS CHARACTER NO-UNDO.

/* Note when adding this expressions.  As the resulting where clause will be used with buffers of tt_list (like tt_list# for example)
   we cannot add the table qualifier to the expressions, Progress will scope the fields to the buffer 
*/

cWhereExp = "":U.
IF fltDesc        <> "*":U THEN cWhereExp = cWhereExp + MINIMUM( cWhereExp , " AND ":U ) + "ttDescription MATCHES '":U + fltDesc + "'":U.
IF fltMinSeverity <> 0     THEN cWhereExp = cWhereExp + MINIMUM( cWhereExp , " AND ":U ) + "ttSeverity >= ":U + STRING( fltMinSeverity ).
IF fltMaxSeverity <> 9     THEN cWhereExp = cWhereExp + MINIMUM( cWhereExp , " AND ":U ) + "ttSeverity <= ":U + STRING( fltMaxSeverity ).
IF fltCompunit    <> "*":U THEN cWhereExp = cWhereExp + MINIMUM( cWhereExp , " AND ":U ) + "ttCompunit MATCHES '":U + fltCompunit + "'":U.
IF fltSource      <> "*":U THEN cWhereExp = cWhereExp + MINIMUM( cWhereExp , " AND ":U ) + "ttSource MATCHES '":U + fltSource + "'":U.
IF fltRule        <> "*":U THEN cWhereExp = cWhereExp + MINIMUM( cWhereExp , " AND ":U ) + "ttRuleID MATCHES '":U + fltRule + "'":U.

RETURN ( IF cWhereExp = ? OR cWhereExp = "" THEN "TRUE":U ELSE cWhereExp) .
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

