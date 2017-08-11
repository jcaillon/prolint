&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          prolintdb        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/* =======================================================================================
    file    : prolint/prolintdb/rulestats.w
    purpose : query data in prolintdb by rule
    note    : create the database with prolint/prolintdb/prolintdb.df
    by      : Jurjen Dijkstra
    -----------------------------------------------------------------

    Copyright (C) 2001,2002 Jurjen Dijkstra

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
DEFINE VARIABLE UtilsRunning AS LOGICAL NO-UNDO INITIAL FALSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES lint_stat_rule lint_stat_ruledir

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 lint_stat_rule.ruleid lint_stat_rule.numwarnings lint_stat_rule.score lint_stat_rule.severity   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 lint_stat_rule.severity   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 lint_stat_rule
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 lint_stat_rule
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 /* OPEN QUERY {&SELF-NAME} FOR EACH lint_stat_rule NO-LOCK INDEXED-REPOSITION. */    RUN OpenBrowse1.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 lint_stat_rule
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 lint_stat_rule


/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 lint_stat_ruledir.subdir lint_stat_ruledir.numwarnings lint_stat_ruledir.score lint_stat_ruledir.severity   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 lint_stat_ruledir.severity   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 lint_stat_ruledir
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 lint_stat_ruledir
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define OPEN-QUERY-BROWSE-2 /* OPEN QUERY {&SELF-NAME} FOR EACH lint_stat_ruledir NO-LOCK                             WHERE lint_stat_ruledir.ruleid = lint_stat_rule.ruleid                             INDEXED-REPOSITION. */ RUN OpenBrowse2.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 lint_stat_ruledir
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 lint_stat_ruledir


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 BROWSE-2 btnResults BtnBuild ~
BtnFreeform 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnBuild 
     LABEL "Recalculate" 
     SIZE 17 BY 1.14 TOOLTIP "Recalculate all statistics (may take a while)".

DEFINE BUTTON BtnFreeform 
     LABEL "Freeform" 
     SIZE 17 BY 1.14.

DEFINE BUTTON btnResults 
     LABEL "Show Results" 
     SIZE 17 BY 1.14 TOOLTIP "Show warnings from selected rule + subdir".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      lint_stat_rule SCROLLING.

DEFINE QUERY BROWSE-2 FOR 
      lint_stat_ruledir SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      lint_stat_rule.ruleid FORMAT "X(15)":U WIDTH 22.2
      lint_stat_rule.numwarnings FORMAT "->,>>>,>>9":U WIDTH 22.2
      lint_stat_rule.score FORMAT "->>,>>>,>>9":U WIDTH 27.2
      lint_stat_rule.severity FORMAT ">9":U WIDTH 19.2
  ENABLE
      lint_stat_rule.severity
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 9.05.

DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      lint_stat_ruledir.subdir FORMAT "X(32)":U
      lint_stat_ruledir.numwarnings FORMAT "->,>>>,>>9":U WIDTH 20.4
      lint_stat_ruledir.score FORMAT "->,>>>,>>9":U WIDTH 20.2
      lint_stat_ruledir.severity FORMAT ">9":U WIDTH 19.6
  ENABLE
      lint_stat_ruledir.severity
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 7.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-1 AT ROW 1.95 COL 2
     BROWSE-2 AT ROW 11.48 COL 2
     btnResults AT ROW 19.33 COL 3
     BtnBuild AT ROW 19.33 COL 21
     BtnFreeform AT ROW 19.33 COL 39
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.2 BY 19.91.


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
         TITLE              = "Statistics by rule"
         HEIGHT             = 19.91
         WIDTH              = 103.2
         MAX-HEIGHT         = 20.19
         MAX-WIDTH          = 143.2
         VIRTUAL-HEIGHT     = 20.19
         VIRTUAL-WIDTH      = 143.2
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 1 DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-2 BROWSE-1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
/* OPEN QUERY {&SELF-NAME} FOR EACH lint_stat_rule NO-LOCK INDEXED-REPOSITION. */

  RUN OpenBrowse1.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
/* OPEN QUERY {&SELF-NAME} FOR EACH lint_stat_ruledir NO-LOCK
                            WHERE lint_stat_ruledir.ruleid = lint_stat_rule.ruleid
                            INDEXED-REPOSITION. */
RUN OpenBrowse2.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Statistics by rule */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Statistics by rule */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON START-SEARCH OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  RUN OpenBrowse1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON VALUE-CHANGED OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-BROWSE-2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON START-SEARCH OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
    RUN OpenBrowse2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnBuild
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnBuild C-Win
ON CHOOSE OF BtnBuild IN FRAME DEFAULT-FRAME /* Recalculate */
DO:
  DEFINE VARIABLE l-rebuilt AS LOGICAL    NO-UNDO.
  PUBLISH "Prolint_Stat_Rebuild" (OUTPUT l-rebuilt).
  IF NOT l-rebuilt = TRUE THEN 
     MESSAGE "Services procedure unable to rebuild statistics." SKIP
             "Please restart this procedure and try again."
             VIEW-AS ALERT-BOX.

  {&OPEN-QUERY-BROWSE-1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnFreeform
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnFreeform C-Win
ON CHOOSE OF BtnFreeform IN FRAME DEFAULT-FRAME /* Freeform */
DO:
  DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hTable AS HANDLE     NO-UNDO.

  CREATE BUFFER hTable FOR TABLE "lint_session" NO-ERROR.
  IF AVAILABLE prolintdb.lint_stat_ruledir THEN 
     ASSIGN cWhere = (IF VALID-HANDLE(hTable) 
                      THEN "      lint_warning.sessionid = 0 AND ~n":U
                      ELSE "":U)
                   +  SUBSTITUTE('      lint_warning.subdir = "&1" ~n  and lint_warning.ruleid = "&2" ':U,
                               prolintdb.lint_stat_ruledir.subdir,
                               prolintdb.lint_stat_ruledir.ruleid).
  ELSE ASSIGN cWhere = (IF VALID-HANDLE(hTable) 
                        THEN ' lint_warning.sessionid = 0 ':U 
                        ELSE " TRUE ":U).


  RUN prolint/prolintdb/freeform.w (cWhere).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResults C-Win
ON CHOOSE OF btnResults IN FRAME DEFAULT-FRAME /* Show Results */
DO:
  RUN ShowWarnings.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   UNSUBSCRIBE TO "Prolint_Stats_Subscribers".
   PUBLISH "Prolint_Stat_Shutdown".
   RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* setup the utilities super procedure */
PUBLISH "Prolint_Stat_Heartbeat" (OUTPUT UtilsRunning ).
IF NOT UtilsRunning THEN RUN prolint/prolintdb/statsutils.p PERSISTENT.
SUBSCRIBE TO "Prolint_Stats_Subscribers" ANYWHERE
  RUN-PROCEDURE "Stay-Subscribed".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  lint_stat_rule.severity:read-only in browse browse-1 = true.
  lint_stat_ruledir.severity:read-only in browse browse-2 = true.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  ENABLE BROWSE-1 BROWSE-2 btnResults BtnBuild BtnFreeform 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenBrowse1 C-Win 
PROCEDURE OpenBrowse1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
  DEFINE VARIABLE order   AS INTEGER NO-UNDO.
  hColumn = browse-1:CURRENT-COLUMN in frame {&frame-name}.
  
  IF VALID-HANDLE(hColumn) THEN
     CASE hColumn:NAME :
         WHEN "Severity":U    THEN  order = 2.
         WHEN "numwarnings":U THEN  order = 3.
         WHEN "score":U       THEN  order = 4.
         OTHERWISE                  order = 1.
     END CASE.
  ELSE 
     order = 1.

  CASE order :
     WHEN 1 THEN  OPEN QUERY browse-1 FOR EACH prolintdb.lint_stat_rule NO-LOCK BY prolintdb.lint_stat_rule.ruleid.
     WHEN 2 THEN  OPEN QUERY browse-1 FOR EACH prolintdb.lint_stat_rule NO-LOCK BY prolintdb.lint_stat_rule.severity DESCENDING.
     WHEN 3 THEN  OPEN QUERY browse-1 FOR EACH prolintdb.lint_stat_rule NO-LOCK BY prolintdb.lint_stat_rule.numwarnings DESCENDING.
     WHEN 4 THEN  OPEN QUERY browse-1 FOR EACH prolintdb.lint_stat_rule NO-LOCK BY prolintdb.lint_stat_rule.score DESCENDING.
  END CASE.

  {&OPEN-QUERY-BROWSE-2}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenBrowse2 C-Win 
PROCEDURE OpenBrowse2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE order   AS INTEGER NO-UNDO.
    hColumn = browse-2:CURRENT-COLUMN in frame {&frame-name}.

    IF VALID-HANDLE(hColumn) THEN
       CASE hColumn:NAME :
           WHEN "subdir":U      THEN  order = 1.
           WHEN "Severity":U    THEN  order = 2.
           WHEN "numwarnings":U THEN  order = 3.
           WHEN "score":U       THEN  order = 4.
           OTHERWISE                  order = 1.
       END CASE.
    ELSE 
        order = 1.

    IF AVAILABLE prolintdb.lint_stat_rule THEN
       CASE order :
           WHEN 1 THEN  OPEN QUERY browse-2 FOR EACH prolintdb.lint_stat_ruledir NO-LOCK WHERE prolintdb.lint_stat_ruledir.ruleid = prolintdb.lint_stat_rule.ruleid BY prolintdb.lint_stat_ruledir.subdir.
           WHEN 2 THEN  OPEN QUERY browse-2 FOR EACH prolintdb.lint_stat_ruledir NO-LOCK WHERE prolintdb.lint_stat_ruledir.ruleid = prolintdb.lint_stat_rule.ruleid BY prolintdb.lint_stat_ruledir.severity DESCENDING.
           WHEN 3 THEN  OPEN QUERY browse-2 FOR EACH prolintdb.lint_stat_ruledir NO-LOCK WHERE prolintdb.lint_stat_ruledir.ruleid = prolintdb.lint_stat_rule.ruleid BY prolintdb.lint_stat_ruledir.numwarnings DESCENDING.
           WHEN 4 THEN  OPEN QUERY browse-2 FOR EACH prolintdb.lint_stat_ruledir NO-LOCK WHERE prolintdb.lint_stat_ruledir.ruleid = prolintdb.lint_stat_rule.ruleid BY prolintdb.lint_stat_ruledir.score DESCENDING.
       END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowWarnings C-Win 
PROCEDURE ShowWarnings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE LogwinRunning AS LOGICAL NO-UNDO.
  DEFINE VARIABLE hw            AS HANDLE  NO-UNDO.

  /* open logwin if it isn't already opened */

  LogwinRunning = FALSE.
  hw = SESSION:FIRST-CHILD.
  DO WHILE VALID-HANDLE(hw) :
     IF hw:PRIVATE-DATA = "prolint_outputhandler_logwin.w":U THEN
        LogwinRunning = TRUE.
     hw = hw:NEXT-SIBLING.
  END.
  IF NOT LogwinRunning THEN 
     RUN prolint/outputhandlers/logwin.w PERSISTENT.
  
  /* send results to the logwin: simply publish warnings as if you are live linting */
  /* send results to the logwin: simply publish warnings as if you are live linting */
  PUBLISH "Prolint_Stat_Details" (prolintdb.lint_stat_ruledir.ruleid, 
                                  prolintdb.lint_stat_ruledir.subdir).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Stay-Subscribed C-Win 
PROCEDURE Stay-Subscribed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pl-iCare AS LOGICAL NO-UNDO INITIAL TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

