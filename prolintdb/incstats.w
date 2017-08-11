&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          prolintdb        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/* =======================================================================================
    file    : prolint/prolintdb/incstats.w
    purpose : query data in prolintdb by includefile
    note    : create the database with prolint/prolintdb/prolintdb.df
    by      : Jurjen Dijkstra
    -----------------------------------------------------------------

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
   ======================================================================================= */

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{prolint/core/dlc-version.i}         
DEFINE VARIABLE UtilsRunning AS LOGICAL NO-UNDO INITIAL FALSE.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-inc

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES lint_stat_inc

/* Definitions for BROWSE BROWSE-inc                                    */
&Scoped-define FIELDS-IN-QUERY-BROWSE-inc lint_stat_inc.sourcefile ~
lint_stat_inc.ruleid lint_stat_inc.numwarnings lint_stat_inc.score ~
lint_stat_inc.distinctwarnings lint_stat_inc.scoredistinct 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-inc ~
lint_stat_inc.scoredistinct 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-inc lint_stat_inc
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-inc lint_stat_inc
&Scoped-define QUERY-STRING-BROWSE-inc FOR EACH lint_stat_inc NO-LOCK ~
    BY lint_stat_inc.sourcefile INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-inc OPEN QUERY BROWSE-inc FOR EACH lint_stat_inc NO-LOCK ~
    BY lint_stat_inc.sourcefile INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-inc lint_stat_inc
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-inc lint_stat_inc


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-inc}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-inc BtnDistinct BtnCalc BtnDelete 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCalc 
     LABEL "Recalculate" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BtnDelete 
     LABEL "Delete warnings from db" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BtnDistinct 
     LABEL "Show Distinct warnings" 
     SIZE 26 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-inc FOR 
      lint_stat_inc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-inc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-inc C-Win _STRUCTURED
  QUERY BROWSE-inc NO-LOCK DISPLAY
      lint_stat_inc.sourcefile FORMAT "X(32)":U
      lint_stat_inc.ruleid FORMAT "X(15)":U
      lint_stat_inc.numwarnings FORMAT "->,>>>,>>9":U
      lint_stat_inc.score FORMAT "->>,>>>,>>9":U
      lint_stat_inc.distinctwarnings FORMAT "->,>>>,>>9":U
      lint_stat_inc.scoredistinct FORMAT "->>>,>>9":U
  ENABLE
      lint_stat_inc.scoredistinct
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 111 BY 12.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-inc AT ROW 1.71 COL 3
     BtnDistinct AT ROW 15.05 COL 4
     BtnCalc AT ROW 15.05 COL 31
     BtnDelete AT ROW 15.05 COL 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115 BY 16.


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
         TITLE              = "Statistics by includefile"
         HEIGHT             = 16
         WIDTH              = 115
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 125
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
/* BROWSE-TAB BROWSE-inc 1 DEFAULT-FRAME */
ASSIGN 
       lint_stat_inc.scoredistinct:COLUMN-READ-ONLY IN BROWSE BROWSE-inc = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-inc
/* Query rebuild information for BROWSE BROWSE-inc
     _TblList          = "prolintdb.lint_stat_inc"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "prolintdb.lint_stat_inc.sourcefile|yes"
     _FldNameList[1]   = prolintdb.lint_stat_inc.sourcefile
     _FldNameList[2]   = prolintdb.lint_stat_inc.ruleid
     _FldNameList[3]   = prolintdb.lint_stat_inc.numwarnings
     _FldNameList[4]   = prolintdb.lint_stat_inc.score
     _FldNameList[5]   = prolintdb.lint_stat_inc.distinctwarnings
     _FldNameList[6]   > prolintdb.lint_stat_inc.scoredistinct
"lint_stat_inc.scoredistinct" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-inc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Statistics by includefile */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Statistics by includefile */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-inc
&Scoped-define SELF-NAME BROWSE-inc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-inc C-Win
ON START-SEARCH OF BROWSE-inc IN FRAME DEFAULT-FRAME
DO:
  
  DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
  DEFINE VARIABLE order   AS INTEGER NO-UNDO.
  hColumn = browse-inc:CURRENT-COLUMN in frame {&frame-name}.
  
  IF VALID-HANDLE(hColumn) THEN
     CASE hColumn:NAME :
         WHEN "ruleid":U        THEN  order = 2.
         WHEN "numwarnings":U   THEN  order = 3.
         WHEN "score":U         THEN  order = 4.
         WHEN "distinctwarnings":U      THEN  order = 5.
         WHEN "scoredistinct":U THEN  order = 6.
         OTHERWISE                    order = 1.
     END CASE.
  ELSE 
     order = 1.

  CASE order :
     WHEN 1 THEN  OPEN QUERY browse-inc FOR EACH prolintdb.lint_stat_inc NO-LOCK BY prolintdb.lint_stat_inc.sourcefile BY prolintdb.lint_stat_inc.ruleid.
     WHEN 2 THEN  OPEN QUERY browse-inc FOR EACH prolintdb.lint_stat_inc NO-LOCK BY prolintdb.lint_stat_inc.ruleid BY prolintdb.lint_stat_inc.scoredistinct DESCENDING.
     WHEN 3 THEN  OPEN QUERY browse-inc FOR EACH prolintdb.lint_stat_inc NO-LOCK BY prolintdb.lint_stat_inc.numwarnings DESCENDING.
     WHEN 4 THEN  OPEN QUERY browse-inc FOR EACH prolintdb.lint_stat_inc NO-LOCK BY prolintdb.lint_stat_inc.score DESCENDING.
     WHEN 5 THEN  OPEN QUERY browse-inc FOR EACH prolintdb.lint_stat_inc NO-LOCK BY prolintdb.lint_stat_inc.distinctwarnings.
     WHEN 6 THEN  OPEN QUERY browse-inc FOR EACH prolintdb.lint_stat_inc NO-LOCK BY prolintdb.lint_stat_inc.scoredistinct DESCENDING.
  END CASE.

  {&OPEN-QUERY-BROWSE-2}


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCalc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCalc C-Win
ON CHOOSE OF BtnCalc IN FRAME DEFAULT-FRAME /* Recalculate */
DO:
  RUN RecalcIncludeStats.
  {&OPEN-QUERY-BROWSE-inc}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDelete C-Win
ON CHOOSE OF BtnDelete IN FRAME DEFAULT-FRAME /* Delete warnings from db */
DO:
  DEFINE VARIABLE answer AS LOGICAL NO-UNDO INITIAL FALSE.

  IF AVAILABLE prolintdb.lint_stat_inc THEN DO:
     MESSAGE SUBSTITUTE("Delete all &1 warnings for rule &2 by this includefile from the database?":T,
                        string(prolintdb.lint_stat_inc.numwarnings),
                        prolintdb.lint_stat_inc.ruleid )
         VIEW-AS ALERT-BOX QUESTION
         BUTTONS YES-NO
         UPDATE answer.
     PROCESS EVENTS.
     IF answer=YES THEN DO:
        PUBLISH "Prolint_Stat_PruneInc" (prolintdb.lint_stat_inc.ruleid, prolintdb.lint_stat_inc.sourcefile).
        MESSAGE "Done. You should recalculate the statistics"
             VIEW-AS ALERT-BOX.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDistinct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDistinct C-Win
ON CHOOSE OF BtnDistinct IN FRAME DEFAULT-FRAME /* Show Distinct warnings */
DO:
  RUN ShowDistinct.
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

  lint_stat_inc.scoredistinct:read-only in browse browse-inc = true.

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
  ENABLE BROWSE-inc BtnDistinct BtnCalc BtnDelete 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RecalcIncludeStats C-Win 
PROCEDURE RecalcIncludeStats :
/*------------------------------------------------------------------------------
  Purpose:     rebuild THESE stats. Not the other statisctics.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE l-rebuilt AS LOGICAL    NO-UNDO.
  PUBLISH "Prolint_Stat_Rebuild" (OUTPUT l-rebuilt).
  IF NOT l-rebuilt = TRUE THEN 
     MESSAGE "Services procedure unable to rebuild statistics." SKIP
             "Please restart this procedure and try again."
             VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowDistinct C-Win 
PROCEDURE ShowDistinct :
/*------------------------------------------------------------------------------
  Purpose:     Show warnings in logwin.w
               but avoid identical warnings (from different compunits)
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
  PUBLISH "Prolint_Stat_Distinct" (prolintdb.lint_stat_inc.ruleid, prolintdb.lint_stat_inc.sourcefile).

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

