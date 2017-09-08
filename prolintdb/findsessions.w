&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          prolintdb        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/* =======================================================================================
    file    : prolint/prolintdb/findsessions.w
    purpose :
    by      : Glen West
    -----------------------------------------------------------------

    Copyright (C) 2007 Glen West

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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER MyContext    AS CHARACTER NO-UNDO INITIAL "OK".  /* OK if called from selectfiles, Load if called from logwin */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE propsrunning AS LOGICAL   NO-UNDO INITIAL FALSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME Br_Sessions

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES lint_session

/* Definitions for BROWSE Br_Sessions                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Sessions lint_session.sessionid ~
lint_session.sessionStartDate string(lint_session.sessionStartTime,"HH:MM:SS") ~
lint_session.sessionuser lint_session.sessionname ~
lint_session.sessionLastDate ~
string(lint_session.sessionLastTime,"HH:MM:SS") lint_session.sessionbox ~
lint_session.sessionparms lint_session.sessionrules 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Sessions lint_session.sessionname 
&Scoped-define ENABLED-TABLES-IN-QUERY-Br_Sessions lint_session
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Br_Sessions lint_session
&Scoped-define QUERY-STRING-Br_Sessions FOR EACH lint_session NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Sessions OPEN QUERY Br_Sessions FOR EACH lint_session NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Sessions lint_session
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Sessions lint_session


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-Br_Sessions}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_user fi_startdate Btn_find fi_name ~
Br_Sessions Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS fi_user fi_startdate fi_name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_find 
     LABEL "Find" 
     SIZE 15 BY 1.

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name (ma)" 
     VIEW-AS FILL-IN 
     SIZE 47.6 BY 1 NO-UNDO.

DEFINE VARIABLE fi_startdate AS DATE FORMAT "99/99/99":U 
     LABEL "StartDate" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_user AS CHARACTER FORMAT "X(30)":U 
     LABEL "User (bg)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Sessions FOR  
      lint_session SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Sessions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Sessions Dialog-Frame _STRUCTURED
  QUERY Br_Sessions NO-LOCK DISPLAY
      lint_session.sessionid FORMAT ">>>>>>>>>9":U
      lint_session.sessionStartDate FORMAT "99/99/99":U
      string(lint_session.sessionStartTime,"HH:MM:SS") COLUMN-LABEL "Time"
            WIDTH 9
      lint_session.sessionuser FORMAT "x(16)":U
      lint_session.sessionname FORMAT "x(30)":U
      lint_session.sessionLastDate FORMAT "99/99/99":U
      string(lint_session.sessionLastTime,"HH:MM:SS") COLUMN-LABEL "Time"
            WIDTH 9
      lint_session.sessionbox FORMAT "x(30)":U
      lint_session.sessionparms FORMAT "x(40)":U
      lint_session.sessionrules FORMAT "x(60)":U
  ENABLE
      lint_session.sessionname
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 75 BY 8.81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fi_user AT ROW 1 COL 11 COLON-ALIGNED
     fi_startdate AT ROW 1 COL 44 COLON-ALIGNED
     Btn_find AT ROW 1.48 COL 61
     fi_name AT ROW 1.95 COL 11 COLON-ALIGNED
     Br_Sessions AT ROW 2.91 COL 1
     Btn_OK AT ROW 13 COL 1.4
     Btn_Cancel AT ROW 13 COL 17.4
     Btn_Help AT ROW 13.05 COL 61
     "NOTE: Why is your *new* Session not here?" VIEW-AS TEXT
          SIZE 74 BY .62 AT ROW 11.76 COL 2
     "It is not created until something is LINTed." VIEW-AS TEXT
          SIZE 46.4 BY .62 AT ROW 12.38 COL 9.6
     SPACE(20.00) SKIP(1.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Find Session"
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
   FRAME-NAME                                                           */
/* BROWSE-TAB Br_Sessions fi_name Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Sessions
/* Query rebuild information for BROWSE Br_Sessions
     _TblList          = "prolintdb.lint_session"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = prolintdb.lint_session.sessionid
     _FldNameList[2]   = prolintdb.lint_session.sessionStartDate
     _FldNameList[3]   > "_<CALC>"
"string(lint_session.sessionStartTime,""HH:MM:SS"")" "Time" ? ? ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = prolintdb.lint_session.sessionuser
     _FldNameList[5]   > prolintdb.lint_session.sessionname
"lint_session.sessionname" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = prolintdb.lint_session.sessionLastDate
     _FldNameList[7]   > "_<CALC>"
"string(lint_session.sessionLastTime,""HH:MM:SS"")" "Time" ? ? ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = prolintdb.lint_session.sessionbox
     _FldNameList[9]   = prolintdb.lint_session.sessionparms
     _FldNameList[10]   = prolintdb.lint_session.sessionrules
     _Query            is OPENED
*/  /* BROWSE Br_Sessions */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Find Session */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_find
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_find Dialog-Frame
ON CHOOSE OF Btn_find IN FRAME Dialog-Frame /* Find */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN fi_user
           fi_startdate
           fi_name.
    RUN OpenQ IN THIS-PROCEDURE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
   RUN prolint/core/openhtml.p( "http://oehive.org/node/944":U ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Sessions
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* since appbuilder does not support 'default-action' the trigger must be defined
   here.  */
ON DEFAULT-ACTION OF br_Sessions IN FRAME {&FRAME-NAME} DO:
   APPLY "CHOOSE" TO btn_ok IN FRAME {&FRAME-NAME}.
END.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  PUBLISH "IsProlintPropertiesRunning":U (OUTPUT propsrunning).
  IF NOT propsrunning THEN
     RUN prolint/core/propsuper.p PERSISTENT.
  RUN IncrementProlintPropertySubscribers.
  
  ASSIGN fi_user      = userid("prolintdb")
         fi_startdate = today.
  RUN enable_UI.
  ASSIGN btn_ok:label = MyContext.
  RUN openq IN THIS-PROCEDURE.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RUN SetSession IN THIS-PROCEDURE.

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
  DISPLAY fi_user fi_startdate fi_name 
      WITH FRAME Dialog-Frame.
  ENABLE fi_user fi_startdate Btn_find fi_name Br_Sessions Btn_OK Btn_Cancel 
         Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQ Dialog-Frame 
PROCEDURE OpenQ :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  CLOSE QUERY br_sessions.
  OPEN QUERY br_sessions FOR EACH lint_session
       WHERE  lint_session.sessionname      MATCHES "*" + fi_name + "*"
         and (lint_session.sessionStartDate >=      fi_startdate 
              or fi_startdate = ?)
         and  lint_session.sessionuser      BEGINS  fi_user
       NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PublishResults Dialog-Frame 
PROCEDURE PublishResults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE vcCompUnit   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcSourceFile AS CHARACTER  NO-UNDO.
DEFINE VARIABLE viLineNo     AS INTEGER    NO-UNDO.
DEFINE VARIABLE vcComment    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcRuleID     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE viSeverity   AS INTEGER    NO-UNDO.

IF NOT AVAILABLE lint_session THEN DO:
   MESSAGE 'No results to publish' VIEW-AS ALERT-BOX.
   RETURN ERROR "No Session".
END.

PUBLISH "Prolint_InitializeResults" (TRUE). /* INITIALIZE RESULT LIST */
  
FOR EACH lint_warning NO-LOCK
   WHERE lint_warning.sessionid = lint_session.sessionid:
  ASSIGN vcCompUnit   = lint_warning.compunit
         vcSourceFile = lint_warning.sourcefile
         viLineNo     = lint_warning.linenumber
         vcComment    = lint_warning.comment
         vcRuleID     = lint_warning.ruleid
         viSeverity   = lint_warning.severity.

  PUBLISH "Prolint_AddResult" (vcCompUnit,vcSourceFile,viLineNo,VcComment,vcRuleID,viSeverity).
END. /* FOR EACH lint_warning */

PUBLISH "Prolint_FinalizeResults".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSession Dialog-Frame 
PROCEDURE SetSession :
/*------------------------------------------------------------------------------
  Purpose:     Place selected session in properties super procedure
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CASE MyContext:
  WHEN "OK" THEN DO:
    IF AVAILABLE lint_session THEN DO:
        RUN SetProlintProperty ("SessionID", 
                                STRING(lint_session.sessionid,">>>>>>>>>9")).
        RUN SetProlintProperty ("SessionName", lint_session.sessionname).
    END.
  END. /* WHEN OK */
  WHEN "Load" THEN DO:
    RUN PublishResults.
  END. /* WHEN LOAD */
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

