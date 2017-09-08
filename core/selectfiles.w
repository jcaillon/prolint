&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File:            prolint/core/selectfiles.w

  Description:     select parameters for prolint/prolint.p
                   in GUI mode

  Author:          Jurjen Dijkstra

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
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{prolint/core/dlc-version.i}

DEFINE TEMP-TABLE tt_sourcefiles NO-UNDO
   FIELD SourceFile   AS CHARACTER
   INDEX idx_id   AS PRIMARY UNIQUE SourceFile.

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_is_running)=0 &THEN
  DEFINE OUTPUT PARAMETER ModalResult AS INTEGER NO-UNDO INITIAL 0.
  DEFINE OUTPUT PARAMETER pProfile    AS CHARACTER NO-UNDO INITIAL 0.
  DEFINE OUTPUT PARAMETER TABLE FOR tt_sourcefiles.
  DEFINE OUTPUT PARAMETER pClearOutput AS LOGICAL NO-UNDO INITIAL TRUE.
&ELSE
  DEFINE VARIABLE ModalResult  AS INTEGER   NO-UNDO INITIAL 0.
  DEFINE VARIABLE pProfile     AS CHARACTER NO-UNDO INITIAL 0.
  DEFINE VARIABLE pClearOutput AS LOGICAL   NO-UNDO INITIAL TRUE.
&ENDIF

DEFINE VARIABLE filemasks    AS CHARACTER NO-UNDO.
DEFINE VARIABLE sessionaware AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE propsrunning AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE SessionName  AS CHARACTER NO-UNDO LABEL "Session Name" FORMAT "X(30)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

&Scoped-define LAYOUT-VARIABLE CURRENT-WINDOW-layout

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_new btn_find btn_session0 cb_profile ~
tg_ClearOutput BT_BROWSE ed_sourcefile Btn_rtbtask Btn_OK Btn_Cancel ~
Btn_Help RT_SEPARATOR 
&Scoped-Define DISPLAYED-OBJECTS cb_profile tg_ClearOutput ed_sourcefile ~
fi_SessionID 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RelativeFileName Dialog-Frame 
FUNCTION RelativeFileName RETURNS CHARACTER
  ( pFileName AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* Define a variable to store the name of the active layout.            */
DEFINE VAR CURRENT-WINDOW-layout AS CHAR INITIAL "Master Layout":U NO-UNDO.

/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     LABEL "Cancel":T 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
     &ELSE SIZE 14 BY 1 &ENDIF
     BGCOLOR 8 .

DEFINE BUTTON btn_find 
     LABEL "Find" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
     &ELSE SIZE 14.2 BY 1 &ENDIF.

DEFINE BUTTON Btn_Help 
     LABEL "&Help":T 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
     &ELSE SIZE 14 BY 1 &ENDIF
     BGCOLOR 8 .

DEFINE BUTTON btn_new 
     LABEL "New" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
     &ELSE SIZE 14.2 BY 1 &ENDIF.

DEFINE BUTTON Btn_OK 
     LABEL "OK":T 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 14 BY 1
     &ELSE SIZE 14 BY 1 &ENDIF
     BGCOLOR 8 .

DEFINE BUTTON Btn_rtbtask 
     LABEL "RTB task":U 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
     &ELSE SIZE 15 BY 1 &ENDIF.

DEFINE BUTTON btn_session0 
     LABEL "No session" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
     &ELSE SIZE 15 BY 1 &ENDIF.

DEFINE BUTTON BT_BROWSE 
     LABEL "..." 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 6 BY 1
     &ELSE SIZE 6 BY 1 &ENDIF.

DEFINE VARIABLE cb_profile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Use profile":T 
     VIEW-AS COMBO-BOX SORT INNER-LINES 15
     DROP-DOWN-LIST
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 77 BY 1
     &ELSE SIZE 77 BY 1 &ENDIF NO-UNDO.

DEFINE VARIABLE ed_sourcefile AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 88 BY 10
     &ELSE SIZE 88 BY 10.48 &ENDIF NO-UNDO.

DEFINE VARIABLE fi_SessionID AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Session ID" 
      VIEW-AS TEXT 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 15 BY 1
     &ELSE SIZE 15 BY .62 &ENDIF NO-UNDO.

DEFINE RECTANGLE RT_SEPARATOR
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 88 BY 1
     &ELSE SIZE 88 BY .1 &ENDIF.

DEFINE VARIABLE tg_ClearOutput AS LOGICAL INITIAL yes 
     LABEL "Clear output handlers before new analyze":T 
     VIEW-AS TOGGLE-BOX
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 71 BY 1
     &ELSE SIZE 71 BY .81 &ENDIF NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btn_new
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 47
          &ELSE AT ROW 1.48 COL 47 &ENDIF
     btn_find
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 62
          &ELSE AT ROW 1.48 COL 62 &ENDIF
     btn_session0
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 77
          &ELSE AT ROW 1.48 COL 77 &ENDIF
     cb_profile
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 3 COL 13 COLON-ALIGNED
          &ELSE AT ROW 2.91 COL 13 COLON-ALIGNED &ENDIF
     tg_ClearOutput
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 5 COL 15
          &ELSE AT ROW 4.57 COL 15 &ENDIF
     BT_BROWSE
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 86
          &ELSE AT ROW 6.24 COL 86 &ENDIF
     ed_sourcefile
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 8 COL 4
          &ELSE AT ROW 7.91 COL 4 &ENDIF NO-LABEL
     Btn_rtbtask
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 4
          &ELSE AT ROW 18.86 COL 4 &ENDIF
     Btn_OK
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 48
          &ELSE AT ROW 18.86 COL 48 &ENDIF
     Btn_Cancel
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 63
          &ELSE AT ROW 18.86 COL 63 &ENDIF
     Btn_Help
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 19 COL 78
          &ELSE AT ROW 18.86 COL 78 &ENDIF
     fi_SessionID
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 13 COLON-ALIGNED
          &ELSE AT ROW 1.71 COL 13 COLON-ALIGNED &ENDIF
     RT_SEPARATOR
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 4
          &ELSE AT ROW 5.76 COL 4 &ENDIF
     "Specify one or more sourcefiles or directories to lint:":T VIEW-AS TEXT
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 50 BY 1
          &ELSE SIZE 50 BY .62 &ENDIF
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 6 COL 4
          &ELSE AT ROW 6.48 COL 4 &ENDIF
     SPACE(40.79) SKIP(13.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Prolint- select files to lint":T
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       ed_sourcefile:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

/* SETTINGS FOR FILL-IN fi_SessionID IN FRAME Dialog-Frame
   NO-ENABLE                                                            */

/* _MULTI-LAYOUT-RUN-TIME-ADJUSTMENTS */

/* LAYOUT-NAME: "Standard Character"
   LAYOUT-TYPE: CHARACTER
   EXPRESSION:  SESSION:DISPLAY-TYPE = 'TTY':U 
   COMMENT:     This layout is the standard layout specification for
                 a customized Character based terminal.  It is usually
                 selected to modify a window that has a GUI based
                 master layout.
                                                                        */
IF SESSION:DISPLAY-TYPE = 'TTY':U  THEN 
  RUN CURRENT-WINDOW-layouts (INPUT 'Standard Character':U) NO-ERROR.

/* END-OF-LAYOUT-DEFINITIONS */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Prolint- select files to lint */
DO:
   RUN Destructor.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
  ModalResult = 0.
  APPLY "go":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_find
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_find Dialog-Frame
ON CHOOSE OF btn_find IN FRAME Dialog-Frame /* Find */
DO:
  /* No need to check availability of database or existence of sequence, the
     button would not be sensitive if they were not available. */
  /* Go get a new sequence number and publish it to propsuper.p */
  RUN prolint/prolintdb/findsessions.w ("OK":U ) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE 'Find Session ID returned an error of:'
            ERROR-STATUS:GET-MESSAGE(1)
            VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END.

  /* go get the new value */
  ASSIGN fi_SessionID =  INTEGER(DYNAMIC-FUNCTION ("ProlintProperty",
                                                   "SessionID")).
  IF fi_SessionID = 0
  OR fi_SessionID = ? THEN DO:
    MESSAGE 'Session ID lost or destroyed'
            VIEW-AS ALERT-BOX.
    DISABLE btn_OK WITH FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.

  /* show it to the user and allow them to proceed */
  DISPLAY fi_SessionID WITH FRAME {&FRAME-NAME}.
  ENABLE btn_OK WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO:
   RUN prolint/core/openhtml.p( "http://oehive.org/node/242":U ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_new
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_new Dialog-Frame
ON CHOOSE OF btn_new IN FRAME Dialog-Frame /* New */
DO:
  /* No need to check availability of database or existence of sequence, the
     button would not be sensitive if they were not available. */

  /* Go get a new sequence number and publish it to propsuper.p */
  RUN prolint/prolintdb/newsessionid.p NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE 'Get New Session ID returned an error of:'
            ERROR-STATUS:GET-MESSAGE(1)
            VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END.

  /* go get the new value */
  ASSIGN fi_SessionID =  INTEGER(DYNAMIC-FUNCTION ("ProlintProperty",
                                                   "SessionID")).
  IF fi_SessionID <= INTEGER(fi_SessionID:SCREEN-VALUE)
  OR fi_SessionID = ? THEN DO:
    MESSAGE 'Session ID did not increment!'
            VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END.

  UPDATE SessionName WITH FRAME TempFrame VIEW-AS DIALOG-BOX THREE-D SIDE-LABELS.
  RUN SetProlintProperty ("SessionName", IF SessionName <> ? THEN SessionName
                                                             ELSE "").


  /* show it to the user and allow them to proceed */
  DISPLAY fi_SessionID WITH FRAME {&FRAME-NAME}.
  ENABLE btn_OK WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    DEFINE VARIABLE i       AS INTEGER      NO-UNDO.
    DEFINE VARIABLE chLine  AS CHARACTER    NO-UNDO.
    ModalResult = 1.
    
    /* copy contents of ed_sourcefiles to temp-table tt_sourcefiles */
    EMPTY TEMP-TABLE tt_sourcefiles.
    ASSIGN ed_sourcefile.
    DO i = 1 TO NUM-ENTRIES(ed_sourcefile,"~n":U) :
        chLine = TRIM(ENTRY(i,ed_sourcefile,"~n":U)).
        IF chLine <> "" THEN
            IF NOT CAN-FIND(tt_sourcefiles WHERE tt_sourcefiles.sourcefile=chLine) THEN DO:
                CREATE tt_sourcefiles.
                ASSIGN tt_sourcefiles.sourcefile = chLine.
            END.
    END.

    IF CAN-FIND(FIRST tt_sourcefiles NO-LOCK) THEN DO:
        /* if session aware and the destination session is 0, be sure the user is aware */
        IF SessionAware AND fi_sessionID = 0 THEN DO:
            MESSAGE "Results will be recorded on the base '0' session, is this OK?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lZero AS LOGICAL.
            IF NOT lZero THEN RETURN NO-APPLY.
        END.

        /* save pProfile in registry as most recently used profile */
        IF OPSYS = "WIN32":U THEN DO:
        LOAD "SOFTWARE":U BASE-KEY "HKEY_CURRENT_USER":U.
            USE "SOFTWARE":U.
            PUT-KEY-VALUE SECTION "Prolint\Selectfiles":U
            KEY "mruprofile":U
            VALUE pProfile NO-ERROR.
        UNLOAD "SOFTWARE":U.
        END.

        APPLY "go":U TO FRAME {&FRAME-NAME}.
    END.
    ELSE
        MESSAGE "Sorry, you need to provide file names to continue." VIEW-AS ALERT-BOX WARNING.

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_rtbtask
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_rtbtask Dialog-Frame
ON CHOOSE OF Btn_rtbtask IN FRAME Dialog-Frame /* RTB task */
DO:
  /* Add all sourcefiles from the currently selected Roundtable task */
  DEFINE VARIABLE RTBCurrentTaskNum       AS INTEGER     NO-UNDO.
  IF CONNECTED("rtb":U) THEN DO:
     RTBCurrentTaskNum = 0.
     /* determine the roundtable version. But how?
        Assume this PUBSUB thing works in 101b and not in 91c */
     PUBLISH "evRtbGetCurrentTask":U (OUTPUT RTBCurrentTaskNum).
     IF RTBCurrentTaskNum = 0 THEN
        RUN prolint/roundtable/91c/taskfiles.p (INPUT THIS-PROCEDURE:HANDLE).
     ELSE
        RUN prolint/roundtable/101b/taskfiles.p (INPUT THIS-PROCEDURE:HANDLE).
     cb_profile:SCREEN-VALUE = "roundtable run":U.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_session0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_session0 Dialog-Frame
ON CHOOSE OF btn_session0 IN FRAME Dialog-Frame /* No session */
DO:
  /* No need to check availability of database or existence of sequence, the
     button would not be sensitive if they were not available. */

  RUN SetProlintProperty ("SessionName", "").
  RUN SetProlintProperty ("SessionId", "").
  ASSIGN fi_SessionID =  0.

  /* show it to the user and allow them to proceed */
  DISPLAY fi_SessionID WITH FRAME {&FRAME-NAME}.
  ENABLE btn_OK WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT_BROWSE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT_BROWSE Dialog-Frame
ON CHOOSE OF BT_BROWSE IN FRAME Dialog-Frame /* ... */
DO:
    DEFINE VARIABLE lOK         AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE cFileName   AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cTmp        AS CHARACTER    NO-UNDO.


    /* Let the user select files */
    SYSTEM-DIALOG GET-FILE cFileName
        TITLE      "Choose sources to analyze..."
        FILTERS    "Source Files (*.p,*.w,*.t)"   "*.p,*.w,*.t",
                   "All Files (*.r)"   "*.*"
        MUST-EXIST
        RETURN-TO-START-DIR
        USE-FILENAME
        UPDATE lOK.

    /* Append the file if needed */
    IF lOK THEN DO:
        /* Ensure the file is not already in the list */
        ASSIGN  cTmp = ed_sourcefile:SCREEN-VALUE
                cTmp = REPLACE(cTmp, "~r~n", "|")
                cTmp = REPLACE(cTmp, "~r", "|")
                cTmp = REPLACE(cTmp, "~n", "|")
                cTmp = REPLACE(cTmp, "||", "|")
                cTmp = TRIM(cTmp, "|").
        IF LOOKUP(cFileName, cTmp, "|") > 0 THEN
            MESSAGE "File is already in the list." VIEW-AS ALERT-BOX WARNING.
        ELSE
            ASSIGN ed_sourcefile:SCREEN-VALUE = LC(REPLACE(TRIM(cTmp + "|" + cFileName, "|"), "|", "~r~n")).
    END.

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb_profile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_profile Dialog-Frame
ON VALUE-CHANGED OF cb_profile IN FRAME Dialog-Frame /* Use profile */
DO:
   ASSIGN cb_profile.
   pProfile = cb_profile.
   RUN GetOutputHandlers IN THIS-PROCEDURE (cb_profile).
   RUN SessionAwareUI IN THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_ClearOutput
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_ClearOutput Dialog-Frame
ON VALUE-CHANGED OF tg_ClearOutput IN FRAME Dialog-Frame /* Clear output handlers before new analyze */
DO:
  ASSIGN tg_ClearOutput.
  pClearOutput = tg_ClearOutput.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

ON DROP-FILE-NOTIFY OF FRAME {&FRAME-NAME} /* Prolint- select files to lint */
DO:
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE numfiles AS INTEGER NO-UNDO.

   numfiles = FRAME {&FRAME-NAME}:NUM-DROPPED-FILES.
   DO i=1 TO numfiles:
      RUN AddFile (FRAME {&FRAME-NAME}:GET-DROPPED-FILE(i)).
   END.
   FRAME {&FRAME-NAME}:END-FILE-DROP().
END.


/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    PUBLISH "IsProlintPropertiesRunning":U (OUTPUT propsrunning).
    IF NOT propsrunning THEN
        RUN prolint/core/propsuper.p PERSISTENT.
    RUN IncrementProlintPropertySubscribers.
    
    /* check to see if the database is connected, and has the session table */
    /* to determine if we should show the session widgets.                  */
    ASSIGN  filemasks    = DYNAMIC-FUNCTION ("ProlintProperty", "compilationunit.filename.mask")
            fi_SessionID =  INTEGER(DYNAMIC-FUNCTION ("ProlintProperty", "SessionID")).
    RUN GetProfiles.
    RUN enable_UI.
    RUN SessionAwareUI IN THIS-PROCEDURE.
    
    /* enable drag/drop if the version allows it */
    IF SessionWindowSystem="GUI":U THEN
     FRAME {&FRAME-NAME}:DROP-TARGET = YES.
    
    Btn_rtbtask:VISIBLE IN FRAME {&FRAME-NAME} = CONNECTED("rtb":U).
    APPLY "entry":U TO ed_sourcefile IN FRAME {&FRAME-NAME}.
    
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
    RUN Destructor.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddFile Dialog-Frame 
PROCEDURE AddFile :
/*------------------------------------------------------------------------------
  Purpose:     add file to tt_sourcefiles if it doesn't already exist
  Parameters:  newfile = filename
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER newfile AS CHAR NO-UNDO.

  FILE-INFO:FILE-NAME = newfile.
  IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.

  IF FILE-INFO:FILE-TYPE MATCHES "*F*":U THEN DO:
     IF NOT CAN-DO(filemasks,newfile) THEN
        RETURN.
  END.

  newfile = RelativeFileName(newfile).

  DO WITH FRAME {&FRAME-NAME} :
     ed_sourcefile:MOVE-TO-TOP().
     ed_sourcefile:INSERT-STRING(newfile + "~n":U).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkOutputHandlers Dialog-Frame 
PROCEDURE checkOutputHandlers :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE CurrProf AS CHARACTER NO-UNDO.

/* get name of current profile */
ASSIGN CurrProf = cb_profile:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

/* Now go look at the output handlers in that profile. */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Destructor Dialog-Frame 
PROCEDURE Destructor :
/*------------------------------------------------------------------------------
  Purpose:     on close of the window, free all resources
------------------------------------------------------------------------------*/
  IF ModalResult=0 THEN
     FOR EACH tt_sourcefiles :
         DELETE tt_sourcefiles.
     END.
  RUN DecrementProlintPropertySubscribers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY cb_profile tg_ClearOutput ed_sourcefile fi_SessionID 
      WITH FRAME Dialog-Frame.
  ENABLE btn_new btn_find btn_session0 cb_profile tg_ClearOutput BT_BROWSE 
         ed_sourcefile Btn_rtbtask Btn_OK Btn_Cancel Btn_Help RT_SEPARATOR 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOutputHandlers Dialog-Frame 
PROCEDURE GetOutputHandlers :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pProfile   AS  CHARACTER NO-UNDO.

  DEFINE VARIABLE        lDirectory AS  CHARACTER NO-UNDO.
  DEFINE VARIABLE        handler    AS  CHARACTER NO-UNDO.
  DEFINE VARIABLE        DBHandler  AS  LOGICAL   NO-UNDO.
  DEFINE VARIABLE        hSessionTbl AS HANDLE    NO-UNDO.

  IF pProfile = ? OR pProfile = "" THEN RETURN ERROR "No Profile".

  RUN GetProfileDirectory IN THIS-PROCEDURE (pProfile, OUTPUT lDirectory) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.

  FILE-INFO:FILE-NAME = lDirectory + "/handlers.d":U.
  IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
     INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME).
     IMPORTLOOP: REPEAT:
       IMPORT handler.
       IF handler BEGINS 'prolintdb' then DO:
          ASSIGN DBHandler = true.
          leave IMPORTLOOP.
       END.
     END. /* IMPORTLOOP */
     INPUT CLOSE.
  END.

  IF DBHandler THEN DO:
     CREATE BUFFER hSessionTbl FOR TABLE "lint_session" NO-ERROR.
     ASSIGN SessionAware = (CONNECTED("prolintdb") and VALID-HANDLE(hSessionTbl)).
     DELETE OBJECT hSessionTbl no-error.
  END.
  ELSE ASSIGN SessionAware = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetProfileDirectory Dialog-Frame 
PROCEDURE GetProfileDirectory :
/*------------------------------------------------------------------------------
  Purpose:     determine the location of configuration settings.
               this would be "local-prolint/settings/" + pCustomProfile
               or "prolint/settings/ + pCustomProfile
               or just "prolint/settings"
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pCustomProfile    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ProfileDirectory  AS CHARACTER NO-UNDO.

DEFINE VARIABLE         PrivateDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE         SharedDir         AS CHARACTER NO-UNDO.
DEFINE VARIABLE         mandatory_d       AS CHARACTER NO-UNDO.
DEFINE VARIABLE         profile           AS CHARACTER NO-UNDO.

IF pCustomProfile = "":U OR pCustomProfile="<none>":U THEN DO:
   FILE-INFO:FILE-NAME = "prolint/settings":U.
   ProfileDirectory = FILE-INFO:FULL-PATHNAME.
END.
ELSE DO:
   FILE-INFO:FILE-NAME = "local-prolint/settings/":U + pCustomProfile.
   PrivateDir = FILE-INFO:FULL-PATHNAME.

   FILE-INFO:FILE-NAME  = "prolint/settings/":U + pCustomProfile.
   SharedDir = FILE-INFO:FULL-PATHNAME.

   IF PrivateDir=? AND SharedDir=? THEN
      ProfileDirectory = "prolint/settings":U.
   ELSE
   IF PrivateDir<>? AND SharedDir=? THEN
      ProfileDirectory = PrivateDir.
   ELSE
   IF PrivateDir=? AND SharedDir<>? THEN
      ProfileDirectory = SharedDir.
   ELSE
   IF PrivateDir<>? AND SharedDir<>? THEN DO:
      /* are private settings allowed to override shared settings? */
      FILE-INFO:FILE-NAME = SharedDir + "/no-local-settings.lk":U.
      IF FILE-INFO:FULL-PATHNAME = ? THEN
         ProfileDirectory = PrivateDir.
      ELSE
         ProfileDirectory = SharedDir.  /* ignore PrivateDir */
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetProfiles Dialog-Frame 
PROCEDURE GetProfiles :
/*------------------------------------------------------------------------------
  Purpose:     build a list of available profiles
------------------------------------------------------------------------------*/
  DEFINE VARIABLE basename AS CHAR NO-UNDO.
  {&_proparse_ prolint-nowarn(varusage)}
  DEFINE VARIABLE fullpath AS CHAR NO-UNDO.
  DEFINE VARIABLE attribs  AS CHAR NO-UNDO.
  DEFINE VARIABLE mruprofile AS CHAR NO-UNDO.
  DEFINE VARIABLE dirlist  AS CHAR NO-UNDO INITIAL "<none>":U.

  FILE-INFO:FILE-NAME = "prolint/settings":U.
  INPUT FROM OS-DIR (FILE-INFO:FULL-PATHNAME).
  REPEAT:
      IMPORT basename fullpath attribs.
      IF attribs MATCHES "*D*":U AND NOT(basename=".":U OR basename="..":U) THEN
         dirlist = dirlist + ",":U + basename.
  END.
  INPUT CLOSE.

  /* add project-specific (or user-specific) profiles: */
  FILE-INFO:FILE-NAME = "local-prolint/settings":U.
  IF FILE-INFO:FULL-PATHNAME<>? THEN DO:
      INPUT FROM OS-DIR (FILE-INFO:FULL-PATHNAME).
      REPEAT:
          IMPORT basename fullpath attribs.
          IF attribs MATCHES "*D*":U AND NOT(basename=".":U OR basename="..":U) THEN
             IF LOOKUP(basename, dirlist) = 0 THEN
                dirlist = dirlist + ",":U + basename.
      END.
      INPUT CLOSE.
  END.

  /* read most recently used profile (mruprofile) from Registry */
  mruprofile=?.
  IF OPSYS = "WIN32":U THEN DO:
     LOAD "SOFTWARE":U BASE-KEY "HKEY_CURRENT_USER":U.
     USE "SOFTWARE":U.
     GET-KEY-VALUE SECTION "Prolint\Selectfiles":U
                   KEY "mruprofile":U
                   VALUE mruprofile.
     UNLOAD "SOFTWARE":U.
  END.
  IF mruprofile=? THEN DO:
     MESSAGE "This appears to be the first time you're using Prolint. Welcome!" SKIP(1)
             "You will now select a profile, and enter some filenames or directorynames in the editor widget." SKIP
             "A profile is a customizable set of rules and outputhandlers." SKIP
             "You can also drag files or directories from the Explorer and drop them into the editor widget."
             VIEW-AS ALERT-BOX TITLE "Welcome to Prolint".
     IF SessionWindowSystem="TTY":U THEN
        mruprofile = "batchrun":U.
     ELSE
        mruprofile = "relaxed":U.
  END.

  DO WITH FRAME {&FRAME-NAME} :
     cb_profile:LIST-ITEMS = dirlist.
     IF LOOKUP(mruprofile,dirlist)>0 THEN
        cb_profile:SCREEN-VALUE = mruprofile.
     ELSE
         cb_profile:SCREEN-VALUE = "<none>".
     APPLY "value-changed":U TO cb_profile.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SessionAwareUI Dialog-Frame 
PROCEDURE SessionAwareUI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN btn_find:VISIBLE     = SessionAware
           btn_new:VISIBLE      = SessionAware
           btn_session0:VISIBLE = SessionAware
           fi_sessionID:VISIBLE = SessionAware.
    IF SessionAware THEN
      ENABLE  btn_find btn_new btn_Session0 fi_SessionID.
    ELSE DO:
      DISABLE btn_find btn_new btn_Session0 fi_SessionID.
      RUN SetProlintProperty ("SessionID", "").
      RUN SetProlintProperty ("SessionName", "").
    END.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK  _PROCEDURE CURRENT-WINDOW-layouts _LAYOUT-CASES
PROCEDURE CURRENT-WINDOW-layouts:
  DEFINE INPUT PARAMETER layout AS CHARACTER                     NO-UNDO.
  DEFINE VARIABLE lbl-hndl AS WIDGET-HANDLE                      NO-UNDO.
  DEFINE VARIABLE widg-pos AS DECIMAL                            NO-UNDO.

  /* Copy the name of the active layout into a variable accessible to   */
  /* the rest of this file.                                             */
  CURRENT-WINDOW-layout = layout.

  CASE layout:
    WHEN "Master Layout" THEN DO:
      ASSIGN
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         FRAME Dialog-Frame:HIDDEN                         = yes &ENDIF
         FRAME Dialog-Frame:HEIGHT                         = 19.52
               + FRAME Dialog-Frame:BORDER-TOP + FRAME Dialog-Frame:BORDER-BOTTOM
         FRAME Dialog-Frame:WIDTH                          = 93.8
               + FRAME Dialog-Frame:BORDER-LEFT + FRAME Dialog-Frame:BORDER-RIGHT.

      ASSIGN
         Btn_Cancel:HIDDEN IN FRAME Dialog-Frame           = yes
         Btn_Cancel:ROW IN FRAME Dialog-Frame              = 18.86
         Btn_Cancel:HIDDEN IN FRAME Dialog-Frame           = no.

      ASSIGN
         btn_find:HIDDEN IN FRAME Dialog-Frame             = yes
         btn_find:ROW IN FRAME Dialog-Frame                = 1.48
         btn_find:WIDTH IN FRAME Dialog-Frame              = 14.2
         btn_find:HIDDEN IN FRAME Dialog-Frame             = no.

      ASSIGN
         Btn_Help:HIDDEN IN FRAME Dialog-Frame             = yes
         Btn_Help:ROW IN FRAME Dialog-Frame                = 18.86
         Btn_Help:HIDDEN IN FRAME Dialog-Frame             = no.

      ASSIGN
         btn_new:HIDDEN IN FRAME Dialog-Frame              = yes
         btn_new:ROW IN FRAME Dialog-Frame                 = 1.48
         btn_new:WIDTH IN FRAME Dialog-Frame               = 14.2
         btn_new:HIDDEN IN FRAME Dialog-Frame              = no.

      ASSIGN
         Btn_OK:HIDDEN IN FRAME Dialog-Frame               = yes
         Btn_OK:ROW IN FRAME Dialog-Frame                  = 18.86
         Btn_OK:HIDDEN IN FRAME Dialog-Frame               = no.

      ASSIGN
         Btn_rtbtask:HIDDEN IN FRAME Dialog-Frame          = yes
         Btn_rtbtask:ROW IN FRAME Dialog-Frame             = 18.86
         Btn_rtbtask:HIDDEN IN FRAME Dialog-Frame          = no.

      ASSIGN
         btn_session0:HIDDEN IN FRAME Dialog-Frame         = yes
         btn_session0:ROW IN FRAME Dialog-Frame            = 1.48
         btn_session0:HIDDEN IN FRAME Dialog-Frame         = no.

      ASSIGN
         BT_BROWSE:HIDDEN IN FRAME Dialog-Frame            = yes
         BT_BROWSE:ROW IN FRAME Dialog-Frame               = 6.24
         BT_BROWSE:HIDDEN IN FRAME Dialog-Frame            = no.

      ASSIGN
         cb_profile:HIDDEN IN FRAME Dialog-Frame           = yes
         widg-pos = cb_profile:ROW IN FRAME Dialog-Frame 
         cb_profile:ROW IN FRAME Dialog-Frame              = 2.91
         lbl-hndl = cb_profile:SIDE-LABEL-HANDLE IN FRAME Dialog-Frame 
         lbl-hndl:ROW = lbl-hndl:ROW + cb_profile:ROW IN FRAME Dialog-Frame  - widg-pos
         cb_profile:HIDDEN IN FRAME Dialog-Frame           = no.

      ASSIGN
         ed_sourcefile:HIDDEN IN FRAME Dialog-Frame        = yes
         ed_sourcefile:HEIGHT IN FRAME Dialog-Frame        = 10.48
         ed_sourcefile:ROW IN FRAME Dialog-Frame           = 7.91
         ed_sourcefile:HIDDEN IN FRAME Dialog-Frame        = no.

      ASSIGN
         fi_SessionID:HIDDEN IN FRAME Dialog-Frame         = yes
         fi_SessionID:HEIGHT IN FRAME Dialog-Frame         = .62
         widg-pos = fi_SessionID:ROW IN FRAME Dialog-Frame 
         fi_SessionID:ROW IN FRAME Dialog-Frame            = 1.71
         lbl-hndl = fi_SessionID:SIDE-LABEL-HANDLE IN FRAME Dialog-Frame 
         lbl-hndl:ROW = lbl-hndl:ROW + fi_SessionID:ROW IN FRAME Dialog-Frame  - widg-pos
         fi_SessionID:HIDDEN IN FRAME Dialog-Frame         = no.

      ASSIGN
         RT_SEPARATOR:EDGE-PIXELS IN FRAME Dialog-Frame    = 2
         RT_SEPARATOR:HIDDEN IN FRAME Dialog-Frame         = yes
         RT_SEPARATOR:HEIGHT IN FRAME Dialog-Frame         = .1
         RT_SEPARATOR:ROW IN FRAME Dialog-Frame            = 5.76
         RT_SEPARATOR:HIDDEN IN FRAME Dialog-Frame         = no.

      ASSIGN
         tg_ClearOutput:HIDDEN IN FRAME Dialog-Frame       = yes
         tg_ClearOutput:HEIGHT IN FRAME Dialog-Frame       = .81
         tg_ClearOutput:ROW IN FRAME Dialog-Frame          = 4.57
         tg_ClearOutput:HIDDEN IN FRAME Dialog-Frame       = no.

      ASSIGN

         FRAME Dialog-Frame:VIRTUAL-HEIGHT                 = 19.52
                    WHEN FRAME Dialog-Frame:SCROLLABLE

         FRAME Dialog-Frame:VIRTUAL-WIDTH                  = 93.80
                    WHEN FRAME Dialog-Frame:SCROLLABLE
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         FRAME Dialog-Frame:HIDDEN                         = no &ENDIF.

    END.  /* Master Layout Layout Case */

    WHEN "Standard Character":U THEN DO:
      ASSIGN
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         FRAME Dialog-Frame:HIDDEN                         = yes &ENDIF
         FRAME Dialog-Frame:HEIGHT                         = 15
               + FRAME Dialog-Frame:BORDER-TOP + FRAME Dialog-Frame:BORDER-BOTTOM
         FRAME Dialog-Frame:WIDTH                          = 73
               + FRAME Dialog-Frame:BORDER-LEFT + FRAME Dialog-Frame:BORDER-RIGHT NO-ERROR.

      ASSIGN
         Btn_Cancel:HIDDEN IN FRAME Dialog-Frame           = yes
         Btn_Cancel:ROW IN FRAME Dialog-Frame              = 19
         Btn_Cancel:HIDDEN IN FRAME Dialog-Frame           = no NO-ERROR.

      ASSIGN
         btn_find:HIDDEN IN FRAME Dialog-Frame             = yes
         btn_find:ROW IN FRAME Dialog-Frame                = 1
         btn_find:WIDTH IN FRAME Dialog-Frame              = 14
         btn_find:HIDDEN IN FRAME Dialog-Frame             = no NO-ERROR.

      ASSIGN
         Btn_Help:HIDDEN IN FRAME Dialog-Frame             = yes
         Btn_Help:ROW IN FRAME Dialog-Frame                = 19
         Btn_Help:HIDDEN IN FRAME Dialog-Frame             = no NO-ERROR.

      ASSIGN
         btn_new:HIDDEN IN FRAME Dialog-Frame              = yes
         btn_new:ROW IN FRAME Dialog-Frame                 = 1
         btn_new:WIDTH IN FRAME Dialog-Frame               = 14
         btn_new:HIDDEN IN FRAME Dialog-Frame              = no NO-ERROR.

      ASSIGN
         Btn_OK:HIDDEN IN FRAME Dialog-Frame               = yes
         Btn_OK:ROW IN FRAME Dialog-Frame                  = 19
         Btn_OK:HIDDEN IN FRAME Dialog-Frame               = no NO-ERROR.

      ASSIGN
         Btn_rtbtask:HIDDEN IN FRAME Dialog-Frame          = yes
         Btn_rtbtask:ROW IN FRAME Dialog-Frame             = 19
         Btn_rtbtask:HIDDEN IN FRAME Dialog-Frame          = no NO-ERROR.

      ASSIGN
         btn_session0:HIDDEN IN FRAME Dialog-Frame         = yes
         btn_session0:ROW IN FRAME Dialog-Frame            = 1
         btn_session0:HIDDEN IN FRAME Dialog-Frame         = no NO-ERROR.

      ASSIGN
         BT_BROWSE:HIDDEN IN FRAME Dialog-Frame            = yes
         BT_BROWSE:ROW IN FRAME Dialog-Frame               = 6
         BT_BROWSE:HIDDEN IN FRAME Dialog-Frame            = no NO-ERROR.

      ASSIGN
         cb_profile:HIDDEN IN FRAME Dialog-Frame           = yes
         widg-pos = cb_profile:ROW IN FRAME Dialog-Frame 
         cb_profile:ROW IN FRAME Dialog-Frame              = 3
         lbl-hndl = cb_profile:SIDE-LABEL-HANDLE IN FRAME Dialog-Frame 
         lbl-hndl:ROW = lbl-hndl:ROW + cb_profile:ROW IN FRAME Dialog-Frame  - widg-pos
         cb_profile:HIDDEN IN FRAME Dialog-Frame           = no NO-ERROR.

      ASSIGN
         ed_sourcefile:HIDDEN IN FRAME Dialog-Frame        = yes
         ed_sourcefile:HEIGHT IN FRAME Dialog-Frame        = 10
         ed_sourcefile:ROW IN FRAME Dialog-Frame           = 8
         ed_sourcefile:HIDDEN IN FRAME Dialog-Frame        = no NO-ERROR.

      ASSIGN
         fi_SessionID:HIDDEN IN FRAME Dialog-Frame         = yes
         fi_SessionID:HEIGHT IN FRAME Dialog-Frame         = 1
         widg-pos = fi_SessionID:ROW IN FRAME Dialog-Frame 
         fi_SessionID:ROW IN FRAME Dialog-Frame            = 2
         lbl-hndl = fi_SessionID:SIDE-LABEL-HANDLE IN FRAME Dialog-Frame 
         lbl-hndl:ROW = lbl-hndl:ROW + fi_SessionID:ROW IN FRAME Dialog-Frame  - widg-pos
         fi_SessionID:HIDDEN IN FRAME Dialog-Frame         = no NO-ERROR.

      ASSIGN
         RT_SEPARATOR:EDGE-PIXELS IN FRAME Dialog-Frame    = 1
         RT_SEPARATOR:GRAPHIC-EDGE IN FRAME Dialog-Frame  = no
         RT_SEPARATOR:HIDDEN IN FRAME Dialog-Frame         = yes
         RT_SEPARATOR:HEIGHT IN FRAME Dialog-Frame         = 1
         RT_SEPARATOR:ROW IN FRAME Dialog-Frame            = 6
         RT_SEPARATOR:HIDDEN IN FRAME Dialog-Frame         = no NO-ERROR.

      ASSIGN
         tg_ClearOutput:HIDDEN IN FRAME Dialog-Frame       = yes
         tg_ClearOutput:HEIGHT IN FRAME Dialog-Frame       = 1
         tg_ClearOutput:ROW IN FRAME Dialog-Frame          = 5
         tg_ClearOutput:HIDDEN IN FRAME Dialog-Frame       = no NO-ERROR.

      ASSIGN

         FRAME Dialog-Frame:VIRTUAL-HEIGHT                 = 20.00
                    WHEN FRAME Dialog-Frame:SCROLLABLE

         FRAME Dialog-Frame:VIRTUAL-WIDTH                  = 94.00
                    WHEN FRAME Dialog-Frame:SCROLLABLE
         &IF '{&WINDOW-SYSTEM}' NE 'TTY':U &THEN
         FRAME Dialog-Frame:HIDDEN                         = no &ENDIF NO-ERROR.

    END.  /* Standard Character Layout Case */

  END CASE.
END PROCEDURE.  /* CURRENT-WINDOW-layouts */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RelativeFileName Dialog-Frame 
FUNCTION RelativeFileName RETURNS CHARACTER
  ( pFileName AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE fullpath  AS CHAR NO-UNDO.
   DEFINE VARIABLE shortpath AS CHAR NO-UNDO.
   DEFINE VARIABLE i         AS INTEGER NO-UNDO.

   /* first replace all backslashes by forwardslashes */
   pFileName = REPLACE(pFileName, '~\':U, '/':U).

   FILE-INFO:FILE-NAME = pFileName.
   fullpath = FILE-INFO:FULL-PATHNAME.

   shortpath = ENTRY(NUM-ENTRIES(pFileName, '/':U), pFileName, '/':U).
   DO i=NUM-ENTRIES(pFileName, '/':U) TO 2 BY -1 :
      FILE-INFO:FILE-NAME = shortpath.
      IF FILE-INFO:FULL-PATHNAME = fullpath THEN
         RETURN shortpath.
      shortpath = ENTRY(i - 1, pFileName, '/':U) + '/':U + shortpath.
   END.

   RETURN pFileName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

