&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File:           prolint/outputhandlers/dlgfilter.w

  Description:    set filter parameters for logwin.w

  Input Parameters:
      <none>

  Output Parameters:
      <none>


-------------------------------------------------------------------------

    Copyright (C) 2003 Jurjen Dijkstra

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

----------------------------------------------------------------------- */

/* ***************************  Definitions  ************************** */
{prolint/core/dlc-version.i}


/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER fltDesc        AS CHARACTER NO-UNDO INITIAL "*":U.
DEFINE INPUT-OUTPUT PARAMETER fltMaxSeverity AS INTEGER   NO-UNDO INITIAL 9.
DEFINE INPUT-OUTPUT PARAMETER fltMinSeverity AS INTEGER   NO-UNDO INITIAL 0.
DEFINE INPUT-OUTPUT PARAMETER fltCompUnit    AS CHARACTER NO-UNDO INITIAL "*":U.
DEFINE INPUT-OUTPUT PARAMETER fltSource      AS CHARACTER NO-UNDO INITIAL "*":U.
DEFINE INPUT-OUTPUT PARAMETER fltRule        AS CHARACTER NO-UNDO INITIAL "*":U.
DEFINE INPUT-OUTPUT PARAMETER fltPersistent  AS LOGICAL   NO-UNDO INITIAL FALSE.

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
&Scoped-Define ENABLED-OBJECTS ed_Desc tg_error ed_MinSeverity ~
ed_MaxSeverity ed_Compunit ed_Source ed_Rule tg_persistent Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS ed_Desc tg_error ed_MinSeverity ~
ed_MaxSeverity ed_Compunit ed_Source ed_Rule tg_persistent 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE ed_Compunit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Comp.unit matches" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE ed_Desc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Description matches" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE ed_MaxSeverity AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Severity <=" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ed_MinSeverity AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Severity >=" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ed_Rule AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rule matches" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE ed_Source AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sourcefile matches" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tg_error AS LOGICAL INITIAL no 
     LABEL "Filter on 'ERROR:*'" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg_persistent AS LOGICAL INITIAL no 
     LABEL "Save filter settings in registry" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ed_Desc AT ROW 1.95 COL 24 COLON-ALIGNED
     tg_error AT ROW 2.05 COL 59
     ed_MinSeverity AT ROW 3.14 COL 24 COLON-ALIGNED
     ed_MaxSeverity AT ROW 4.33 COL 24 COLON-ALIGNED
     ed_Compunit AT ROW 5.52 COL 24 COLON-ALIGNED
     ed_Source AT ROW 6.71 COL 24 COLON-ALIGNED
     ed_Rule AT ROW 7.91 COL 24 COLON-ALIGNED
     tg_persistent AT ROW 9.33 COL 26
     Btn_OK AT ROW 10.76 COL 33
     SPACE(39.39) SKIP(0.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Filter for Prolint Results"
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Filter for Prolint Results */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  
    DO WITH FRAME {&FRAME-NAME} :
       ASSIGN 
         ed_Desc
         ed_MaxSeverity
         ed_MinSeverity
         ed_Compunit
         ed_Source
         ed_Rule
         tg_Persistent.
    END.

    ASSIGN 
      fltDesc        = ed_Desc
      fltMaxSeverity = ed_MaxSeverity
      fltMinSeverity = ed_MinSeverity
      fltCompunit    = ed_CompUnit
      fltSource      = ed_Source
      fltRule        = ed_Rule
      fltPersistent  = tg_Persistent.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_error
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_error Dialog-Frame
ON VALUE-CHANGED OF tg_error IN FRAME Dialog-Frame /* Filter on 'ERROR:*' */
DO:
  ASSIGN tg_error ed_Desc.

  IF tg_error THEN 
     IF NOT ed_Desc MATCHES "ERROR:*" THEN
        ed_Desc = "ERROR: " + ed_Desc.

  IF NOT tg_error THEN 
     IF ed_Desc MATCHES "ERROR: *" THEN
        ed_Desc = SUBSTRING(ed_Desc,8).
     ELSE
        IF ed_Desc MATCHES "ERROR:*" THEN
           ed_Desc = SUBSTRING(ed_Desc,7).

  DISPLAY ed_Desc WITH FRAME {&FRAME-NAME}.

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

  ASSIGN 
    tg_Error       = fltDesc MATCHES "ERROR:*"
    ed_Desc        = fltDesc
    ed_MaxSeverity = fltMaxSeverity
    ed_MinSeverity = fltMinSeverity
    ed_Compunit    = fltCompunit
    ed_Source      = fltSource
    ed_Rule        = fltRule
    tg_Persistent  = fltPersistent.

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
  DISPLAY ed_Desc tg_error ed_MinSeverity ed_MaxSeverity ed_Compunit ed_Source 
          ed_Rule tg_persistent 
      WITH FRAME Dialog-Frame.
  ENABLE ed_Desc tg_error ed_MinSeverity ed_MaxSeverity ed_Compunit ed_Source 
         ed_Rule tg_persistent Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

