&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: prolint/outputhandlers/codepreview.w

  Description: Displays the source of the currently selected source on 
               the ProLint Result Browser.  

  Input Parameters:
      phCalleHandle - The handle of the caller procedure that run this 
                      procedure

  Output Parameters:
      <none>

  Author: Ildefonzo Arocha
  Created: 17.October.2003
  
  Update by: Ildefonzo Arocha
  Update on: 12.Mar.2004
    Changes: - Added navigation buttons on the top for scrolling along messages
             - Added editing possibilities. The code preview window displays a small
               lock and sets the file to read-only if the program exists in RTB and
               is not checked-out under the current task
             - Added a status indicator that display the message string and 
               a color if the error is severe/warning or none.
             - Use Syntax Coloring
             - Added Icon on the Status Bar that indicates if the program
               was not able to reposition to the line correctly
             - Double-Click on the Warning launches help on that rule
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

/*CREATE WIDGET-POOL "_ADE_Procedure_Window".*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

&SCOPED-DEFINE WINTITLE Code Preview
&IF DEFINED( AppBuilder_is_Running ) <> 0 &THEN
  DEFINE VARIABLE phCallerHandle AS HANDLE NO-UNDO.
&ELSE
  DEFINE INPUT PARAMETER phCallerHandle AS HANDLE NO-UNDO.
&ENDIF

DEFINE VARIABLE ghParentWindow AS HANDLE    NO-UNDO.
DEFINE VARIABLE gcFileName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcFullFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE glDock         AS LOGICAL   NO-UNDO INITIAL FALSE.

DEFINE VARIABLE giColor AS INTEGER NO-UNDO INITIAL ?.
DEFINE VARIABLE EdEditor AS HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE vhRtbCustFunc AS HANDLE NO-UNDO.

 /* Load these definitions only if RTB is available */
{prolint/outputhandlers/logwin.i}

/* Used for Syntax coloring */
{adecomm/adestds.i}
{adecomm/peditor.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tempEditor fiDesc btn_close I-LOCK ~
R-SEVERITY 
&Scoped-Define DISPLAYED-OBJECTS tempEditor fiDesc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_close 
     IMAGE-UP FILE "prolint/images/exit.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Exit" 
     SIZE 4.8 BY 1.14 TOOLTIP "Close this window"
     BGCOLOR 8 .

DEFINE BUTTON buFirst 
     IMAGE-UP FILE "prolint/images/first.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "First" 
     SIZE 4.8 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON buLast 
     IMAGE-UP FILE "prolint/images/last.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Last" 
     SIZE 4.8 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON buNext 
     IMAGE-UP FILE "prolint/images/next.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Next" 
     SIZE 4.8 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON buPrev 
     IMAGE-UP FILE "prolint/images/prev.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Prev" 
     SIZE 4.8 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON buSave 
     IMAGE-UP FILE "prolint/images/savelog.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 4.8 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE tempEditor AS CHARACTER 
     CONTEXT-HELP-ID 0
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 30 BY 4 NO-UNDO.

DEFINE VARIABLE fiDesc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 69.2 BY .86 TOOLTIP "Double-Click to get help on this rule"
     FONT 6 NO-UNDO.

DEFINE IMAGE I-LOCK
     FILENAME "prolint/images/locked.gif":U TRANSPARENT
     SIZE 3.2 BY .76.

DEFINE IMAGE imRepositionFailed
     FILENAME "prolint/images/repositionfailed.gif":U
     SIZE 3.2 BY .76.

DEFINE RECTANGLE R-SEVERITY
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2.8 BY .67
     BGCOLOR 2 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     buFirst AT ROW 1.1 COL 1.6
     buLast AT ROW 1.1 COL 17.2
     tempEditor AT ROW 6.81 COL 19 NO-LABEL
     buNext AT ROW 1.1 COL 12
     fiDesc AT ROW 15.86 COL 9.8 COLON-ALIGNED NO-LABEL
     buPrev AT ROW 1.1 COL 6.8
     buSave AT ROW 1.1 COL 22.2
     btn_close AT ROW 1.1 COL 75.8
     imRepositionFailed AT ROW 15.91 COL 7.8
     I-LOCK AT ROW 15.91 COL 1
     R-SEVERITY AT ROW 15.95 COL 4.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 15.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Code Preview"
         HEIGHT             = 15.71
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         PRIVATE-DATA       = "_ab.p":U
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
                                                                        */
/* SETTINGS FOR BUTTON buFirst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON buLast IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON buNext IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON buPrev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON buSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiDesc:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR IMAGE imRepositionFailed IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       imRepositionFailed:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       tempEditor:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Code Preview */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Code Preview */
DO:
  /* Result window will run destroyObject in this procedure */
  PUBLISH "Prolint_codePreviewClosed".
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Code Preview */
DO:
  RUN resizeTo( {&WINDOW-NAME}:WIDTH-PIXELS , {&WINDOW-NAME}:HEIGHT-PIXELS ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_close C-Win
ON CHOOSE OF btn_close IN FRAME DEFAULT-FRAME /* Exit */
DO:
  PUBLISH "Prolint_codePreviewClosed".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buFirst C-Win
ON CHOOSE OF buFirst IN FRAME DEFAULT-FRAME /* First */
DO:
  IF VALID-HANDLE( phCallerHandle ) THEN 
    RUN repositionTo IN phCallerHandle ( "FIRST":U ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buLast C-Win
ON CHOOSE OF buLast IN FRAME DEFAULT-FRAME /* Last */
DO:
  IF VALID-HANDLE( phCallerHandle ) THEN 
    RUN repositionTo IN phCallerHandle ( "LAST":U ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buNext C-Win
ON CHOOSE OF buNext IN FRAME DEFAULT-FRAME /* Next */
DO:
  IF VALID-HANDLE( phCallerHandle ) THEN 
    RUN repositionTo IN phCallerHandle ( "NEXT":U ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buPrev C-Win
ON CHOOSE OF buPrev IN FRAME DEFAULT-FRAME /* Prev */
DO:
  IF VALID-HANDLE( phCallerHandle ) THEN 
    RUN repositionTo IN phCallerHandle ( "PREV":U ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buSave C-Win
ON CHOOSE OF buSave IN FRAME DEFAULT-FRAME /* Save */
DO:
  RUN saveFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiDesc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiDesc C-Win
ON MOUSE-SELECT-DBLCLICK OF fiDesc IN FRAME DEFAULT-FRAME
DO:
  IF VALID-HANDLE( phCallerHandle ) THEN DO:
    RUN BrowseHelp IN phCallerHandle.
  END.
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
   RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* set the minimum and maximum window size */
ASSIGN
  {&WINDOW-NAME}:MIN-WIDTH-PIXELS  = 80
  {&WINDOW-NAME}:MIN-HEIGHT-PIXELS = 16
  {&WINDOW-NAME}:MAX-WIDTH-PIXELS  = SESSION:WIDTH-PIXELS
  {&WINDOW-NAME}:MAX-HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS
  /* Default the window position to the bottom right corner of the session.
     Progress 9.1c23, Progress is not returning correctly the Work Area Size if you have 
     customized the size of the Task Bar, I use a double sized Task Bar for example */
  {&WINDOW-NAME}:X = SESSION:WORK-AREA-WIDTH-PIXELS  - ( {&WINDOW-NAME}:WIDTH-PIXELS + 10 )
  {&WINDOW-NAME}:Y = SESSION:WORK-AREA-HEIGHT-PIXELS - ( {&WINDOW-NAME}:HEIGHT-PIXELS + 10 ).

/* Create are own custom color, but only if its not created otherwise
   we create one color each time we run this window */
IF giColor = ? THEN DO:
  giColor = COLOR-TABLE:NUM-ENTRIES.
  COLOR-TABLE:NUM-ENTRIES = giColor + 1.
  COLOR-TABLE:SET-DYNAMIC( giColor , TRUE ).
END.
  
/* Get the parent window handle for docking function */
IF VALID-HANDLE( phCallerHandle ) THEN
  ghParentWindow = phCallerHandle:CURRENT-WINDOW.

/* Create the Editor Widget */
CREATE EDITOR EdEditor 
ASSIGN
  COL         = 1
  ROW         = 2.38
  WIDTH-CHARS = 80
  HEIGHT      = 13.38
  SCROLLBAR-V = YES
  SCROLLBAR-H = YES
  FONT        = editor_font         /* From adestds.i */
  FGCOLOR     = std_ed4gl_fgcolor   /* From adestds.i */
  BGCOLOR     = std_ed4gl_bgcolor   /* From adestds.i */
  AUTO-INDENT = TRUE
  LARGE       = TRUE
  PROGRESS-SOURCE = YES  /* No-Op on GUI, but ok to have. */
  FRAME       = FRAME {&FRAME-NAME}:HANDLE.

  RUN SetEditor (INPUT EdEditor ).
  RUN SetEdHelpFile.

  ASSIGN EdEditor:VISIBLE          = TRUE
         EdEditor:SENSITIVE        = TRUE
         EdEditor:MODIFIED         = FALSE
         imRepositionFailed:HIDDEN = TRUE.

RUN loadUserSettings.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN loadUserSettings.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkForChanges C-Win 
PROCEDURE checkForChanges :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER pcAction AS CHARACTER NO-UNDO INITIAL "":U.

DEFINE VARIABLE lSave AS LOGICAL NO-UNDO.

IF edEditor:MODIFIED THEN DO:
  MESSAGE 
     gcFullFilename SKIP
    "This Program has changes which have not been saved." SKIP(1)
    "Save changes before closing ?"
    VIEW-AS ALERT-BOX WARNING BUTTON YES-NO-CANCEL UPDATE lSave.
  IF lSave THEN DO:
    RUN saveFile.
  END.
  /* Cancel */
  IF lSave = ? THEN DO:
    pcAction = "CANCEL":U.
    RETURN.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject C-Win 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cOption AS CHARACTER NO-UNDO.

  RUN checkForChanges( OUTPUT cOption ).
  IF cOption = "CANCEL":U THEN RETURN.
  
  RUN saveUserSettings.
  
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dockWindow C-Win 
PROCEDURE dockWindow :
/*------------------------------------------------------------------------------
  Purpose:     This procedure "Docks" the code preview below the Result Window
               This functionallity is currently not working as in order to 
               it to work properly I will need a "WINDOW-MOVED" event, which
               Progress does not support at the moment.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE( ghParentWindow ) THEN RETURN.

/* Set the new window position */
ASSIGN
  {&WINDOW-NAME}:Y = ghParentWindow:Y + ghParentWindow:HEIGHT-PIXELS
  {&WINDOW-NAME}:X = ghParentWindow:X.

/* Resize Width to parent window */
RUN resizeTo( {&WINDOW-NAME}:HEIGHT-PIXELS , ghParentWindow:WIDTH-PIXELS ).
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
  DISPLAY tempEditor fiDesc 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tempEditor fiDesc btn_close I-LOCK R-SEVERITY 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadFile C-Win 
PROCEDURE loadFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcFileName AS CHARACTER NO-UNDO.

DEFINE VARIABLE lAllowEdit        AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE cOption           AS CHARACTER NO-UNDO.

/* Already loaded ? return */
IF gcFileName = pcFileName THEN RETURN.

DO WITH FRAME {&FRAME-NAME}:
  RUN checkForChanges( OUTPUT cOption ).
  IF cOption = "CANCEL":U THEN RETURN.

  ASSIGN
    lAllowEdit          = FALSE
    gcFileName          = "":U
    gcFullFileName      = "":U
    FILE-INFO:FILE-NAME = pcFileName.

  IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
    MESSAGE "Cannot locate source file " + pcFileName VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.
  
  IF EdEditor:READ-FILE( FILE-INFO:FULL-PATHNAME ) THEN DO:
    IF NOT tempEditor:READ-FILE( FILE-INFO:FULL-PATHNAME ) THEN
      MESSAGE SUBSTITUTE( "Error when loading &1 into the temp editor. ~n~n" +
                          "Line repositioning will not be accurate !!" ,
                          FILE-INFO:FULL-PATHNAME
                         ) VIEW-AS ALERT-BOX ERROR.

    ASSIGN
      gcFileName           = pcFileName
      gcFullFileName       = FILE-INFO:FULL-PATHNAME  /* Store the full path for the save function */
      {&WINDOW-NAME}:TITLE = SUBSTITUTE( "{&WINTITLE} - &1":U , pcFileName )
      edEditor:MODIFIED    = FALSE. /* Reset the modified flag */
  END.
  ELSE DO:
    MESSAGE "Error when loading " + FILE-INFO:FULL-PATHNAME  VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.
  
  /* If RTB is running only allow editing if the object is checekd out */
  IF VALID-HANDLE( vhRtbCustFunc ) THEN
    lAllowEdit = DYNAMIC-FUNCTION( "fnRtbAllowFileEdit" IN vhRtbCustFunc , FILE-INFO:FULL-PATHNAME ).
  ELSE
    lAllowEdit = TRUE.

  ASSIGN
    buSave:SENSITIVE   = lAllowEdit
    EdEditor:READ-ONLY = NOT lAllowEdit.
  I-LOCK:LOAD-IMAGE( SUBSTITUTE( "prolint/images/&1locked.gif" , IF lAllowEdit THEN "un" ELSE "" ) ).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadUserSettings C-Win 
PROCEDURE loadUserSettings :
/*------------------------------------------------------------------------------
  Purpose:     Loads all user settings from the Registry
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cCodePreviewX     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCodePreviewY     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCodePreviewSizeX AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCodePreviewSizeY AS CHARACTER NO-UNDO.

DEFINE VARIABLE piX     AS INTEGER NO-UNDO.
DEFINE VARIABLE piY     AS INTEGER NO-UNDO.
DEFINE VARIABLE piSizeX AS INTEGER NO-UNDO.
DEFINE VARIABLE piSizeY AS INTEGER NO-UNDO.

LOAD "SOFTWARE":U BASE-KEY "HKEY_CURRENT_USER":U.
USE "SOFTWARE":U.

GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "CodePreviewX"     VALUE cCodePreviewX.
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "CodePreviewY"     VALUE cCodePreviewY.
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "CodePreviewSizeX" VALUE cCodePreviewSizeX.
GET-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "CodePreviewSizeY" VALUE cCodePreviewSizeY.
UNLOAD "SOFTWARE".

ASSIGN
  piX     = INTEGER( cCodePreviewX )
  piy     = INTEGER( cCodePreviewY )
  piSizeX = INTEGER( cCodePreviewSizeX )
  piSizeY = INTEGER( cCodePreviewSizeY ) NO-ERROR.

IF piSizeX > 0 AND piSizeY > 0 THEN DO:
  ASSIGN
    {&WINDOW-NAME}:X = piX
    {&WINDOW-NAME}:Y = piY
    {&WINDOW-NAME}:WIDTH-PIXELS  = piSizeX
    {&WINDOW-NAME}:HEIGHT-PIXELS = piSizeY.
  RUN resizeTo( piSizeX , piSizeY ).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parentResized C-Win 
PROCEDURE parentResized :
/*------------------------------------------------------------------------------
  Purpose:     Runs if the parent has been resized - used for docking function
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF glDock THEN
    RUN dockWindow.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repositionToLine C-Win 
PROCEDURE repositionToLine :
/*------------------------------------------------------------------------------
  Purpose:     Repositions the Editor to some line
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piLineNumber AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER pcText       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER piSeverity   AS INTEGER   NO-UNDO.

DEFINE VARIABLE iNewColor          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iStartOffSet       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEndOffset         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSearchString      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iRelativeLinesUp   AS INTEGER   NO-UNDO INITIAL ?.
DEFINE VARIABLE iRelativeLinesDown AS INTEGER   NO-UNDO INITIAL ?.

IF gcFileName = "" THEN RETURN.

IF piLineNumber = ? OR piLineNumber = 0 THEN piLineNumber = 1.

DO WITH FRAME {&FRAME-NAME}:
  IF ABS( edEditor:NUM-LINES - TempEditor:NUM-LINES ) > 10 THEN DO:
    MESSAGE "The source has had to many changes, repositioning to lines could be incorrect. ~n~n" + 
            "It is highly recommended to ProLint this source code again !"
    VIEW-AS ALERT-BOX WARNING.
  END.
  IF pcText = ? THEN pcText = "".
  ASSIGN
    /* Change window title */
    {&WINDOW-NAME}:TITLE      = SUBSTITUTE( "{&WINTITLE} - &1 (&2)":U , gcFileName , piLineNumber )
    fiDesc:SCREEN-VALUE       = pcText
    fiDesc:TOOLTIP            = pcText
    imRepositionFailed:HIDDEN = TRUE
    /* Position the cursor to proper line.  On previous version the line was selected (highlighted)
    this functionality is lost when using the Syntax Higlighling :( */
    edEditor:CURSOR-LINE = piLineNumber.  

  /* Workaround for KB 3527, we use a temp editor to get the offset where the line starts and ends.
     We select the line, read the text and search for that text in the other editor */
  ASSIGN
    iStartOffset = tempEditor:CONVERT-TO-OFFSET(  piLineNumber , 1 )
    iEndOffSet   = tempEditor:CONVERT-TO-OFFSET( piLineNumber + 1 , 1 ).
  tempEditor:SET-SELECTION( iStartOffset , iEndOffset - 1 ).
  cSearchString = tempEditor:SELECTION-TEXT.
 
  /* Try to somehow reposition to the correct line, note that this is tricky as the user could
     of added lines or deleted them, which in this case all line numbers are no longer valid.
     What the algorithim does is find the text in the source code and then check the line number
     where it was found if its different than where it should be, we allow this diffrence be
     max of 10 lines. The text can be found either up or down from its original location, we
     have to search in both directions */

  /* We give preference searching upwards, as usually when removing ProLint errors, we delete lines.
     So we position the cursor one line below where the original line should be and search upwards */
  IF piLineNumber + 1 > edEditor:NUM-LINES THEN
    edEditor:MOVE-TO-EOF().
  ELSE
    edEditor:CURSOR-LINE = piLineNumber + 1.

  IF NOT edEditor:SEARCH( cSearchString , 38 ) THEN  /* 38 = Case Sensitive + Find & Select + Previous Occurance */
    iRelativeLinesUp = ?.
  ELSE DO:
    ASSIGN
      iRelativeLinesUp = ABS( edEditor:CURSOR-LINE - piLineNumber ) /* Calculate the relative position of the found line */
      iRelativeLinesUp = IF iRelativeLinesUp > 10 THEN ? ELSE iRelativeLinesUp.  /* If found more than 10 lines up we assume not found */
  END.

  /* If not found at the position where it should be then search downwards */
  IF iRelativeLinesUp <> 0 THEN DO:
    IF piLineNumber + 1 > edEditor:NUM-LINES THEN
      edEditor:MOVE-TO-EOF().
    ELSE
      edEditor:CURSOR-LINE = piLineNumber + 1.

    IF NOT edEditor:SEARCH( cSearchString , 37 ) THEN  /* 37 = Case Sensitive + Find & Select + Next Occurance */
      iRelativeLinesDown = ?.
    ELSE DO:
      ASSIGN
        iRelativeLinesDown = ABS( edEditor:CURSOR-LINE - piLineNumber ) /* Calculate the relative position of the found line */
        iRelativeLinesDown = IF iRelativeLinesDown > 10 THEN ? ELSE iRelativeLinesDown.  /* If found more than 10 lines up we assume not found */
    END.
  END.

  /* So now we have how many lines the text was located either upwards or downwards */
  IF iRelativeLinesDown <> ? OR iRelativeLinesUp <> ? THEN DO:  /* If it was found */
    /* Always give preference to closest offset, if they are the same, give prefference to upwards */
    IF iRelativeLinesDown = iRelativeLinesUp OR iRelativeLinesUp <= iRelativeLinesDown OR iRelativeLinesDown = ?  THEN DO:
      IF piLineNumber + 1 > edEditor:NUM-LINES THEN
        edEditor:MOVE-TO-EOF().
      ELSE
        edEditor:CURSOR-LINE = piLineNumber + 1.

      edEditor:SEARCH( cSearchString , 38 ).  /* 38 = Case Sensitive + Find & Select + Previous Occurance */
    END.
    ELSE DO:
      IF piLineNumber + 1 > edEditor:NUM-LINES THEN
        edEditor:MOVE-TO-EOF().
      ELSE
        edEditor:CURSOR-LINE = piLineNumber.

      edEditor:SEARCH( cSearchString , 37 ).  /* 37 = Case Sensitive + Find & Select + Next Occurance */
    END.
  END.
  ELSE DO:
    /* What to do if not found ? Display a warning message and go to line 1 */
    ASSIGN
      imRepositionFailed:VISIBLE  = TRUE
      edEditor:CURSOR-LINE        = piLineNumber.
      imRepositionFailed:TOOLTIP  = SUBSTITUTE( "Fail to reposition source to line &1~n-----~n&2" , piLineNumber , cSearchString ).
    MESSAGE SUBSTITUTE( "Cannot reposition to line &1" , piLineNumber ) VIEW-AS ALERT-BOX WARNING.
  END.

  edEditor:CURSOR-CHAR = 1.
  APPLY "ENTRY" TO edEditor.
  
  /* Use our user defined dynamic color according to the severity */
  IF piSeverity >= {&NODE_HIGH_START_LIMIT} THEN
    iNewColor = {&NODE_HIGH_COLOR}.
  ELSE 
    IF piSeverity >= {&NODE_MED_START_LIMIT} THEN
      iNewColor = {&NODE_MEDIUM_COLOR}.
    ELSE
      iNewColor = {&NODE_LOW_COLOR}.
  
  COLOR-TABLE:SET-RGB-VALUE( giColor , iNewColor ).
  ASSIGN
    R-SEVERITY:BGCOLOR = giColor
    R-SEVERITY:TOOLTIP = SUBSTITUTE( "Severity Level: &1" , piSeverity ).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetObject C-Win 
PROCEDURE resetObject :
/*------------------------------------------------------------------------------
  Purpose:     Blanks the contents of the editors.
               This procedure is called whenever we think the
               source might have changed (prolint again for exmaple)
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cAction AS CHARACTER NO-UNDO.

RUN checkForChanges( OUTPUT cAction ).
IF cAction = "CANCEL":U THEN RETURN.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
    gcFileName                = ""   /* Current filename */
    tempEditor:SCREEN-VALUE   = ""   /* Temporal Editor  */
    edEditor:SCREEN-VALUE     = ""   /* Dynamic Code Editor */
    fiDesc:SCREEN-VALUE       = ""
    imRepositionFailed:HIDDEN = TRUE
    edEditor:MODIFIED         = FALSE.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeTo C-Win 
PROCEDURE resizeTo :
/*------------------------------------------------------------------------------
  Purpose:     Resizes the window - I hate resizing procedures :(
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piNewWidth  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER piNewHeight AS INTEGER NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
   ASSIGN
    {&WINDOW-NAME}:WIDTH-PIXELS  = piNewWidth 
    {&WINDOW-NAME}:HEIGHT-PIXELS = piNewHeight. 

  IF piNewHeight < FRAME {&FRAME-NAME}:HEIGHT-PIXELS THEN 
    ASSIGN 
      fiDesc:Y                                  = piNewHeight - ( fiDesc:HEIGHT-PIXELS + 1 )
      I-LOCK:Y                                  = fiDesc:Y + 1
      R-SEVERITY:Y                              = fiDesc:Y + 1
      imRepositionFailed:Y                      = fiDesc:Y + 1
      EdEditor:HEIGHT-PIXELS                    = piNewHeight - ( EdEditor:Y + fiDesc:HEIGHT-PIXELS + 2 )
      FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = piNewHeight.

  IF piNewHeight > FRAME {&FRAME-NAME}:HEIGHT-PIXELS THEN 
    ASSIGN 
      FRAME {&FRAME-NAME}:HEIGHT-PIXELS = piNewHeight 
      fiDesc:Y                          = piNewHeight - ( fiDesc:HEIGHT-PIXELS + 1 )
      I-LOCK:Y                          = fiDesc:Y + 1
      imRepositionFailed:Y              = fiDesc:Y + 1
      R-SEVERITY:Y                      = fiDesc:Y + 1
      EdEditor:HEIGHT-PIXELS            = piNewHeight - ( EdEditor:Y + fiDesc:HEIGHT-PIXELS + 2 ) .
      
  IF piNewWidth < FRAME {&FRAME-NAME}:WIDTH-PIXELS THEN 
    ASSIGN 
      btn_close:X                              = piNewWidth - ( btn_close:WIDTH-PIXELS + 2 )     
      fiDesc:WIDTH-PIXELS                      = piNewWidth - fiDesc:X
      EdEditor:WIDTH-PIXELS                    = piNewWidth - EdEditor:X
      FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = piNewWidth.

  IF piNewWidth > FRAME {&FRAME-NAME}:WIDTH-PIXELS THEN 
    ASSIGN 
      FRAME {&FRAME-NAME}:WIDTH-PIXELS = piNewWidth
      EdEditor:WIDTH-PIXELS            = piNewWidth - EdEditor:X
      btn_close:X                      = piNewWidth - ( btn_close:WIDTH-PIXELS + 2 )
      fiDesc:WIDTH-PIXELS              = piNewWidth - fiDesc:X.
  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveFile C-Win 
PROCEDURE saveFile :
/*------------------------------------------------------------------------------
  Purpose:     Saves the file
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF edEditor:MODIFIED AND gcFileName <> "":U AND gcFullFileName <> "":U THEN
    IF NOT edEditor:SAVE-FILE( gcFullFileName ) THEN
      MESSAGE "An Error occured when saving file " + gcFullFileName VIEW-AS ALERT-BOX ERROR.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveUserSettings C-Win 
PROCEDURE saveUserSettings :
/*------------------------------------------------------------------------------
  Purpose:     Saves settings to the Registry
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
LOAD "SOFTWARE":U BASE-KEY "HKEY_CURRENT_USER":U.
USE "SOFTWARE":U.
/* Code Preview Window Properties */
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "CodePreviewX"     VALUE ( STRING( {&WINDOW-NAME}:X             ) )  NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "CodePreviewY"     VALUE ( STRING( {&WINDOW-NAME}:Y             ) )  NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "CodePreviewSizeX" VALUE ( STRING( {&WINDOW-NAME}:WIDTH-PIXELS  ) )  NO-ERROR.
PUT-KEY-VALUE SECTION "Prolint\Output Handlers\logwin" KEY "CodePreviewSizeY" VALUE ( STRING( {&WINDOW-NAME}:HEIGHT-PIXELS ) )  NO-ERROR.
UNLOAD "SOFTWARE".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setButtonsState C-Win 
PROCEDURE setButtonsState :
/*------------------------------------------------------------------------------
  Purpose:     Enables or disables the buttons
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER plFirstButtonState AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER plPrevButtonState  AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER plNextButtonState  AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER plLastButtonState  AS LOGICAL NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
    buFirst:SENSITIVE = plFirstButtonState
    buPrev:SENSITIVE  = plPrevButtonState 
    buNext:SENSITIVE  = plNextButtonState 
    buLast:SENSITIVE  = plLastButtonState.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

