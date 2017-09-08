&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:
      prolint/core/lintcfg.w

  Description:
      UI for maintaining the configuration files in prolint/settings

  Input Parameters:
      pInitialProfile : initial value for combo-box "profile"

  Output Parameters:
      <none>

  Author:
      Jurjen Dijkstra

  Created:
      July 3, 2001

    -----------------------------------------------------------------

    Copyright (C) 2001,2002,2003,2004 Jurjen Dijkstra

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

/* pInitialProfile : initial value for combo-box "profile" */
&IF DEFINED(UIB_is_running)=0 &THEN
DEFINE INPUT PARAMETER pInitialProfile AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE pInitialProfile AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE CurrentProfile AS CHAR NO-UNDO INITIAL "":U.
DEFINE VARIABLE PrivateDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE SharedDir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE RecentDir      AS CHARACTER NO-UNDO.

DEFINE VARIABLE categorytext AS CHARACTER NO-UNDO INITIAL "selected:  &1 out of &2":T.

DEFINE VARIABLE cDefaultProfile  AS CHARACTER  NO-UNDO.




{prolint/core/tt_rules.i}

DEFINE TEMP-TABLE tt_output NO-UNDO
   FIELD progname     AS CHARACTER FORMAT "x(15)":U
   FIELD DlcVersion   AS INTEGER   FORMAT ">9":U
   FIELD WindowSystem AS CHARACTER
   FIELD description  AS CHARACTER FORMAT "x(80)":U
   FIELD required     AS LOGICAL FORMAT "yes/no":U INITIAL NO
   INDEX idx_progname AS PRIMARY UNIQUE progname.

DEFINE TEMP-TABLE tt_category NO-UNDO
   FIELD catname      AS CHARACTER
   FIELD hframe       AS HANDLE
   FIELD hText        AS HANDLE
   FIELD numrules     AS INTEGER.

define stream rulemanifest.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brw_output

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_output

/* Definitions for BROWSE brw_output                                    */
&Scoped-define FIELDS-IN-QUERY-brw_output tt_output.progname tt_output.required tt_output.description   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw_output tt_output.required   
&Scoped-define ENABLED-TABLES-IN-QUERY-brw_output tt_output
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brw_output tt_output
&Scoped-define SELF-NAME brw_output
&Scoped-define QUERY-STRING-brw_output FOR EACH tt_output
&Scoped-define OPEN-QUERY-brw_output OPEN QUERY {&SELF-NAME} FOR EACH tt_output.
&Scoped-define TABLES-IN-QUERY-brw_output tt_output
&Scoped-define FIRST-TABLE-IN-QUERY-brw_output tt_output


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brw_output}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_localdel btn_help cb-profiles ~
BT_SET_DEFAUTL btn_localcopy brw_output btn_new btn_shareddel ed_where 
&Scoped-Define DISPLAYED-OBJECTS cb-profiles ed_where 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_help 
     LABEL "Help":T 
     SIZE 12 BY 1.14 TOOLTIP "Help".

DEFINE BUTTON btn_localcopy 
     IMAGE-UP FILE "prolint/images/mklocal.bmp":U
     IMAGE-INSENSITIVE FILE "prolint/images/mklocali.bmp":U NO-FOCUS
     LABEL "Local Copy" 
     SIZE 12 BY 1.14 TOOLTIP "Make a local copy of this shared profile".

DEFINE BUTTON btn_localdel 
     IMAGE-UP FILE "prolint/images/dellocal.bmp":U
     IMAGE-INSENSITIVE FILE "prolint/images/dellocali.bmp":U NO-FOCUS
     LABEL "Delete Local" 
     SIZE 12 BY 1.14 TOOLTIP "Delete this local profile (and maybe revert to shared profile)".

DEFINE BUTTON btn_new 
     IMAGE-UP FILE "prolint/images/new.bmp":U NO-FOCUS
     LABEL "New..." 
     SIZE 12 BY 1.14 TOOLTIP "Create a new profile".

DEFINE BUTTON btn_shareddel 
     IMAGE-UP FILE "prolint/images/delshared.bmp":U
     IMAGE-INSENSITIVE FILE "prolint/images/delsharedi.bmp":U NO-FOCUS
     LABEL "Delete Shared" 
     SIZE 12 BY 1.14 TOOLTIP "Delete this shared profile".

DEFINE BUTTON BT_SET_DEFAUTL 
     LABEL "&Set as default":T 
     SIZE 21 BY 1.14 TOOLTIP "Help".

DEFINE VARIABLE cb-profiles AS CHARACTER FORMAT "X(256)":U INITIAL "<none>" 
     LABEL "Profile":T 
     VIEW-AS COMBO-BOX SORT INNER-LINES 15
     LIST-ITEMS "<none>" 
     DROP-DOWN-LIST
     SIZE 79 BY 1 NO-UNDO.

DEFINE VARIABLE ed_where AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 79 BY .62 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw_output FOR 
      tt_output SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw_output
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw_output C-Win _FREEFORM
  QUERY brw_output DISPLAY
      tt_output.progname     FORMAT "x(15)":U  WIDTH-CHARS 15
   tt_output.required     FORMAT "yes/no":U
   tt_output.description  FORMAT "x(80)":U  LABEL "description"

enable tt_output.required
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 109 BY 10.57 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btn_localdel AT ROW 1.24 COL 29
     btn_help AT ROW 1.24 COL 56
     cb-profiles AT ROW 2.86 COL 8 COLON-ALIGNED
     BT_SET_DEFAUTL AT ROW 2.86 COL 91
     btn_localcopy AT ROW 1.24 COL 16
     brw_output AT ROW 6.14 COL 3
     btn_new AT ROW 1.24 COL 3
     btn_shareddel AT ROW 1.24 COL 42
     ed_where AT ROW 4.05 COL 8 COLON-ALIGNED NO-LABEL
     "Select 1 or more outputhandlers:":T VIEW-AS TEXT
          SIZE 32 BY .71 AT ROW 5.05 COL 3
     "Select which rules you want to run, by category:":T VIEW-AS TEXT
          SIZE 51 BY .71 AT ROW 17.67 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.2 BY 30.71.

DEFINE FRAME FRAME-categories
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 3 ROW 18.86
         SCROLLABLE SIZE 108 BY 12.38
         BGCOLOR 15 .


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
         TITLE              = "Prolint Profile configuration"
         HEIGHT             = 30.71
         WIDTH              = 112.2
         MAX-HEIGHT         = 31.52
         MAX-WIDTH          = 120
         VIRTUAL-HEIGHT     = 31.52
         VIRTUAL-WIDTH      = 120
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-categories:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brw_output btn_localcopy DEFAULT-FRAME */
ASSIGN 
       brw_output:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR FRAME FRAME-categories
   UNDERLINE                                                            */
ASSIGN 
       FRAME FRAME-categories:HEIGHT           = 12.38
       FRAME FRAME-categories:WIDTH            = 108.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw_output
/* Query rebuild information for BROWSE brw_output
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_output.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brw_output */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-categories
/* Query rebuild information for FRAME FRAME-categories
     _Query            is NOT OPENED
*/  /* FRAME FRAME-categories */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Prolint Profile configuration */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Prolint Profile configuration */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "ENTRY":U TO cb-profiles IN FRAME {&FRAME-NAME}.
  RUN SaveProfile (CurrentProfile).
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_help C-Win
ON CHOOSE OF btn_help IN FRAME DEFAULT-FRAME /* Help */
DO:
   RUN prolint/core/openhtml.p ( "http://oehive.org/node/250":U ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_localcopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_localcopy C-Win
ON CHOOSE OF btn_localcopy IN FRAME DEFAULT-FRAME /* Local Copy */
DO:
    DEFINE VARIABLE targetdir AS CHARACTER NO-UNDO.
    DEFINE VARIABLE basename  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fullpath  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE attribs   AS CHARACTER NO-UNDO.

    ASSIGN cb-profiles.

    IF cb-profiles=? OR cb-profiles="" OR cb-profiles="<none>":U THEN
       RETURN NO-APPLY.

    targetdir = PrivateDir + "/":U + cb-profiles.
    FILE-INFO:FILE-NAME = targetdir.
    IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
       OS-CREATE-DIR VALUE(targetdir).

       FILE-INFO:FILE-NAME = SharedDir + "/":U + cb-profiles.
       IF FILE-INFO:FULL-PATHNAME<>? THEN DO:
           INPUT FROM OS-DIR (FILE-INFO:FULL-PATHNAME).
           REPEAT:
               IMPORT basename fullpath attribs.
               IF NOT (attribs MATCHES "*D*":U OR basename=".":U OR basename="..":U) THEN
                  OS-COPY VALUE(fullpath) VALUE(targetdir).
           END.
           INPUT CLOSE.
       END.



       RUN ImportCustomData.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_localdel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_localdel C-Win
ON CHOOSE OF btn_localdel IN FRAME DEFAULT-FRAME /* Delete Local */
DO:
  ASSIGN cb-profiles.

  IF cb-profiles=? OR cb-profiles="" OR cb-profiles="<none>":U THEN
     RETURN NO-APPLY.

  FILE-INFO:FILE-NAME = "local-prolint/settings/" + cb-profiles.
  IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
     OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME) RECURSIVE.
     cb-profiles:LIST-ITEMS = "<none>":U.
     RUN GetProfiles.
     cb-profiles:SCREEN-VALUE = "<none>":U.
     CurrentProfile = "<none>":U.
     APPLY "value-changed":U TO cb-profiles.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_new
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_new C-Win
ON CHOOSE OF btn_new IN FRAME DEFAULT-FRAME /* New... */
DO:
  DEFINE VARIABLE newprofile AS CHARACTER NO-UNDO.

  RUN prolint/core/dnewprofile.w (OUTPUT newprofile).
  IF newprofile <> ? THEN DO:
     RUN SaveProfile(CurrentProfile).
     cb-profiles:LIST-ITEMS = cb-profiles:LIST-ITEMS + ",":U + newprofile.
     cb-profiles:SCREEN-VALUE = newprofile.
     APPLY "value-changed":U TO cb-profiles.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_shareddel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_shareddel C-Win
ON CHOOSE OF btn_shareddel IN FRAME DEFAULT-FRAME /* Delete Shared */
DO:
    ASSIGN cb-profiles.

    IF cb-profiles=? OR cb-profiles="" OR cb-profiles="<none>":U THEN
       RETURN NO-APPLY.

    FILE-INFO:FILE-NAME = "prolint/settings/" + cb-profiles.
    IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
       OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME) RECURSIVE.
       cb-profiles:LIST-ITEMS = "<none>":U.
       RUN GetProfiles.
       cb-profiles:SCREEN-VALUE = "<none>":U.
       CurrentProfile = "<none>":U.
       APPLY "value-changed":U TO cb-profiles.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT_SET_DEFAUTL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT_SET_DEFAUTL C-Win
ON CHOOSE OF BT_SET_DEFAUTL IN FRAME DEFAULT-FRAME /* Set as default */
DO:

    /* Save pProfile in registry as most recently used profile */
    IF CB-PROFILES:SCREEN-VALUE <> ? AND OPSYS = "WIN32":U THEN DO:
        LOAD "SOFTWARE":U BASE-KEY "HKEY_CURRENT_USER":U.
            USE "SOFTWARE":U.
            PUT-KEY-VALUE SECTION "Prolint\Selectfiles":U
            KEY "mruprofile":U
            VALUE CB-PROFILES:SCREEN-VALUE NO-ERROR.
        UNLOAD "SOFTWARE":U.

        RUN ImportCustomData.
        MESSAGE QUOTER(CB-PROFILES:SCREEN-VALUE) + " was set as your default profile." VIEW-AS ALERT-BOX INFORMATION.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-profiles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-profiles C-Win
ON VALUE-CHANGED OF cb-profiles IN FRAME DEFAULT-FRAME /* Profile */
DO:
    ASSIGN cb-profiles.
    IF cb-profiles <> CurrentProfile THEN
        RUN SaveProfile(CurrentProfile).
    CurrentProfile = cb-profiles.
    RUN ImportCustomData.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brw_output
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

    RUN GetDirectories.
    RUN ImportDefaultData.
    RUN enable_UI.
    RUN GetProfiles.
    RUN CreateCategories.
    RUN DisplayCategoryUsage.


    ASSIGN cDefaultProfile = ?.
    IF OPSYS = "WIN32":U THEN DO:
       LOAD "SOFTWARE":U BASE-KEY "HKEY_CURRENT_USER":U.
       USE "SOFTWARE":U.
       GET-KEY-VALUE SECTION "Prolint\Selectfiles":U
                     KEY "mruprofile":U
                     VALUE cDefaultProfile.
       UNLOAD "SOFTWARE":U.

       IF cDefaultProfile <> ? THEN DO:
           ASSIGN CB-PROFILES:SCREEN-VALUE = "<none>".                  /* This is not a mistale */
           ASSIGN CB-PROFILES:SCREEN-VALUE = cDefaultProfile NO-ERROR.  /* This is not a mistale */
           APPLY "VALUE-CHANGED":U TO CB-PROFILES.
       END.
    END.


    APPLY "ENTRY":U TO cb-profiles.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateCategories C-Win 
PROCEDURE CreateCategories :
/*------------------------------------------------------------------------------
  Purpose:     Make a list of categories.
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

  FOR EACH tt_rules :
     FIND tt_category WHERE tt_category.catname = tt_rules.category NO-ERROR.
     IF NOT AVAILABLE tt_category THEN DO:
        CREATE tt_category.
        ASSIGN tt_category.catname = tt_rules.category.
     END.
     ASSIGN tt_category.numrules = tt_category.numrules + 1.
  END.

  RUN CreateCategoryFrames.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateCategoryFrames C-Win 
PROCEDURE CreateCategoryFrames :
/*------------------------------------------------------------------------------
  Purpose:     Create a "browse" where each row is in fact a frame,
               representing a category
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE frameheight AS INTEGER NO-UNDO INITIAL 25.
  DEFINE VARIABLE NextY       AS INTEGER NO-UNDO.
  DEFINE VARIABLE hwidget     AS HANDLE  NO-UNDO.
  DEFINE VARIABLE parentheight AS INTEGER NO-UNDO.

  parentheight = FRAME frame-categories:HEIGHT-PIXELS.

  FOR EACH tt_category BY tt_category.catname :
      CREATE FRAME tt_category.hframe
           ASSIGN
              PARENT = FRAME frame-categories:FIRST-CHILD
              X = 0
              Y = NextY
              HEIGHT-PIXELS = frameheight
              WIDTH-PIXELS = FRAME frame-categories:WIDTH-PIXELS
              THREE-D = TRUE
              BGCOLOR = FRAME frame-categories:BGCOLOR
              FGCOLOR = FRAME frame-categories:FGCOLOR
              BOX = NO
              VISIBLE = YES.

      CREATE RECTANGLE hwidget
           ASSIGN
              PARENT = tt_category.hframe:FIRST-CHILD
              X = 0
              Y = frameheight - 1
              HEIGHT-PIXELS = 1
              WIDTH-PIXELS = tt_category.hframe:WIDTH-PIXELS
              FGCOLOR = 8
              VISIBLE = YES.

      NextY = NextY + frameheight.

      CREATE TEXT hwidget
          ASSIGN
              PARENT = tt_category.hframe:FIRST-CHILD
              X = 10
              Y = 3
              WIDTH-PIXELS  = 200
              HEIGHT-PIXELS = 20
              FORMAT = "x(256)"
              SCREEN-VALUE  = tt_category.catname
              /* FONT = 6 */
              VISIBLE = YES.

      CREATE BUTTON hwidget
          ASSIGN
              PARENT = tt_category.hframe:FIRST-CHILD
              X = 220
              Y = 3
              WIDTH-PIXELS = 50
              HEIGHT-PIXELS = 20
              LABEL = "All"
              VISIBLE = YES
              SENSITIVE = YES
          TRIGGERS:
              ON "CHOOSE":U PERSISTENT RUN SelectCategoryAll IN THIS-PROCEDURE (tt_category.catname).
          END TRIGGERS.

      CREATE BUTTON hwidget
          ASSIGN
              PARENT = tt_category.hframe:FIRST-CHILD
              X = 280
              Y = 3
              WIDTH-PIXELS = 50
              HEIGHT-PIXELS = 20
              LABEL = "None"
              VISIBLE = YES
              SENSITIVE = YES
          TRIGGERS:
              ON "CHOOSE":U PERSISTENT RUN SelectCategoryNone IN THIS-PROCEDURE (tt_category.catname).
          END TRIGGERS.

      CREATE BUTTON hwidget
          ASSIGN
              PARENT = tt_category.hframe:FIRST-CHILD
              X = 340
              Y = 3
              WIDTH-PIXELS = 60
              HEIGHT-PIXELS = 20
              LABEL = "Some..."
              VISIBLE = YES
              SENSITIVE = YES
          TRIGGERS:
              ON "CHOOSE":U PERSISTENT RUN SelectCategorySome IN THIS-PROCEDURE (tt_category.catname).
          END TRIGGERS.

      CREATE TEXT tt_Category.hText
          ASSIGN
              PARENT = tt_category.hframe:FIRST-CHILD
              X = 430
              Y = 3
              WIDTH-PIXELS  = 150
              HEIGHT-PIXELS = 20
              FORMAT = "x(256)"
              SCREEN-VALUE  = ""
              VISIBLE = YES.

  END.

  FRAME frame-categories:VIRTUAL-HEIGHT-PIXELS = NextY.
  FRAME frame-categories:HEIGHT-PIXELS = parentheight.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayCategoryUsage C-Win 
PROCEDURE DisplayCategoryUsage :
/*------------------------------------------------------------------------------
  Purpose:     Display how many rules are selected, by category
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE numselected  AS INTEGER NO-UNDO.

  FOR EACH tt_category :
     numselected = 0.
     FOR EACH tt_rules WHERE tt_rules.category = tt_category.catname
                         AND tt_rules.required = TRUE:
          numselected = numselected + 1.
     END.
     tt_category.hText:SCREEN-VALUE = SUBSTITUTE(categorytext, numselected, tt_category.numrules).
  END.


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
  DISPLAY cb-profiles ed_where 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btn_localdel btn_help cb-profiles BT_SET_DEFAUTL btn_localcopy 
         brw_output btn_new btn_shareddel ed_where 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME FRAME-categories IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-categories}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDirectories C-Win 
PROCEDURE GetDirectories :
/*------------------------------------------------------------------------------
  Purpose:
------------------------------------------------------------------------------*/

   FILE-INFO:FILE-NAME  = "prolint/settings":U.
   SharedDir = FILE-INFO:FULL-PATHNAME.

   FILE-INFO:FILE-NAME = "local-prolint/settings":U.
   PrivateDir = FILE-INFO:FULL-PATHNAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetProfiles C-Win 
PROCEDURE GetProfiles :
/*------------------------------------------------------------------------------
  Purpose:     build a list of available profiles
------------------------------------------------------------------------------*/
  DEFINE VARIABLE basename AS CHAR NO-UNDO.
  {&_proparse_ prolint-nowarn(varusage)}
  DEFINE VARIABLE fullpath AS CHAR NO-UNDO.
  DEFINE VARIABLE attribs  AS CHAR NO-UNDO.
  DEFINE VARIABLE dirlist  AS CHAR NO-UNDO INITIAL "<none>":U.

  FILE-INFO:FILE-NAME = SharedDir.
  INPUT FROM OS-DIR (FILE-INFO:FULL-PATHNAME).
  REPEAT:
      IMPORT basename fullpath attribs.
      IF attribs MATCHES "*D*":U AND NOT(basename=".":U OR basename="..":U) THEN
         dirlist = dirlist + ",":U + basename.
  END.
  INPUT CLOSE.

  /* add project-specific (or user-specific) profiles: */
  FILE-INFO:FILE-NAME = PrivateDir.
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

  DO WITH FRAME {&FRAME-NAME} :
     cb-profiles:LIST-ITEMS = dirlist.
     IF LOOKUP(pInitialProfile,dirlist) EQ 0 THEN
        cb-profiles:SCREEN-VALUE = "<none>":U.
     ELSE
        cb-profiles:SCREEN-VALUE = pInitialProfile.
     APPLY "value-changed":U TO cb-profiles.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportCustomData C-Win 
PROCEDURE ImportCustomData :
/*------------------------------------------------------------------------------
  Purpose:     import settings for currently selected profile
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ProfileDirectory AS CHARACTER NO-UNDO.
  DEFINE VARIABLE customrequired   AS LOGICAL NO-UNDO.
  DEFINE VARIABLE customrule       AS CHAR    NO-UNDO.
  DEFINE VARIABLE customlevel      AS INTEGER NO-UNDO.
  DEFINE VARIABLE handler          AS CHAR    NO-UNDO.

  DEFINE VARIABLE SharedDirProfile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE PrivateDirProfile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE foundlocally      AS LOGICAL INITIAL FALSE.
  DEFINE VARIABLE no-override       AS LOGICAL INITIAL FALSE.


   FOR EACH tt_rules :
      ASSIGN
         tt_rules.required = TRUE
         tt_rules.customseverity = tt_rules.severity.
   END.

   /* is it a local profile, a shared profile or "<none>" ? */
   run prolint/core/getprofiledir.p (cb-profiles, output ProfileDirectory, output foundlocally, output no-override).

   /* remember where you found it. That's also where you will save it */
   RecentDir = ProfileDirectory.

   /* enable/disable buttons. */
   DO WITH FRAME {&FRAME-NAME} :
       IF cb-profiles="<none>":U THEN
           ASSIGN
/*              ed_where:SCREEN-VALUE   = "default profile"*/                                       
              btn_localcopy:SENSITIVE = FALSE
              btn_localdel:SENSITIVE  = FALSE
              btn_shareddel:SENSITIVE = FALSE.
       ELSE
           IF PrivateDir=? THEN
                 ASSIGN
/*                    ed_where:SCREEN-VALUE   = ""*/                                                
                    btn_localcopy:SENSITIVE = FALSE
                    btn_localdel:SENSITIVE  = FALSE
                    btn_shareddel:SENSITIVE = TRUE.
           ELSE
              IF foundlocally THEN
                 ASSIGN
/*                    ed_where:SCREEN-VALUE   = "local profile (in local-prolint/settings)"*/       
                    btn_localcopy:SENSITIVE = FALSE
                    btn_localdel:SENSITIVE  = TRUE
                    btn_shareddel:SENSITIVE = FALSE.
              ELSE
                 ASSIGN
/*                    ed_where:SCREEN-VALUE   = "shared profile (in prolint/settings)"*/            
                    btn_localcopy:SENSITIVE = NOT no-override
                    btn_localdel:SENSITIVE  = FALSE
                    btn_shareddel:SENSITIVE = NOT no-override.

    ASSIGN cDefaultProfile = ?.
    IF OPSYS = "WIN32":U THEN DO:
       LOAD "SOFTWARE":U BASE-KEY "HKEY_CURRENT_USER":U.
       USE "SOFTWARE":U.
       GET-KEY-VALUE SECTION "Prolint\Selectfiles":U
                     KEY "mruprofile":U
                     VALUE cDefaultProfile.
       UNLOAD "SOFTWARE":U.
       IF cb-profiles:SCREEN-VALUE = cDefaultProfile THEN
           ASSIGN ed_where:SCREEN-VALUE   = "<< Default Profile >>".
       ELSE
           ASSIGN ed_where:SCREEN-VALUE   = "".
    END.


   END.

   /* if there is no sverity.d, read profile "<none>",
      but still save as recentdir */
   FILE-INFO:FILE-NAME = ProfileDirectory + "/severity.d":U.
   IF FILE-INFO:FULL-PATHNAME = ? THEN
      ProfileDirectory = "prolint/settings":U.

   FILE-INFO:FILE-NAME = ProfileDirectory + "/severity.d":U.
   IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
      INPUT FROM VALUE(file-info:FULL-PATHNAME).
      REPEAT:
         customlevel = -1. /* if still -1 after import, means that user doesn't want to modify the default */
         IMPORT customrequired customrule customlevel.
         FIND tt_rules WHERE tt_rules.RuleID = customrule NO-ERROR.
         IF AVAILABLE tt_rules THEN DO:
             IF NOT customlevel=-1 THEN
                ASSIGN tt_rules.customseverity = customlevel.
             ASSIGN
                 tt_rules.required = customrequired.
         END.
      END.
      INPUT CLOSE.
   END.

   FOR EACH tt_output :
       ASSIGN
          tt_output.required = FALSE.
   END.

   FILE-INFO:FILE-NAME = ProfileDirectory + "/handlers.d":U.
   IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
      INPUT FROM VALUE(file-info:FULL-PATHNAME).
      REPEAT:
         IMPORT handler.
         FIND tt_output WHERE tt_output.progname = handler NO-ERROR.
         IF AVAILABLE tt_output THEN
            ASSIGN tt_output.required = TRUE.
      END.
      INPUT CLOSE.
   END.

   OPEN QUERY   brw_output FOR EACH tt_output.
   RUN DisplayCategoryUsage.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportDefaultData C-Win 
PROCEDURE ImportDefaultData :
/* import the list of rules */
   run prolint/core/findrules.p ("/all/":U, output table tt_rules).

   /* import list of outputhandlers */
   FILE-INFO:FILE-NAME = "prolint/outputhandlers/choices.d":U.
   IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
      INPUT FROM VALUE(file-info:FULL-PATHNAME).
      REPEAT:
         CREATE tt_output.
         IMPORT tt_output.
      END.
      INPUT CLOSE.
   END.

   FOR EACH tt_output WHERE tt_output.progname = "" :
       DELETE tt_output.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveProfile C-Win 
PROCEDURE SaveProfile :
/*------------------------------------------------------------------------------
  Purpose:     on close of window, or value-changed of combo, first save the
               current settings to the (previous) profile directory
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pProfile AS CHAR NO-UNDO.

  IF pProfile = "" THEN RETURN.

  DEFINE VARIABLE vDirectory AS CHAR NO-UNDO.

  CASE pProfile :
      WHEN "<none>":U THEN vDirectory = "prolint/settings":U.
      OTHERWISE            vDirectory = RecentDir.
  END CASE.

  FILE-INFO:FILE-NAME = vDirectory.
  vDirectory = FILE-INFO:FULL-PATHNAME.

  OUTPUT TO VALUE (vDirectory + "/severity.d":U).
     FOR EACH tt_rules WHERE tt_rules.required=FALSE
                          OR (tt_rules.customseverity <> tt_rules.severity) :
         IF (tt_rules.customseverity <> tt_rules.severity) THEN
           EXPORT tt_rules.required
                  tt_rules.ruleid
                  tt_rules.customseverity.
         ELSE
           EXPORT tt_rules.required
                  tt_rules.ruleid.
     END.
  OUTPUT CLOSE.

  OUTPUT TO VALUE (vDirectory + "/handlers.d":U).
     FOR EACH tt_output WHERE tt_output.required=TRUE :
         EXPORT tt_output.progname.
     END.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectCategoryAll C-Win 
PROCEDURE SelectCategoryAll :
/*------------------------------------------------------------------------------
  Purpose:     Select all rules in a given category
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pCategory AS CHARACTER NO-UNDO.

  FOR EACH tt_rules WHERE tt_rules.category = pCategory :
      tt_rules.required = TRUE.
  END.
  FIND tt_category WHERE tt_category.catname = pCategory NO-ERROR.
  IF AVAILABLE tt_category THEN
     tt_category.hText:SCREEN-VALUE = SUBSTITUTE(categorytext, tt_category.numrules, tt_category.numrules).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectCategoryNone C-Win 
PROCEDURE SelectCategoryNone :
/*------------------------------------------------------------------------------
  Purpose:     Unselect all rules in a given category
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pCategory AS CHARACTER NO-UNDO.

  FOR EACH tt_rules WHERE tt_rules.category = pCategory :
      tt_rules.required = FALSE.
  END.

  FIND tt_category WHERE tt_category.catname = pCategory NO-ERROR.
  IF AVAILABLE tt_category THEN
     tt_category.hText:SCREEN-VALUE = SUBSTITUTE(categorytext, 0, tt_category.numrules).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectCategorySome C-Win 
PROCEDURE SelectCategorySome :
/*------------------------------------------------------------------------------
  Purpose:     Select some rules in a given category.
               Open a dialog, containing a browse widget of tt_rules
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pCategory AS CHARACTER NO-UNDO.

  DEFINE VARIABLE numselected  AS INTEGER NO-UNDO.

  RUN prolint/core/lintcfgbycat.w (INPUT pCategory,
                              INPUT-OUTPUT TABLE tt_rules).


  FOR EACH tt_category WHERE tt_category.catname = pCategory :
     numselected = 0.
     FOR EACH tt_rules WHERE tt_rules.category = tt_category.catname
                         AND tt_rules.required = TRUE:
          numselected = numselected + 1.
     END.
     tt_category.hText:SCREEN-VALUE = SUBSTITUTE(categorytext, numselected, tt_category.numrules).
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

