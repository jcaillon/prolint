/* ============================================================================
   file    : protools/rtb/addmenu.p
   by      : Jurjen Dijkstra
   purpose : add Prolint to the menu of the Roundtable desktop
   History :
   April 21, 2005 Carl Verbiest
   * add prolint button @ original compile position & size to accomodate for 
     old-standard PPU (default font size)
     
   Thomas Hansen, appSolutions 02/2008:
   Updated code to use RTB event model and variables from RTB 10.1B
     
   ============================================================================ */
                                        
DEFINE VARIABLE hWindow        AS HANDLE  NO-UNDO.
DEFINE VARIABLE hMenu          AS HANDLE  NO-UNDO.              
DEFINE VARIABLE hFrame         AS HANDLE  NO-UNDO.
DEFINE VARIABLE hItem          AS HANDLE  NO-UNDO.
DEFINE VARIABLE hButton        AS HANDLE  NO-UNDO.
DEFINE VARIABLE added          AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iLintX         AS INTEGER NO-UNDO.
DEFINE VARIABLE hCompItem      AS HANDLE NO-UNDO.

DEFINE VARIABLE hRTBDeskHandle AS HANDLE      NO-UNDO.

PUBLISH "evRtbGetDeskHandle":U (OUTPUT hRTBDeskHandle). 
IF NOT VALID-HANDLE(hRTBDeskHandle) THEN
   RETURN. 

hWindow = hRTBDeskHandle:CURRENT-WINDOW.
IF NOT VALID-HANDLE(hWindow) THEN 
   RETURN.

/* locate menu-item "m_Tools" */
hMenu = hWindow:MENUBAR.
hMenu = hMenu:FIRST-CHILD.
DO WHILE (hMenu<>?) AND hMenu:LABEL <> "T&ool":U :
   hMenu = hMenu:NEXT-SIBLING.
END.                              

/* menu-item not found? oops, give up - current-window may not be the rtb desktop */
IF hMenu=? THEN RETURN.

/* look at items in the Tool menu, see if "Prolint" is already added */
hItem = hMenu:FIRST-CHILD.
DO WHILE hItem<>? :                
   IF hItem:NAME MATCHES "m_Prolint_*":U THEN 
      added = TRUE.
   hItem = hItem:NEXT-SIBLING.
END.

/* if "Prolint" was not already added, then add it now */
IF NOT added THEN DO:

   CREATE MENU-ITEM hItem
       ASSIGN SUBTYPE   = "RULE":U
              PARENT    = hMenu:HANDLE.         

   CREATE MENU-ITEM hItem
       ASSIGN SUBTYPE   = "NORMAL":U
              LABEL     = "Prolint selected object"
              NAME      = "m_Prolint_object":U
              PARENT    = hMenu:HANDLE
              SENSITIVE = TRUE
       TRIGGERS:
         ON CHOOSE PERSISTENT RUN prolint/roundtable/101b/lintobj.p.
       END TRIGGERS.            

   CREATE MENU-ITEM hItem
       ASSIGN SUBTYPE   = "NORMAL":U
              LABEL     = "Prolint selected task"
              NAME      = "m_Prolint_task":U
              PARENT    = hMenu:HANDLE
              SENSITIVE = TRUE
       TRIGGERS:
         ON CHOOSE PERSISTENT RUN prolint/roundtable/101b/linttask.p.
       END TRIGGERS.            

   CREATE MENU-ITEM hItem
       ASSIGN SUBTYPE   = "NORMAL":U
              LABEL     = "Configure Prolint"
              NAME      = "m_Prolint_cfg":U
              PARENT    = hMenu:HANDLE
              SENSITIVE = TRUE
       TRIGGERS:
         ON CHOOSE PERSISTENT RUN prolint/core/lintcfg.w ("roundtable run":U).
       END TRIGGERS.            

   
   /* also add a button in frame FR-control, next to button BT-visual-diff */ 
   hFrame = hWindow:FIRST-CHILD.
/*   DO WHILE hFrame<>? AND hFrame:NAME<>"FR-control":U :*/
   DO WHILE hFrame<>? AND hFrame:NAME<>"Button-Frame":U :   
      hFrame = hFrame:NEXT-SIBLING.
   END.                            

   /* make some room, move existing buttons to the right */   
   hItem = hFrame:FIRST-CHILD.
   hItem = hItem:FIRST-CHILD.
   DO WHILE hItem<>? :
/*      IF hItem:NAME="rect-6":U THEN*/
/*         hItem:X = hItem:X + 24.   */
/*      IF hItem:NAME="BT-VisDiff":U THEN*/
/*         hItem:X = hItem:X + 24.       */
/*      IF hItem:NAME="BT-run":U THEN    */
/*         hItem:X = hItem:X + 24.       */
/*      IF hItem:NAME="BT-CompX":U THEN  */
/*         hItem:X = hItem:X + 24.       */
         
      IF hItem:NAME="BT-Filter":U THEN 
         ASSIGN
            iLintX = hItem:X + 24
            hCompItem = hItem.
/*            hItem:X = hItem:X + 24.*/
         
/*      IF hItem:NAME="BT-Comp":U THEN ASSIGN*/
/*         iLintX = hItem:X                  */
/*         hCompItem = hItem                 */
/*         hItem:X = hItem:X + 24.           */
      hItem = hItem:NEXT-SIBLING.
   END.
     
   CREATE BUTTON hButton 
       ASSIGN FRAME         = hFrame 
              NAME          = "BT-prolint":U
              TOOLTIP       = "Prolint Object"
              X             = iLintX /* 179 */
              Y             = hCompItem:Y /* 4 */
              WIDTH-PIXELS  = hCompItem:WIDTH-PIXELS /* 24 */
              HEIGHT-PIXELS = hCompItem:HEIGHT-PIXELS /* 22*/
              FLAT-BUTTON   = TRUE
              SENSITIVE     = TRUE  
              VISIBLE       = TRUE
       TRIGGERS:    
          ON CHOOSE PERSISTENT RUN prolint/roundtable/101b/lintobj.p.
       END TRIGGERS.
   hButton:LOAD-IMAGE("prolint/roundtable/101b/bt-prolint.bmp":U).
   hButton:LABEL = "Lint".
   
END.

