/* prolint/proparse-shim/utilities/resultswindow.p
 *
 * 2001 by John Green, Joanju Limited
 *
 * Simple window for displaying a text file read-only.
 * Useful for displaying reports, log files, etc.
 *
 */

DEFINE INPUT PARAMETER filename AS CHARACTER NO-UNDO.

DEFINE VARIABLE ed1          AS WIDGET-HANDLE NO-UNDO.

FORM WITH FRAME f1 VIEW-AS DIALOG-BOX SIZE 122 BY 27 NO-LABELS THREE-D.

CREATE EDITOR ed1
  ASSIGN
    READ-ONLY = TRUE
    FRAME = FRAME f1:HANDLE
    SCROLLBAR-VERTICAL = TRUE
    SCROLLBAR-HORIZONTAL = TRUE
    LARGE = TRUE
    WIDTH = 120
    HEIGHT = 25.
DISPLAY WITH FRAME f1.

ed1:READ-FILE(filename).

ENABLE ALL WITH FRAME f1.
DO ON ERROR UNDO, LEAVE ON END-KEY UNDO, LEAVE:
  WAIT-FOR WINDOW-CLOSE OF FRAME f1.
END.

RETURN.
