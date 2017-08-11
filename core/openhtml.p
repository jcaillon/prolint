/* =======================================================================
   file    : prolint/core/openhtml.p
   purpose : open a HTML file in viewer
             platform-independent
   ======================================================================= */

DEFINE INPUT PARAMETER htmlfile AS CHARACTER NO-UNDO.

 DEFINE VARIABLE parentHwnd AS INTEGER NO-UNDO.
 DEFINE VARIABLE cHelpfile  AS CHARACTER NO-UNDO.
 DEFINE VARIABLE cTopic     AS CHARACTER NO-UNDO.

 IF OPSYS="WIN32":U THEN DO:
 /*   IF SESSION:DISPLAY-TYPE = "GUI":U THEN DO:
       /* Windows GUI: try compiled HTML-help. Only because it's cool. */
       FILE-INFO:FILE-NAME = "prolint/prolint.chm".
       chelpfile = FILE-INFO:FULL-PATHNAME.
       cTopic    = SUBSTRING(htmlfile, INDEX(chelpfile, "prolint.chm")).
       SYSTEM-HELP chelpfile HELP-TOPIC ( cTopic ).
    END.
    ELSE */ DO:
       /* Windows ChUI: open uncompiled HTML in default internet-browser.
          Avoid OS-COMMAND because it does not seem to work very well. */
       IF FOCUS<>? THEN
          parentHwnd = FOCUS:HWND.
       ELSE
          parentHwnd = 0.
       RUN ShellExecuteA (parentHwnd,
                          "open":U,
                          htmlfile,
                          "",
                          "",
                          1).
    END.
 END.
 ELSE
    /* Linux: open uncompiled HTML in default internet-browser */
    OS-COMMAND NO-WAIT START VALUE(htmlfile).

RETURN.

PROCEDURE ShellExecuteA EXTERNAL "shell32.dll":U PERSISTENT:
   DEFINE INPUT PARAMETER HWND         AS LONG NO-UNDO.
   DEFINE INPUT PARAMETER lpOperation  AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER lpFile       AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER lpParameters AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER lpDirectory  AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER nShowCmd     AS LONG NO-UNDO.
END.
    

