/* ====================================================================
   file    : prolint/core/add2protools.p
   by      : Jurjen Dijkstra
   purpose : Add Prolint Desktop to PRO*TOOLS palette
   ==================================================================== */

DEFINE INPUT   PARAMETER cAction          AS CHARACTER NO-UNDO.
DEFINE OUTPUT  PARAMETER lAlreadyInThere  AS LOGICAL   NO-UNDO INITIAL NO.

DEFINE VARIABLE ProtoolsDatfile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProgram         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMaxOrder        AS INTEGER   NO-UNDO.
DEFINE VARIABLE hproc            AS HANDLE    NO-UNDO.
DEFINE VARIABLE NeedsNewBitmap   AS LOGICAL   NO-UNDO INITIAL FALSE.

DEFINE TEMP-TABLE ttTools NO-UNDO
   FIELD caption AS CHARACTER
   FIELD prog    AS CHARACTER
   FIELD glyph   AS CHARACTER
   FIELD dunno1  AS LOGICAL
   FIELD dunno2  AS LOGICAL
   FIELD ordnum  AS INTEGER
   FIELD dunno3  AS LOGICAL.


GET-KEY-VALUE SECTION "ProTools"
    KEY "FunctionDefs" VALUE ProtoolsDatfile.
IF ProtoolsDatfile=? OR ProtoolsDatfile="" THEN DO:
   MESSAGE "Prolint cannot find the location of file 'protools.dat'" SKIP
           "You probably have to use the standard PRO*Tools Customization dialog first, to specify that you want to save the palette position."
           VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.

/* read protools.dat */
INPUT FROM VALUE(ProtoolsDatfile).
REPEAT:
    CREATE ttTools.
    IMPORT ttTools.
    IF ttTools.ordnum > iMaxOrder THEN
       iMaxOrder = ttTools.ordnum.
END.
INPUT CLOSE.
FOR EACH ttTools WHERE ttTools.prog="" :
    DELETE ttTools.
END.

FIND ttTools WHERE ttTools.prog = "prolint/desktop.w" NO-ERROR.
IF AVAILABLE ttTools THEN DO:
   lAlreadyInThere = TRUE.
   IF ttTools.glyph = "prolint/protools.bmp" THEN DO:
      ttTools.glyph = "prolint/images/protools.bmp".
      MESSAGE "The bitmap for the Prolint Desktop in PRO*Tools will now be repaired...." VIEW-AS ALERT-BOX.
      RUN RewriteProtools.
   END.
END.

/* see if it is possible to write to the file. It may not be possible in controlled environments like WTS/Citrix.
   Set the alreadyInThere flag so the "Add to Protools" button gets hidden. */
file-info:file-name = ProtoolsDatfile.
if lookup("W",file-info:file-type) = 0 then
   lAlreadyInThere = true.

/* rewrite the file unless the file is not writeable */ 
IF cAction = "ADD":U THEN DO:
    /* append to protools.dat, if still needed */
    IF NOT lAlreadyInThere THEN DO:
       CREATE ttTools.
       ASSIGN ttTools.caption = "Prolint Desktop"
              ttTools.prog = "prolint/desktop.w"
              ttTools.glyph = "prolint/images/protools.bmp"
              ttTools.dunno1 = yes
              ttTools.dunno2 = yes
              ttTools.ordnum = iMaxOrder + 10
              ttTools.dunno3 = NO.
       RUN RewriteProtools.
       lAlreadyInThere = TRUE.
    END.
END.

PROCEDURE RewriteProtools :
   OUTPUT TO VALUE(ProtoolsDatfile).
   FOR EACH ttTools :
      EXPORT ttTools.
   END.
   OUTPUT CLOSE.

   /* try to close protools palette */
   hproc = SESSION:FIRST-PROCEDURE.
   DO WHILE (VALID-HANDLE(hProc)) AND NOT (hProc:FILE-NAME MATCHES "*protools*_protool*") :
      hproc = hproc:NEXT-SIBLING.
   END.
   IF (VALID-HANDLE(hProc)) AND (hProc:FILE-NAME MATCHES "*protools*_protool*") THEN
      RUN Destroy IN hProc NO-ERROR.
   MESSAGE "Re-open the PRO*Tools palette to see the new Prolint Desktop option"
      VIEW-AS ALERT-BOX.
   
END PROCEDURE.


