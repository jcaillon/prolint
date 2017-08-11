/* -------------------------------------------------------------
   file    : prolint/launch/lintFileList.p
   by      : Martijn Voncken 
           : took start.p by Jurjen Dijkstra and modified it for string-input
   purpose : Prolint pcFileList(,-erperated char) and launch prolint.p.
   ------------------------------------------------------------- */

DEFINE INPUT PARAMETER pcFileList AS CHAR NO-UNDO.

DEFINE VARIABLE profile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ClearOutput AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iFile       AS INTEGER   NO-UNDO.
DEFINE VARIABLE mruprofile  AS CHARACTER NO-UNDO.

IF pcFileList = "?":U OR pcFileList = "" OR pcFileList = ? THEN DO:
    MESSAGE "No files to prolint":T VIEW-AS ALERT-BOX.
    RETURN.
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
   IF mruprofile <> ?  THEN profile = mruprofile.
END.

iFile = 0. /*!*/                         

RUN prolint/core/prolint.p ("",
                       THIS-PROCEDURE:HANDLE,
                       profile,
                       ClearOutput).    

PROCEDURE GetFirstLintSource :
  /* purpose: prolint.p calls this ip to ask for the first sourcefile to analyze.
              return ? if you don't have any sourcefiles. */
  DEFINE OUTPUT PARAMETER pSourceFile AS CHARACTER NO-UNDO.
  
  /*next == first*/
  RUN GetNextLintSource(OUTPUT pSourceFile). 
    
END PROCEDURE.


PROCEDURE GetNextLintSource :
  /* purpose: prolint.p calls this ip to ask for the next sourcefile to analyze.
              return ? if you don't have any sourcefiles. */
  DEFINE OUTPUT PARAMETER pSourceFile AS CHARACTER NO-UNDO INITIAL ?.
  
  iFile = iFile + 1.
  IF NUM-ENTRIES(pcFileList) >= iFile THEN DO:
      pSourceFile = SEARCH(entry(iFile,pcFileList)).

      IF pSourceFile = ? THEN /*invalid entry..*/
         RUN GetNextLintSource(OUTPUT pSourceFile).
  END.
  ELSE pSourceFile = ?.

END PROCEDURE.
           
           
