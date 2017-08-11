/* -------------------------------------------------------------
   file    : lintproglist.p
   by      : hacked together by John Green
             from Jurjen Dijkstra's linttask.p
   purpose : Lint all files listed a text file.
   input   : The name of the text file containing the
             list of files to lint.
   input   : The name of the profile to use.
   ------------------------------------------------------------- */

DEFINE INPUT PARAMETER listFile AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER profileName AS CHARACTER NO-UNDO.
   
DEFINE TEMP-TABLE tt_sourcefiles NO-UNDO RCODE-INFORMATION
   FIELD SourceFile   AS CHARACTER  FORMAT "x(64)":U
   INDEX idx_id       AS PRIMARY UNIQUE SourceFile.

DEFINE VARIABLE filemasks AS CHARACTER NO-UNDO.
DEFINE VARIABLE propsrunning AS LOGICAL NO-UNDO INITIAL FALSE.
PUBLISH "IsProlintPropertiesRunning":U (OUTPUT propsrunning).
IF NOT propsrunning THEN
   RUN prolint/core/propsuper.p PERSISTENT.
RUN IncrementProlintPropertySubscribers.

filemasks = DYNAMIC-FUNCTION ("ProlintProperty", "compilationunit.filename.mask").

RUN DecrementProlintPropertySubscribers.

RUN addAllFiles.
RUN prolint/core/prolint.p ("",
                       THIS-PROCEDURE:HANDLE,
                       profileName,
                       TRUE).    


PROCEDURE addAllFiles :
/*------------------------------------------------------------------------------
  Reads the contents of the file named by the listFile input parameter.
  Adds a new entry to the temp table for each valid filename read.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currFilename AS CHARACTER NO-UNDO.
  INPUT FROM VALUE(listFile).
  REPEAT:
    IMPORT UNFORMATTED currFilename.
    IF currFilename > "" THEN DO:
      RUN addFile (INPUT currFilename).
    END.
  END.
  INPUT CLOSE.
END PROCEDURE. /* addAllFiles */


PROCEDURE GetFirstLintSource :
  /* purpose: prolint.p calls this ip to ask for the first sourcefile to analyze.
              return ? if you don't have any sourcefiles. */
  DEFINE OUTPUT PARAMETER pSourceFile AS CHARACTER NO-UNDO.
  
  FIND FIRST tt_sourcefiles NO-ERROR.
  IF AVAILABLE tt_sourcefiles THEN 
     pSourceFile = tt_sourcefiles.SourceFile.
  ELSE 
     pSourceFile = ?.
  
END PROCEDURE.


PROCEDURE GetNextLintSource :
  /* purpose: prolint.p calls this ip to ask for the next sourcefile to analyze.
              return ? if you don't have any sourcefiles. */
  DEFINE OUTPUT PARAMETER pSourceFile AS CHARACTER NO-UNDO.
  
  FIND NEXT tt_sourcefiles NO-ERROR.
  IF AVAILABLE tt_sourcefiles THEN 
     pSourceFile = tt_sourcefiles.SourceFile.
  ELSE 
     pSourceFile = ?.
  
END PROCEDURE.
           
                          

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
         
             
PROCEDURE AddFile :
/*------------------------------------------------------------------------------
  Purpose:     add file to tt_sourcefiles if it doesn't already exist
  Parameters:  newfile = filename
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER newfile AS CHAR NO-UNDO.

  newfile = RelativeFileName(newfile).
  IF NOT CAN-DO(filemasks,newfile) THEN
     RETURN.

  IF NOT CAN-FIND(tt_sourcefiles WHERE tt_sourcefiles.sourcefile=newfile) THEN DO:
     CREATE tt_sourcefiles.
     ASSIGN tt_sourcefiles.sourcefile = newfile.
  END.

END PROCEDURE.


