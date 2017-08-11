/* wspace-culist.p
 * March 2003 by John Green
 * Create a list of all compile units in a workspace.
 *
 * To Do
 * =====
 * - creation of temporary directory structure could
 *   be more efficient.
 * - Does not make use of any object compile parameters
 *   that are available in Roundtable.
 */

DEFINE NEW GLOBAL SHARED VARIABLE vhRtbCustFunc AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE viRtbCustFuncId AS INTEGER NO-UNDO.

{prolint/roundtable/101b/rt_customfunc.i}

DEFINE VARIABLE fullPathName              AS CHARACTER NO-UNDO.
DEFINE VARIABLE listName                  AS CHARACTER NO-UNDO INITIAL "joanjuproglist.txt":U.
DEFINE VARIABLE parsePathName             AS CHARACTER NO-UNDO.
DEFINE VARIABLE partPathName              AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempCount                 AS INTEGER NO-UNDO.
DEFINE VARIABLE wodir                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE webUtilObjType            AS CHARACTER NO-UNDO.
DEFINE VARIABLE webUtilObjName            AS CHARACTER NO-UNDO.

DEFINE VARIABLE gcRtbCurrentWorkspace     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcRtbCurrentWorkspacePath AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcRtbCurrentWorkspaceRoot AS CHARACTER   NO-UNDO.

wodir = SESSION:TEMP-DIR + "/joanjutmpwo":U.

OS-DELETE VALUE(wodir) RECURSIVE.
OS-CREATE-DIR VALUE(wodir).

OUTPUT TO VALUE(listName).

PUBLISH "evRtbGetCurrentWorkspace":U (OUTPUT gcRtbCurrentWorkspace).
ASSIGN
   gcRtbCurrentWorkspacePath = DYNAMIC-FUNCTION('fnRtbGetWorkspacePath':U, gcRtbCurrentWorkspace)    
   gcRtbCurrentWorkspaceRoot = DYNAMIC-FUNCTION('fnRtbGetWorkspaceRoot':U,gcRtbCurrentWorkspace).

FOR EACH rtb.rtb_object
  WHERE rtb.rtb_object.wspace-id = gcRtbCurrentWorkspace
  AND   rtb.rtb_object.obj-type = "PCODE":U
  NO-LOCK,
  rtb.rtb_ver OF rtb.rtb_object
  NO-LOCK:

  IF NOT rtb.rtb_ver.compiles THEN
    NEXT.
  
  ASSIGN 
    partPathName =
      ENTRY(
        1,
        DYNAMIC-FUNCTION(
          "fnRtbGetSourceName":U,
          ROWID(rtb.rtb_object),
          ""
        ) )
    fullPathName = gcRtbCurrentWorkspaceRoot + "/" + partPathName
    parsePathName = fullPathName
    webUtilObjType = ""
    webUtilObjName = ""
    .

  ASSIGN FILE-INFO:FILE-NAME = fullPathName.
  IF fullPathName MATCHES("*~~.htm*":U) THEN DO ON ERROR UNDO, LEAVE:
    DO tempCount = 1 TO NUM-ENTRIES(partPathName, "/":U) - 1:
      OS-CREATE-DIR VALUE(wodir + "/":U + ENTRY(tempCount, partPathName, "/":U)).
    END.
    OS-COPY VALUE(fullPathName) VALUE(wodir + "/":U + partPathName).
    RUN webutil/e4gl-gen.p
      (INPUT        wodir + "/":U + partPathName,
       INPUT-OUTPUT webUtilObjType,
       INPUT-OUTPUT webUtilObjName).
    ASSIGN parsePathName = webUtilObjName.
  END.
  IF RETURN-VALUE <> "" THEN
    MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX TITLE "e4gl-gen.p " + FILE-INFO:FULL-PATHNAME.
  RUN clearReturnValue.

  PUT UNFORMATTED parsePathName SKIP.

END.

OUTPUT CLOSE.

PROCEDURE clearReturnValue:
  RETURN "".
END.


