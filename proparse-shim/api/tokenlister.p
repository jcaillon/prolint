/* proparse/api/tokenlister.p
 * 2001 by John Green, Joanju Limited
 *
 * Walks through the syntax tree, and for each token (ie node) in the tree,
 * displays the token's type as well as the token's text.
 *
 * usage
 * =====
 * The default tokenlister settings will be used, results displayed in the resultswindow.
 * RUN proparse/api/tokenlister.p PERSISTENT SET tokenlister.
 * RUN setParseFile IN tokenlister ("filename.p").
 * RUN main IN tokenlister.
 * APPLY "CLOSE":U TO tokenlister.
 *
 *
 * Other methods
 * =============
 *
 * To write the output to "output.txt" instead of showing the output in resultswindow:
 * RUN setOutputFile IN tokenlister ("output.txt").
 *
 * To set a comma-delim list of node attributes to be displayed:
 * RUN setDispAttr IN tokenlister (yourList).
 * Ex: "linenum,filename,statehead,state2"
 * 
 */

DEFINE VARIABLE filename        AS CHARACTER NO-UNDO.
DEFINE VARIABLE outfile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE indentby        AS INTEGER   NO-UNDO INITIAL 4.
DEFINE VARIABLE indent          AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE firstnode       AS INTEGER   NO-UNDO.
DEFINE VARIABLE grandchild      AS INTEGER   NO-UNDO.
DEFINE VARIABLE showColumn      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE showDirectives  AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE showFile        AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE showLine        AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE showState2      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE showStatehead   AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE showStoretype   AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE showWindow      AS LOGICAL   NO-UNDO INITIAL TRUE.

DEFINE VARIABLE parser AS HANDLE NO-UNDO.
RUN proparse/api/proparse.p PERSISTENT SET parser.
{proparse/api/proparse.i parser}

ASSIGN outfile = SESSION:TEMP-DIRECTORY + "/joanju_tokenlister.txt":U.


ON "CLOSE":U OF THIS-PROCEDURE DO:
  APPLY "CLOSE":U TO parser.
  DELETE PROCEDURE THIS-PROCEDURE.
END.


RETURN.


PROCEDURE displayNode:
  DEFINE INPUT PARAMETER theNode AS INTEGER NO-UNDO.

  DEFINE VARIABLE nodeType AS CHARACTER NO-UNDO.
  DEFINE VARIABLE child    AS INTEGER   NO-UNDO.
  ASSIGN child = parserGetHandle().

  RUN printline (theNode, indent).

  ASSIGN nodeType = parserNodeFirstChild(theNode,child).
  ASSIGN indent = indent + indentby.
  DO WHILE nodeType <> "":
    /* If this is a new node head, run displayNode with it */
    IF parserNodeFirstChild(child, grandchild) <> "" THEN DO:
      RUN displayNode (child).
      ASSIGN nodeType = parserNodeNextSibling(child,child).
    END.
    ELSE DO:
      RUN printline (child, indent).
      ASSIGN nodeType = parserNodeNextSibling(child, child).
    END.
  END.
  ASSIGN indent = indent - indentby.
  parserReleaseHandle(child).
END PROCEDURE. /* displayNode */


PROCEDURE main:
  DEFINE VARIABLE wasShowParserDirectives AS CHARACTER NO-UNDO.
  ASSIGN wasShowParserDirectives = parserConfigGet("show-proparse-directives":U).
  IF showDirectives THEN
    parserConfigSet("show-proparse-directives":U, "true":U).
  ELSE
    parserConfigSet("show-proparse-directives":U, "false":U).
  OUTPUT TO VALUE(outfile).
  errorblock:
  DO ON ERROR UNDO, LEAVE:
    FILE-INFO:FILE-NAME = filename.
    IF parserParse(FILE-INFO:FULL-PATHNAME) = FALSE THEN DO:
      MESSAGE parserErrorGetText() VIEW-AS ALERT-BOX ERROR TITLE "parserParse".
      LEAVE errorblock.
    END.
    ASSIGN
      firstnode  = parserGetHandle()
      grandchild = parserGetHandle().
    parserNodeTop(firstnode).  /* gets us the "Program" node */
    RUN displayNode (firstnode).
  END. /* errorblock */
  OUTPUT CLOSE.
  parserConfigSet("show-proparse-directives":U, wasShowParserDirectives).
  IF showWindow THEN
    RUN prolint/proparse-shim/utilities/resultswindow.p (outfile).
END PROCEDURE. /* main */


PROCEDURE printline:
  DEFINE INPUT PARAMETER theNode AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER indent AS INTEGER NO-UNDO.
  PUT UNFORMATTED
    SKIP
    FILL(" ",indent)
    parserGetNodeType(theNode)
    "    "
    parserGetNodeText(theNode)
    .
  IF showDirectives THEN
    PUT UNFORMATTED  "    " parserAttrGet(theNode, "proparsedirective":U).
  IF showLine OR showColumn THEN
    PUT UNFORMATTED  "    " STRING(parserGetNodeLine(theNode)).
  IF showColumn THEN
    PUT UNFORMATTED  ":" STRING(parserGetNodeColumn(theNode)).
  IF showFile THEN
    PUT UNFORMATTED  "    " parserGetNodeFilename(theNode).
  IF showStoretype THEN
    PUT UNFORMATTED  "    " parserAttrGet(theNode,"storetype":U).
  IF showStatehead AND parserAttrGet(theNode, "statehead":U) <> "" THEN
    PUT UNFORMATTED  "    statehead".
  IF showState2 THEN
    PUT UNFORMATTED  "    " parserAttrGet(theNode, "state2":U).
END PROCEDURE. /* printline */


PROCEDURE setDispAttr:
  DEFINE INPUT PARAMETER attrList AS CHARACTER NO-UNDO.
  ASSIGN
    showColumn     = CAN-DO(attrList, "column":U)
    showDirectives = CAN-DO(attrList, "proparsedirective":U)
    showFile       = CAN-DO(attrList, "filename":U)
    showLine       = CAN-DO(attrList, "linenum":U)
    showState2     = CAN-DO(attrList, "state2":U)
    showStatehead  = CAN-DO(attrList, "statehead":U)
    showStoretype  = CAN-DO(attrList, "storetype":U).
END PROCEDURE.  


PROCEDURE setOutputFile:
  DEFINE INPUT PARAMETER p AS CHARACTER NO-UNDO.
  ASSIGN
    outfile = p
    showWindow = FALSE.
END PROCEDURE.


PROCEDURE setParseFile:
  DEFINE INPUT PARAMETER p AS CHARACTER NO-UNDO.
  ASSIGN filename = p.
END PROCEDURE.
