/** Some unit tests for Proparse.Net API.
 */

USING Progress.Lang.AppError.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

DEF VAR hproparse AS HANDLE NO-UNDO.
{prolint/proparse-shim/api/proparse.i hproparse}
RUN prolint/proparse-shim/api/proparse.p PERSISTENT SET hproparse.


DEF VAR numResults AS INT NO-UNDO.
DEF VAR nodeType AS CHAR NO-UNDO.


RUN assertString(propath, parserConfigGet("propath")).

DEF VAR proparseSchema AS CLASS org.prorefactor.core.schema.Schema.
proparseSchema = org.prorefactor.core.schema.Schema:getInstance().
RUN assertString("", parserErrorGetText()).

parserParse("invalid-file-name.p").
RUN assertInt(-2, parserErrorGetStatus()).
RUN assertContains("invalid-file-name.p", parserErrorGetText()).

parserErrorClear().
parserParse("prolint/proparse-shim/unit-test.p").
RUN assertInt(0, parserErrorGetStatus()).

RUN assertContains("unit-test.p", parserGetIndexFilename(1)).

DEF VAR nh1 AS INT NO-UNDO.
DEF VAR nh2 AS INT NO-UNDO.
DEF VAR nh3 AS INT NO-UNDO.
DEF VAR nh4 AS INT NO-UNDO.
RUN getNewHandles.

parserNodeTop(nh1).
parserNodeFirstChild(nh1, nh2).
parserNodeFirstChild(nh1, nh3).

IF parserIsSameNode(nh1, nh2) THEN
  UNDO, THROW NEW AppError("Expected false parserIsSameNode", 0).
IF NOT parserIsSameNode(nh2, nh3) THEN
  UNDO, THROW NEW AppError("Expected true parserIsSameNode", 0).

RUN checkErrors.
numResults = parserQueryCreate(nh1, "test1", "DEFINE").
RUN checkErrors.
IF numResults LT 10 THEN
  UNDO, THROW NEW AppError("Expected more query results.", 0).
parserQueryGetResult("test1", 10, nh1).
RUN checkErrors.
RUN assertString("DEFINE", parserGetNodeType(nh1)).


parserErrorClear().
parserParse("prolint/proparse-shim/testdata/test3.cls").
RUN checkErrors.
RUN getNewHandles.
parserNodeTop(nh1).
numResults = parserQueryCreate(nh1, "class", "CLASS").
IF numResults NE 2 THEN
  UNDO, THROW NEW AppError("Expected two CLASS nodes", 0).
parserQueryGetResult("class", 1, nh2).
nh3 = parserAttrGetI(nh2, 2100). /* super class class node */
RUN assertContains("test2.cls", parserGetNodeFilename(nh3)).
RUN assertString("class", parserGetNodeText(nh3)).
nodetype = parserNodeFirstChild(nh3, nh4).
DO WHILE nodetype NE "Code_block" AND nodetype NE "":
  nodetype = parserNodeNextSibling(nh4, nh4).
END.
parserNodeFirstChild(nh4, nh4).
RUN assertString("VARIABLE", parserAttrGet(nh4, "state2")).




MESSAGE "Done unit tests." VIEW-AS ALERT-BOX.
RETURN.




CATCH e AS Progress.Lang.Error :
  MESSAGE e:GetMessage(1) VIEW-AS ALERT-BOX TITLE "unit-test.p exception message".
  MESSAGE parserErrorGetStackTrace() VIEW-AS ALERT-BOX TITLE "unit-test.p Stack Trace".
END CATCH.


PROCEDURE assertContains:
  DEF INPUT PARAM expect AS CHAR.
  DEF INPUT PARAM got AS CHAR.
  IF GOT EQ ? OR INDEX(got, expect) EQ 0 THEN
    UNDO, THROW NEW AppError("assert failed: ~r~nExpect: " + STRING(expect) + "~r~nGot: " + STRING(got), 0).
END PROCEDURE.

PROCEDURE assertInt:
  DEF INPUT PARAM expect AS INT.
  DEF INPUT PARAM got AS INT.
  IF got NE expect THEN
    UNDO, THROW NEW AppError("assert failed: ~r~nExpect: " + STRING(expect) + "~r~nGot: " + STRING(got), 0).
END PROCEDURE.

PROCEDURE assertString:
  DEF INPUT PARAM expect AS CHAR.
  DEF INPUT PARAM got AS CHAR.
  IF got NE expect THEN
    UNDO, THROW NEW AppError("assert failed: ~r~nExpect: " + STRING(expect) + "~r~nGot: " + STRING(got), 0).
END PROCEDURE.

PROCEDURE checkErrors:
  IF parserErrorGetStatus() NE 0 THEN
    UNDO, THROW NEW AppError(parserErrorGetText(), 0).
END PROCEDURE.

PROCEDURE getNewHandles:
  nh1 = parserGetHandle().
  nh2 = parserGetHandle().
  nh3 = parserGetHandle().
  nh4 = parserGetHandle().
END PROCEDURE.
