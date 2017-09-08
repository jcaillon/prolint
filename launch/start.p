/* -------------------------------------------------------------
file    : prolint/launch/start.p
by      : Jurjen Dijkstra
purpose : Select some files to lint and launch prolint.p.
------------------------------------------------------------- */
{prolint/core/dlc-version.i}

DEFINE VARIABLE modalresult AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE profile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ClearOutput AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE tt_sourcefiles NO-UNDO
    FIELD SourceFile   AS CHARACTER
    INDEX idx_id       AS PRIMARY UNIQUE SourceFile.

RUN prolint/core/selectfiles.w (OUTPUT modalresult, /* 1=ok, else is cancel */
    OUTPUT profile,
    OUTPUT TABLE tt_sourcefiles,
    OUTPUT ClearOutput).

IF modalresult=1 THEN
    RUN prolint/core/prolint.p ("",
        THIS-PROCEDURE:HANDLE,
        profile,
        ClearOutput).

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

