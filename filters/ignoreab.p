/* ------------------------------------------------------------------
    file    : prolint/filters/ignoreab.p
    purpose : ignore warnings from AB-generated code sections
    -----------------------------------------------------------------

    Copyright (C) 2001-2009 Jurjen Dijkstra

    This file is part of Prolint.

    Prolint is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    Prolint is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Prolint; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   ------------------------------------------------------------------ */

{prolint/filters/filterparams.i}

DEFINE VARIABLE IgnoreAppbuilderstuff AS LOGICAL NO-UNDO.

IgnoreAppbuilderstuff = LOGICAL ( DYNAMIC-FUNCTION ("ProlintProperty", "filters.IgnoreAppbuilderstuff")).

ON "CLOSE":U OF THIS-PROCEDURE DO:
  DELETE PROCEDURE THIS-PROCEDURE.
END.

  /* tt_codesection helps suppressing warnings from UIB/AppBuilder-generated code */
  DEFINE TEMP-TABLE tt_codesection NO-UNDO
     FIELD sourcefile AS CHARACTER
     FIELD firstline  AS INTEGER
     FIELD lastline   AS INTEGER
     INDEX idx AS PRIMARY sourcefile firstline.

  SUBSCRIBE TO "Prolint_Status_FileEnd" ANYWHERE.

/* ------------------------------------------------------------------------------------
                       maintenance procedures for tt_codesection

   Temp-table tt_codesection contains a record for each code-section.
   The negative approach, one record for each UIB/AB-generated section, may seem more
   natural but I have decided to search for codesections for the following reasons:
     - when a sourcefile is created without section-editor it won't have an AbSection
       but it still has a codesection (only one, as large as the sourcefile) so it is
       easier to remember that we've already scanned this sourcefile
     - when using the section-editor, it will be interesting to log prolint warnings
       using the name of the section (and relative linenumber) <-- TODO
   ------------------------------------------------------------------------------------ */

FUNCTION IsAbGenerated RETURNS LOGICAL ( pSourceFile AS CHARACTER, pLineNumber AS INTEGER ) :
    /* purpose : return TRUE if this line is in UIB/AppBuilder-generated code block.
                 in other words, if you can NOT find a matching tt_codesection record  */

    IF pLineNumber=0 THEN
        RETURN FALSE. /* pLineNumber may be 0 when you are inspecting a synthetic node,
                        FALSE just appears to make a little bit more sense than TRUE  */

    IF NOT IgnoreAppbuilderstuff THEN
       RETURN FALSE.
    ELSE DO:

        /* did we already scan this sourcefile for &ANALYZE_SUSPEND directives? */
        IF NOT CAN-FIND(FIRST tt_codesection WHERE tt_codesection.sourcefile = pSourceFile) THEN
           RUN AbFindSections (pSourceFile).

        /* if this line is in a codesection, then it is not AB-generated */
        FIND FIRST tt_codesection WHERE tt_codesection.sourcefile  = pSourceFile
                                    AND tt_codesection.firstline  <= pLineNumber
                                    AND tt_codesection.lastline   >= pLineNumber
                                  NO-ERROR.
        RETURN NOT (AVAILABLE tt_codesection).

    END.

END FUNCTION.



PROCEDURE  AbFindSections :
    /* purpose : read a sourcefile, see where programmer-defined code begins/ends */

    DEFINE INPUT PARAMETER pSourcefile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vAbGenerated AS LOGICAL   NO-UNDO INITIAL FALSE.
    DEFINE VARIABLE vLineNumber  AS INTEGER   NO-UNDO INITIAL 0.
    DEFINE VARIABLE vBlockType   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vInsideBlock AS LOGICAL   NO-UNDO INITIAL FALSE.
    DEFINE VARIABLE rw           AS ROWID     NO-UNDO.

    FILE-INFORMATION:FILE-NAME = pSourcefile.
    IF FILE-INFORMATION:FULL-PATHNAME = ? THEN
        RETURN.

    INPUT FROM VALUE( FILE-INFORMATION:FULL-PATHNAME ).

    loop_importlines:
    REPEAT:

        IMPORT UNFORMATTED vLine.
        vLineNumber = vLineNumber + 1.

        IF vLine MATCHES "&ANALYZE-SUSPEND*":U THEN
        DO:
            IF vLineNumber EQ 1 THEN
                vAbGenerated  = TRUE.

            if vLine matches "*_CONTROL-LOAD"
            or vLine matches "*_DEFAULT-DISABLE"
            or vLine matches "*_DEFAULT-ENABLE" then
              next.

            vBlockType = ENTRY( 1, SUBSTRING( vLine, 18 ), " ":U ).
            if vBlockType EQ "_UIB-CODE-BLOCK":U       /* procedures, user defined functions */
            OR vBlockType EQ "_QUERY-BLOCK":U          /* free-form queries */
            OR vBlockType EQ "_RUN-TIME-ATTRIBUTES":U  /* specifies private-data */
            then
            DO:
             /* TODO: store vBlockType and use it in outputhandlers (report section name and relative linenumber) */
                CREATE tt_codesection.
                ASSIGN
                    tt_codesection.sourcefile = pSourcefile
                    tt_codesection.firstline  = vLineNumber
                    vInsideBlock              = TRUE.
                rw = ROWID( tt_codesection ).
            END.
        END.
        ELSE
            IF vInsideBlock AND ( vLine MATCHES "&ANALYZE-RESUME*":U ) THEN
            DO:
                FIND tt_codesection WHERE
                        ROWID( tt_codesection )=rw.
                ASSIGN
                    tt_codesection.lastline = vLineNumber
                    vInsideBlock            = FALSE.
            END.

        IF NOT vAbGenerated THEN
            LEAVE loop_importlines.
    END.
    INPUT CLOSE.

    IF NOT vAbGenerated THEN
    DO:
       /* if sourcefile is not UIB/AB-generated, then entire file is one big codesection */
        CREATE tt_codesection.
        ASSIGN
            tt_codesection.sourcefile = pSourcefile
            tt_codesection.firstline  = 0
            tt_codesection.lastline   = 99999999. /* your sourcefiles aren't that big, i hope */
    END.

END PROCEDURE.



PROCEDURE Prolint_Status_FileEnd :
    /* purpose : delete all tt_codesection when prolint is done with the compilation-unit */
    FOR EACH tt_codesection :
        DELETE tt_codesection.
    END.
END PROCEDURE.



PROCEDURE GetFilterResult :
/* purpose:
   this procedure is called from prolint/core/filterplugins.p just before the
   warning is published to the outputhandler(s). You can modify the description
   or the severity. You can set filtered=true to hide the warning */
    DEFINE INPUT        PARAMETER pCompilationUnit AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER pFullSource      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER pRelativeSource  AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER pLineNumber      AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER pRuleID          AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER pIgnoreAB        AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER pDescription     AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER pSeverity        AS INTEGER   NO-UNDO.
    DEFINE OUTPUT       PARAMETER filtered         AS LOGICAL   NO-UNDO.

    IF pIgnoreAB = FALSE THEN
        RETURN.

    IF IgnoreAppbuilderstuff THEN
       filtered = IsAbGenerated ( pFullSource, pLineNumber ).
    ELSE
       filtered = FALSE.

END PROCEDURE.



