/* ------------------------------------------------------------------
    file    : prolint/filters/nowarn.p
    purpose : filter warnings based on definitions in nowarn.lst
    -----------------------------------------------------------------

    Copyright (C) 2001-2003 Jurjen Dijkstra

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

DEFINE VARIABLE useThisFilter AS LOGICAL NO-UNDO.

useThisFilter = LOGICAL ( DYNAMIC-FUNCTION("ProlintProperty", "filters.nowarnlist")).

DEFINE TEMP-TABLE tt_ignore NO-UNDO
   FIELD Sourcefile AS CHARACTER
   FIELD RuleID     AS CHARACTER
   FIELD LineNumber AS INTEGER
   INDEX idx_1 AS PRIMARY SourceFile RuleID LineNumber.

ON "CLOSE":U OF THIS-PROCEDURE DO:
  DELETE PROCEDURE THIS-PROCEDURE.
END.
  
IF useThisFilter THEN
   RUN FillIgnoreList (ProfileDirectory).

RETURN.



PROCEDURE FillIgnoreList :
   /* purpose: read a list of warnings that you want to ignore.
               store them in a temp-table for fast retrieval.
               this appears to be the fastest way to do it */
    DEFINE INPUT PARAMETER ProfileDirectory AS CHARACTER NO-UNDO.

    DEFINE VARIABLE ignorelist AS CHARACTER NO-UNDO.
    DEFINE VARIABLE oneline    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vname   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vrule   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vlines  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE size_ok AS LOGICAL   NO-UNDO.

   /* empty temp-table */
    FOR EACH tt_ignore :
        DELETE tt_ignore.
    END.

    DEFINE BUFFER buf_ignore FOR tt_ignore.

   /* ugly format, reminds me of linux :-)

         sourcefile|RuleID|linenumber,linenumber,linenumber,......,linenumber
         sourcefile|_file-size|size

      if _file-size is not present, or size doesn't match, then ignore all
      lines for this sourcefile
   */

    FILE-INFORMATION:FILE-NAME=ProfileDirectory + "/nowarn.lst":U.
    ignorelist = FILE-INFORMATION:FULL-PATHNAME.
    IF ignorelist <> ? THEN
    DO:
        INPUT FROM VALUE( ignorelist ).
        loop_readline:
        REPEAT :
            IMPORT UNFORMATTED oneline.
            oneline=TRIM( oneline ).
            IF ( oneline='' ) OR ( oneline MATCHES "#*":U ) OR ( NUM-ENTRIES( oneline, "|":U )<>3 ) THEN
                NEXT loop_readline.

            ASSIGN
                vname = ENTRY( 1, oneline, '|':U )
                vrule = ENTRY( 2, oneline, '|':U )
                vlines= ENTRY( 3, oneline, '|':U ).

            DO i=1 TO NUM-ENTRIES( vlines ) :
                CREATE tt_ignore.
                ASSIGN 
                    tt_ignore.SourceFile = vname
                    tt_ignore.RuleID     = vrule
                    tt_ignore.LineNumber = INTEGER( ENTRY( i, vlines ) ).
            END.
        END.
        INPUT CLOSE.
    END.

   /* delete files from tt_ignore if their filesizes changed */
   /* purpose: stored line numbers are unreliable if the sourcefile is edited */
    vname = "".
    FOR EACH tt_ignore NO-LOCK:
        IF tt_ignore.SourceFile <> vname THEN
        DO:
            vname = tt_ignore.SourceFile.
            FIND buf_ignore WHERE
                    buf_ignore.SourceFile = vname AND buf_ignore.RuleID     = "_file-size":U NO-LOCK NO-ERROR.
            IF NOT AVAILABLE buf_ignore THEN
                size_ok=FALSE.
            ELSE
            DO:
                FILE-INFORMATION:FILE-NAME = vname.
                size_ok = FILE-INFORMATION:FILE-SIZE = buf_ignore.LineNumber.
            END.
            IF NOT size_ok THEN
               FOR EACH buf_ignore EXCLUSIVE-LOCK WHERE buf_ignore.SourceFile = vname :
                   DELETE buf_ignore.
               END.
        END.
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


   IF useThisFilter THEN
      filtered = CAN-FIND(tt_ignore WHERE tt_ignore.SourceFile = pRelativeSource
                                      AND tt_ignore.RuleID     = pRuleID
                                      AND tt_ignore.LineNumber = pLineNumber).
   ELSE
      filtered = FALSE.

END PROCEDURE.



