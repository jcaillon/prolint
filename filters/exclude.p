/* ------------------------------------------------------------------
    file    : prolint/filters/exclude.p
    purpose : filter warnings based on definitions in exclude.lst
    -----------------------------------------------------------------

    Copyright (C) 2001-2003 Jurjen Dijkstra, Ildefonzo Arocha

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


  DEFINE TEMP-TABLE tt_exclude NO-UNDO
      FIELD Sourcefile AS CHARACTER
      FIELD RuleID     AS CHARACTER
      INDEX idx_1 AS PRIMARY SourceFile RuleID.

  ON "CLOSE":U OF THIS-PROCEDURE DO:
     DELETE PROCEDURE THIS-PROCEDURE.
  END.

  RUN FillExcludeList.

RETURN.


PROCEDURE FillExcludeList :
   /* purpose: read a list of warnings that you want to exclude.
               store them in a temp-table for fast retrieval.
               this appears to be the fastest way to do it */
    DEFINE VARIABLE ignorelist AS CHARACTER NO-UNDO.
    DEFINE VARIABLE oneline    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vname      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vrules     AS CHARACTER NO-UNDO.

   /* empty temp-table */
    FOR EACH tt_exclude :
        DELETE tt_exclude.
    END.

    IF NOT LOGICAL(DYNAMIC-FUNCTION("ProlintProperty", "filters.excludelist")) THEN
       RETURN.

   /* file format:

         sourcefile|RuleID1, RuleID2 .... RuleID3

      sourcefile can be a CAN-DO expression with wildcards.
      the rule list can also be a CAN-DO expression.
   */

    FILE-INFORMATION:FILE-NAME=ProfileDirectory + "/exclude.lst":U.
    ignorelist = FILE-INFORMATION:FULL-PATHNAME.
    IF ignorelist <> ? THEN
    DO:
        INPUT FROM VALUE( ignorelist ).
        loop_readline:
        REPEAT :
            IMPORT UNFORMATTED oneline.
            oneline=TRIM( oneline ).
            IF ( oneline='' ) OR ( oneline MATCHES "#*":U ) OR ( NUM-ENTRIES( oneline, "|":U )<>2 ) THEN
                NEXT loop_readline.

            ASSIGN
                vname  = ENTRY( 1, oneline, '|':U )
                vrules = ENTRY( 2, oneline, '|':U ).

            CREATE tt_exclude.
            ASSIGN
                tt_exclude.SourceFile = vname
                tt_exclude.RuleID     = vRules.
        END.
        INPUT CLOSE.
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

    filtered =  CAN-FIND( FIRST tt_exclude
        WHERE CAN-DO( tt_exclude.SourceFile, pRelativeSource )
        AND CAN-DO( tt_exclude.RuleID, pRuleID ) ).

END PROCEDURE.



