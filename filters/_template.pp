/* ------------------------------------------------------------------
    file    : prolint/filters/_template.pp
    purpose : filter template
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

ON "CLOSE":U OF THIS-PROCEDURE DO:
   DELETE PROCEDURE THIS-PROCEDURE.
END.  

RETURN.


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


   /* This procedure is called before a warning is published to the outputhandlers.
      You can suppress the warning by setting filtered=TRUE.
      Or you can modify the Description and/or the severity. */

   /* a simple example would be:
      if (pRuleID = "varusage") and (pDescription matches "*parameter*") then
         filtered=TRUE
   */

END PROCEDURE.



