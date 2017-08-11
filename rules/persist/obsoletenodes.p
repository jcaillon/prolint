/* =======================================================================================
   file    : prolint/rules/persist/obsoletenodes.p
   purpose : keep a persistent list of obsolete keywords
   by      : Jurjen Dijkstra
    -----------------------------------------------------------------

    Copyright (C) 2002 Jurjen Dijkstra

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
   ======================================================================================= */

DEFINE INPUT PARAMETER hLintSuper AS HANDLE NO-UNDO.

DEFINE STREAM s-in.

DEFINE TEMP-TABLE tt-Obsolete NO-UNDO
       FIELD obsoleteNode   AS CHAR
       FIELD severityLevel  AS INTEGER
       FIELD warningMessage AS CHAR
       INDEX obsoleteNode   IS PRIMARY obsoleteNode.

   RUN ImportList.

   /* raise warning when obsolete.d not found, only once for the Prolint session: */
   IF NOT CAN-FIND(FIRST tt-Obsolete) THEN
      RUN PublishResultSeverity IN hLintSuper ("",0,0,"Obsolete nodes list not found.","prolint", 8).

RETURN.


PROCEDURE ImportList :
   /* purpose : import obsolete.d */

   ASSIGN FILE-INFO:FILE-NAME = "prolint/custom/rules/persist/obsoletenodes.d":U.
   IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
      ASSIGN FILE-INFO:FILE-NAME = "prolint/rules/persist/obsoletenodes.d":U.
      IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN "error":U.
   END.
   INPUT STREAM s-in FROM VALUE(FILE-INFO:FULL-PATHNAME).
   REPEAT:
      CREATE tt-Obsolete.
      IMPORT STREAM s-in tt-Obsolete.
   END.
   INPUT STREAM s-in CLOSE.
   FOR EACH tt-obsolete WHERE tt-Obsolete.obsoleteNode = "":
       DELETE tt-obsolete.
   END.

END PROCEDURE.


PROCEDURE GetList :
   /* purpose: copy the temp-table to rules/obsolete.p */
   DEFINE OUTPUT PARAMETER TABLE FOR tt-Obsolete.
END PROCEDURE.


