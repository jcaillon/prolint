/* =======================================================================================
   file    : prolint/rules/persist/where-udf.p
   purpose : keep a persistent list of User Defined Functions that can
             safely be used in a WHERE-clause.
   by      : Jurjen Dijkstra
    -----------------------------------------------------------------

    Copyright (C) 2001,2002 Jurjen Dijkstra

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

   DEFINE TEMP-TABLE tmp_safe_udf NO-UNDO
      FIELD udfname AS CHARACTER
      INDEX idx_name AS PRIMARY udfname.

   RUN ImportAllWhitelists.

RETURN.


FUNCTION IsFunctionSafe RETURNS LOGICAL (INPUT functionname AS CHARACTER) :
   /* purpose : this function is called from rule "where-udf" to query if functionname is on the whitelist */
   RETURN CAN-FIND(tmp_safe_udf WHERE tmp_safe_udf.udfname=functionname).
END FUNCTION.


PROCEDURE ImportAllWhitelists :
   /* purpose : import the user-maintained lists of safe udf names.
                there may be two whitelists: read them both */
   DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.

   FILE-INFO:FILE-NAME = "local-prolint/settings/where-udf.lst":U.
   cFile = FILE-INFO:FULL-PATHNAME.
   IF cFile<>? THEN
      RUN ImportOneWhiteList (cFile).

   FILE-INFO:FILE-NAME = "prolint/settings/where-udf.lst":U.
   cFile = FILE-INFO:FULL-PATHNAME.
   IF cFile<>? THEN
      RUN ImportOneWhiteList (cFile).
      

END PROCEDURE.


PROCEDURE ImportOneWhiteList :
   /* purpose : import one whitelist */
   DEFINE INPUT PARAMETER cFile AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.

   INPUT FROM VALUE(cFile).
   REPEAT:
     IMPORT UNFORMATTED cLine.
     cLine = TRIM(cLine).
     IF NOT CAN-FIND(tmp_safe_udf WHERE tmp_safe_udf.udfname=cLine) THEN DO:
        CREATE tmp_safe_udf.
        ASSIGN tmp_safe_udf.udfname = cLine.
     END.
   END.
   INPUT CLOSE.
   
END PROCEDURE.


