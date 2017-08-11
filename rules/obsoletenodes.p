/* ------------------------------------------------------------------------
   file    :  rules/obsolete.p
   by      :  Igor Natanzon
   purpose :  Identify obsolete nodes.
    -----------------------------------------------------------------
    Copyright (C) 2001,2002 Igor Natanzon

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

   ------------------------------------------------------------------------ */

{prolint/core/ruleparams.i}

DEFINE TEMP-TABLE tt-Obsolete NO-UNDO
       FIELD obsoleteNode   AS CHAR
       FIELD severityLevel  AS INTEGER
       FIELD warningMessage AS CHAR
       INDEX obsoleteNode   IS PRIMARY obsoleteNode.

IF VALID-HANDLE(hpRulePersist) THEN
   RUN GetList IN hpRulePersist (OUTPUT TABLE tt-Obsolete).

FOR EACH tt-Obsolete:
    RUN searchNode            (hTopnode, "InspectNode":U, tt-Obsolete.obsoleteNode).
END.

RETURN.


PROCEDURE InspectNode :

  DEFINE INPUT  PARAMETER theNode        AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER AbortSearch    AS LOGICAL NO-UNDO INITIAL NO.
  DEFINE OUTPUT PARAMETER SearchChildren AS LOGICAL NO-UNDO INITIAL YES.

  RUN PublishResultSeverity           
                               (compilationunit,
                                parserGetNodeFilename(theNode),
                                parserGetNodeLine(theNode),
                                SUBSTITUTE(tt-Obsolete.warningMessage, CAPS(parserGetNodeText(theNode))),
                                rule_id,
                                tt-Obsolete.severityLevel).
END PROCEDURE.
