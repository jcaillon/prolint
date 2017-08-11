/* =======================================================================================
    file    : prolint/prolintdb/runffq.p
    purpose : run a freeform query
              the WHERE-clause is in a temporary includefile,
              this includefile is created by dialog freeform.w
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


 {prolint/core/dlc-version.i}

  DEFINE VARIABLE LogwinRunning AS LOGICAL NO-UNDO.
  DEFINE VARIABLE hw            AS HANDLE  NO-UNDO.

  /* open logwin if it isn't already opened */

  LogwinRunning = FALSE.
  hw = SESSION:FIRST-CHILD.
  DO WHILE VALID-HANDLE(hw) :
     IF hw:PRIVATE-DATA = "prolint_outputhandler_logwin.w":U THEN
        LogwinRunning = TRUE.
     hw = hw:NEXT-SIBLING.
  END.
  IF NOT LogwinRunning THEN 
     RUN prolint/outputhandlers/logwin.w PERSISTENT.
  
  /* send results to the logwin: simply publish warnings as if you are live linting */

  PUBLISH "Prolint_Status_Profile" ("prolintdb").
  PUBLISH "Prolint_InitializeResults" (TRUE).


FOR EACH lint_warning NO-LOCK WHERE
    {prolintffq.tmp.i}  :

    PUBLISH "Prolint_AddResult":U (prolintdb.lint_warning.compunit,
                                   prolintdb.lint_warning.sourcefile,
                                   prolintdb.lint_warning.linenumber,
                                   prolintdb.lint_warning.comment,
                                   prolintdb.lint_warning.ruleid,
                                   prolintdb.lint_warning.severity).

END.

PUBLISH "Prolint_FinalizeResults".




