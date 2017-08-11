/* ==========================================================================================
    file    : prolint/launch/test.p
    purpose : regression test for prolint itself.
              if you changed or added something to prolint, run prolint/launch/test.p
              to see if it still finds the same warnings

    Copyright (C) 2001-2006 Jurjen Dijkstra

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
   ========================================================================================== */


/* how it works: well pretty simple actually...
   program "prolint/regrtest/test.p" contains a lot of bad programming.
   profile "regression test" writes logfile "lint-selftest.log" and when 
   it is done, it compares this new log to the already existing "prolint/regrtest/expect.log".
   if "no differences encountered", prolint is working as expected */

{prolint/core/dlc-version.i}

define variable alreadyconnected as logical no-undo.
alreadyconnected =  CONNECTED("prolintest":U).

IF CONNECTED("sports":U) OR CONNECTED("sports2000":U) THEN DO:
   MESSAGE "Please Disconnect The SPORTS Database":T SKIP(1)
           "The Prolint regression test requires its own database,":T SKIP
           "which cannot be connected alongside SPORTS":T
           VIEW-AS ALERT-BOX.
   RETURN.
END.

IF NOT CONNECTED("prolintest":U) THEN DO:
   FILE-INFO:FILE-NAME = "prolint/regrtest/db/prolintest.db":U.
   IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
      CONNECT VALUE(FILE-INFO:FULL-PATHNAME) -1 NO-ERROR.
      IF NOT CONNECTED("prolintest":U) THEN
         MESSAGE "Test Database Could Not Be Connected":T SKIP(1)
                 ERROR-STATUS:GET-MESSAGE(1)
                 VIEW-AS ALERT-BOX.
   END.
   ELSE
      MESSAGE "Test Database Not Found":T SKIP(1)
              "Please create and connect database prolint/regrtest/db/prolintest.db":T SKIP
              "with the DF file found in prolint/regrtest/db":T SKIP
              "and then try the regression test again":T
              VIEW-AS ALERT-BOX.

END.

IF CONNECTED("prolintest":U) THEN
   RUN prolint/core/prolint.p ("prolint/regrtest" &IF {&dlc-version}=10 &THEN + ",prolint/regrtest-oo" &ENDIF ,
                          ?,
                          "regression test":U,
                          TRUE).


IF CONNECTED("prolintest":U) THEN
   if not alreadyconnected then
      DISCONNECT "prolintest":U.


