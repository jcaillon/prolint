/* =======================================================================================
    file    : prolint/prolintdb/recalcstats.p
    purpose : delete all statistics and calculate new statistics
    note    : create the database with prolint/prolintdb/prolintdb.df
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

   DEFINE VARIABLE prevrule    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prevdir     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE prevcount   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE prevscore   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE maxseverity AS INTEGER   NO-UNDO.


   RUN set-hourglass (TRUE).

   /* first, delete all existing statistics */

   DO TRANSACTION:
      FOR EACH prolintdb.lint_stat_rule EXCLUSIVE-LOCK :
         DELETE prolintdb.lint_stat_rule.
      END.

      FOR EACH prolintdb.lint_stat_subdir EXCLUSIVE-LOCK :
         DELETE prolintdb.lint_stat_subdir.
      END.

      FOR EACH prolintdb.lint_stat_ruledir EXCLUSIVE-LOCK :
         DELETE prolintdb.lint_stat_ruledir.
      END.
   END.


   /* build lint_stat_rule from scratch */
   DO TRANSACTION:
       ASSIGN
           prevrule  = ""
           prevcount = 0
           prevscore = 0.
       FOR EACH prolintdb.lint_warning NO-LOCK
                                       BY prolintdb.lint_warning.ruleid :
           IF prolintdb.lint_warning.ruleid = prevrule THEN
              ASSIGN
                 prevcount = prevcount + 1
                 prevscore = prevscore + prolintdb.lint_warning.severity.
           ELSE DO:
              IF prevrule<>"" THEN DO:
                 CREATE prolintdb.lint_stat_rule.
                 ASSIGN prolintdb.lint_stat_rule.ruleid      = prevrule
                        prolintdb.lint_stat_rule.numwarnings = prevcount
                        prolintdb.lint_stat_rule.score       = prevscore
                        prolintdb.lint_stat_rule.severity    = INTEGER(prevscore / prevcount).
              END.
              ASSIGN
                prevrule  = prolintdb.lint_warning.ruleid
                prevcount = 1
                prevscore = prolintdb.lint_warning.severity.
          END.
       END.
       IF prevrule<>"" THEN DO:
          CREATE prolintdb.lint_stat_rule.
          ASSIGN prolintdb.lint_stat_rule.ruleid      = prevrule
                 prolintdb.lint_stat_rule.numwarnings = prevcount
                 prolintdb.lint_stat_rule.score       = prevscore
                 prolintdb.lint_stat_rule.severity    = INTEGER(prevscore / prevcount).
       END.
   END.


   /* build lint_stat_subdir */
   DO TRANSACTION:
       ASSIGN
           prevdir     = ""
           prevcount   = 0
           prevscore   = 0
           maxseverity = 0.
       FOR EACH prolintdb.lint_warning NO-LOCK
                                       BY prolintdb.lint_warning.subdir :
           IF prolintdb.lint_warning.subdir = prevdir THEN
              ASSIGN
                 prevcount   = prevcount + 1
                 maxseverity = MAXIMUM(maxseverity,prolintdb.lint_warning.severity)
                 prevscore   = prevscore + prolintdb.lint_warning.severity.
           ELSE DO:
              IF prevdir<>"" THEN DO:
                 CREATE prolintdb.lint_stat_subdir.
                 ASSIGN prolintdb.lint_stat_subdir.subdir      = prevdir
                        prolintdb.lint_stat_subdir.numwarnings = prevcount
                        prolintdb.lint_stat_subdir.severity    = maxseverity
                        prolintdb.lint_stat_subdir.score       = prevscore.
              END.
              ASSIGN
                prevdir     = prolintdb.lint_warning.subdir
                prevcount   = 1
                maxseverity = prolintdb.lint_warning.severity
                prevscore   = prolintdb.lint_warning.severity.
          END.
       END.
       IF prevdir<>"" THEN DO:
          CREATE prolintdb.lint_stat_subdir.
          ASSIGN prolintdb.lint_stat_subdir.subdir      = prevdir
                 prolintdb.lint_stat_subdir.numwarnings = prevcount
                 prolintdb.lint_stat_subdir.severity    = maxseverity
                 prolintdb.lint_stat_subdir.score       = prevscore.
       END.
   END.


   /* build lint_stat_ruledir */
   DO TRANSACTION:
       ASSIGN
           prevrule    = ""
           prevdir     = ""
           prevcount   = 0
           prevscore   = 0.
       FOR EACH prolintdb.lint_warning NO-LOCK
                                       BY prolintdb.lint_warning.subdir
                                       BY prolintdb.lint_warning.ruleid :
           IF prolintdb.lint_warning.subdir=prevdir AND prolintdb.lint_warning.ruleid=prevrule THEN
              ASSIGN
                 prevcount   = prevcount + 1
                 prevscore   = prevscore + prolintdb.lint_warning.severity.
           ELSE DO:
              IF prevdir<>"" THEN DO:
                 CREATE prolintdb.lint_stat_ruledir.
                 ASSIGN prolintdb.lint_stat_ruledir.subdir      = prevdir
                        prolintdb.lint_stat_ruledir.ruleid      = prevrule
                        prolintdb.lint_stat_ruledir.numwarnings = prevcount
                        prolintdb.lint_stat_ruledir.severity    = INTEGER(prevscore / prevcount)
                        prolintdb.lint_stat_ruledir.score       = prevscore.
              END.
              ASSIGN
                prevdir     = prolintdb.lint_warning.subdir
                prevrule    = prolintdb.lint_warning.ruleid
                prevcount   = 1
                prevscore   = prolintdb.lint_warning.severity.
          END.
       END.
       IF prevdir<>"" THEN DO:
          CREATE prolintdb.lint_stat_ruledir.
          ASSIGN prolintdb.lint_stat_ruledir.subdir      = prevdir
                 prolintdb.lint_stat_ruledir.ruleid      = prevrule
                 prolintdb.lint_stat_ruledir.numwarnings = prevcount
                 prolintdb.lint_stat_ruledir.severity    = INTEGER(prevscore / prevcount)
                 prolintdb.lint_stat_ruledir.score       = prevscore.
          END.
   END.


   RUN set-hourglass (FALSE).

PROCEDURE set-hourglass :
/*------------------------------------------------------------------------------
       Purpose:     Change the cursor to reflect the process state
       Parameters:  Logical - Yes/True = Wait
       Notes:
------------------------------------------------------------------------------*/
   define input parameter MUST-WAIT as LOGICAL no-undo.
   /* Set the cursor */
   if MUST-WAIT then SESSION:set-wait-state("GENERAL":U). /* Hourglass */
                else SESSION:set-wait-state("").        /* Arrow */
END PROCEDURE.


