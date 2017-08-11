/* ------------------------------------------------------------------------
   file    :  prolint/rules/where-udf.p
   by      :  John Green
   purpose :  Find WHERE clauses which contain a UDF 
   ------------------------------------------------------------------------

    Copyright (C) 2002 John Green, Jurjen Dijkstra

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
   ---------------------------------------------------------------------------
*/
  
{prolint/core/ruleparams.i}
{prolint/core/ttprocedure.i}

DEFINE TEMP-TABLE tt_function NO-UNDO
   FIELD fname   AS CHARACTER
   FIELD fsafety AS LOGICAL
   INDEX idx_fname AS PRIMARY UNIQUE fname.

FUNCTION IsFunctionSafe RETURNS LOGICAL (INPUT functionname AS CHARACTER) IN hpRulePersist.

RUN ProcedureListGet IN hLintSuper (OUTPUT TABLE tt_procedure).

RUN searchNode            (hTopnode,        /* "Program_root" node */
                           "InspectNode":U,  /* name of callback procedure */
                           "WHERE":U).        /* list of statements to search, ?=all */


RETURN.


FUNCTION IsLocalFunctionSafe RETURNS LOGICAL (INPUT functionname AS CHARACTER) :
   /* purpose: try to determine if the function contains db access */

   DEFINE VARIABLE IsSafe      AS LOGICAL NO-UNDO.
   DEFINE VARIABLE IsDangerous AS LOGICAL NO-UNDO.
   DEFINE VARIABLE IsUnknown   AS LOGICAL NO-UNDO.

   /* if funcionname is on the whitelist, then ok */
   IsSafe = IsFunctionSafe (functionname).
   IF IsSafe=TRUE THEN
      RETURN TRUE.
   ELSE DO:
   /* if not on the whitelist, then try to find the implementation of
      the function in this c.u. and see if it appears to be safe */
      FIND tt_function WHERE tt_function.fname=functionname NO-ERROR.
      IF AVAILABLE tt_function THEN
         IF tt_function.fsafety = TRUE THEN
            RETURN TRUE.
         ELSE
            RETURN FALSE.
      ELSE DO:
         /* function is not yet evaluated; do it now and cache the result in
            temp-table tt_function */
         FIND tt_procedure WHERE (tt_procedure.proctype  = "FUNCTION":U OR tt_procedure.proctype  = "METHOD":U)
                             AND tt_procedure.procname  = functionname
                             AND tt_procedure.prototype = FALSE
                           NO-ERROR.
         IF NOT AVAILABLE tt_procedure THEN DO:
            CREATE tt_function.
            ASSIGN tt_function.fname   = functionname
                   tt_function.fsafety = ?.  /* not determined. not safe */
            RETURN FALSE.
         END.
         ELSE DO:
            /* look for dangerous things (FIND, QUERY, Record_name, ...) */
            IsDangerous = FALSE.
            IsDangerous = IsDangerous OR parserQueryCreate(tt_procedure.startnode, "qdanger":U, "FIND":U) > 0.
            parserQueryClear("qdanger":U).
            IsDangerous = IsDangerous OR parserQueryCreate(tt_procedure.startnode, "qdanger":U, "QUERY":U) > 0.
            parserQueryClear("qdanger":U).
            IsDangerous = IsDangerous OR parserQueryCreate(tt_procedure.startnode, "qdanger":U, "FOR":U) > 0.
            parserQueryClear("qdanger":U).
            IsDangerous = IsDangerous OR parserQueryCreate(tt_procedure.startnode, "qdanger":U, "RECORD_NAME":U) > 0.
            parserQueryClear("qdanger":U).

            /* then look for indirect danger (USER_FUNC, SUPER, RUN, ...) */
            IsUnknown = FALSE.
            IsUnknown = IsUnknown OR parserQueryCreate(tt_procedure.startnode, "qdanger":U, "USER_FUNC":U) > 0.
            parserQueryClear("qdanger":U).
            IsUnknown = IsUnknown OR parserQueryCreate(tt_procedure.startnode, "qdanger":U, "DYNAMICFUNCTION":U) > 0.
            parserQueryClear("qdanger":U).
            IsUnknown = IsUnknown OR parserQueryCreate(tt_procedure.startnode, "qdanger":U, "RUN":U) > 0.
            parserQueryClear("qdanger":U).
            IsUnknown = IsUnknown OR parserQueryCreate(tt_procedure.startnode, "qdanger":U, "SUPER":U) > 0.
            parserQueryClear("qdanger":U).

            IsSafe = NOT (IsDangerous OR IsUnknown).

            CREATE tt_function.
            ASSIGN tt_function.fname   = functionname
                   tt_function.fsafety = IF IsSafe THEN TRUE ELSE IF IsDangerous THEN FALSE ELSE ?.
            RETURN IsSafe.
         END.
      END.
   END.

   /* all the way down and still no answer? Impossible. */
   RETURN FALSE.

END FUNCTION.


FUNCTION UnQuote RETURNS CHARACTER (INPUT qstring AS CHARACTER) :
   /* purpose : removes the quotes from a QSTRING.
                and also the string-attribute */
   DEFINE VARIABLE quote  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE strlen AS INTEGER   NO-UNDO.

   quote     = SUBSTRING(qstring,1,1).
   strlen    = R-INDEX(qstring, quote) - 2.
   
   RETURN TRIM(SUBSTRING(qstring,2,strlen)).
END FUNCTION.


PROCEDURE InspectNode :
  /* purpose : callback from searchNode. Inspect the node found by searchNode */
  DEFINE INPUT  PARAMETER theNode        AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER AbortSearch    AS LOGICAL NO-UNDO INITIAL NO.
  DEFINE OUTPUT PARAMETER SearchChildren AS LOGICAL NO-UNDO.
  
  ASSIGN
    SearchChildren = FALSE  /* a WHERE clause can't contain more WHERE clauses */
    .

  DEFINE VARIABLE numResults   AS INTEGER NO-UNDO.
  DEFINE VARIABLE FoundUnsafe  AS LOGICAL NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE i            AS INTEGER NO-UNDO.
  DEFINE VARIABLE subNode      AS INTEGER NO-UNDO.


  subNode = parserGetHandle().
  
  numResults = parserQueryCreate(theNode, "userfuncs":U, "USER_FUNC":U).
  i = 1.
  DO WHILE i<=numResults AND NOT FoundUnsafe :
     parserQueryGetResult("userfuncs":U, i, subNode).
     IF NOT IsLocalFunctionSafe(parserGetNodeText(subNode)) THEN
        FoundUnsafe = TRUE.
     i = i + 1.
  END.
  parserQueryClear("userfuncs":U).


  /* Assume: a dynamic functions always calls a user-defined function. Agree?? */
  IF NOT FoundUnsafe THEN DO:
     numResults = parserQueryCreate(theNode, "userfuncs":U, "DYNAMICFUNCTION":U).
     i = 1.
     DO WHILE i<=numResults AND NOT FoundUnsafe :
         parserQueryGetResult("userfuncs":U, i, subNode).
         /* now we are looking for the first QSTRING after the LEFTPAREN */
         FoundUnsafe = TRUE.
         IF "LEFTPAREN":U = parserNodeFirstChild(subNode, subNode) THEN
            IF "QSTRING":U = parserNodeNextSibling(subNode, subNode) THEN
               IF IsLocalFunctionSafe(UnQuote(parserGetNodeText(subNode))) THEN
                  FoundUnsafe = FALSE.
               /*  If in doubt, the function is unsafe :-)  */
         i = i + 1.
     END.
     parserQueryClear("userfuncs":U).
  END.


  /* Create a query for user functions, check the number of results... */
  IF FoundUnsafe THEN
    RUN PublishResult            (compilationunit,
                                  parserGetNodeFilename(theNode),
                                  parserGetNodeLine(theNode), 
                                  "User Defined Function used in WHERE clause":T,
                                  rule_id).

  parserReleaseHandle(subNode).
    
END PROCEDURE.

