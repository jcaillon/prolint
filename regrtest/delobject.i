/* ----------------------------------------------------------------------------
   delobject.i  : testcases for delobject.p
   expect a warning when the number of CREATE statements is larger than the
   number of "DELETE OBJECT" + "DELETE WIDGET" statements.
   Don't count CREATE statements where a widget-pool is specified: assume that
   you know what you're doing when you specify widget-pools.
   Not specifying any widget-pool may be better, or worse. Hard to tell :-(
   ---------------------------------------------------------------------------- */
RETURN.  /* this source is supposed to compile, but you must never run it! */

DEFINE VARIABLE hq3 AS HANDLE NO-UNDO.
DEFINE VARIABLE hq4 AS HANDLE NO-UNDO.
DEFINE VARIABLE hq5 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hq6 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hq7 AS WIDGET-HANDLE NO-UNDO.

CREATE QUERY hq3.
CREATE QUERY hq4.
DELETE object hq4.
CREATE QUERY hq4.

RUN NewQuery (OUTPUT hq5).
DELETE OBJECT hq5.

CREATE QUERY hq6.
DELETE WIDGET hq6.  /* support DELETE WIDGET as well as DELETE OBJECT */

/* ignore CREATE when "IN WIDGET-POOL" is specified: */
CREATE QUERY hq7 IN WIDGET-POOL "delobject-a":U.

PROCEDURE delobject_1 :
    /* purpose: see if it works differently in an internal procedure */
    DEFINE VARIABLE hq AS HANDLE NO-UNDO.
    DEFINE VARIABLE hq2 AS HANDLE NO-UNDO.

    CREATE QUERY hq.
    CREATE QUERY hq2.
    DELETE object hq2.
    CREATE QUERY hq2.
END PROCEDURE.

PROCEDURE NewQuery :
    /* purpose: construct a query for use elsewhere. Prolint can not trace
                if the query in the actual parameter is ever deleted */
    DEFINE OUTPUT PARAMETER pQ AS HANDLE NO-UNDO.
    CREATE QUERY pQ.
END PROCEDURE.

PROCEDURE delobject_2 :
    /* purpose: demo different iterations. Prolint can't count! */
    DEFINE VARIABLE hdelobject AS HANDLE NO-UNDO EXTENT 500.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.

    DO i=1 TO 500 :
       CREATE QUERY hdelobject[i].
    END.

    DO i=3 to 5 :
       DELETE OBJECT hdelobject[i].
    END.
    /* Prolint thinks the number of DELETES matches the number of CREATES,
       but Prolint is shortsighted */

END PROCEDURE.
