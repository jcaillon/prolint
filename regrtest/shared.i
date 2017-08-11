/* --------------------------------------------------
   file    : shared.i
   purpose : testcases for rule shared.p
             locate SHARED objects
   --------------------------------------------------- */             


/* only allow SHARED variables if they are "NEW GLOBAL SHARED" procedure-handles. */
/* well, actually we can't know if those handles are used for procedure-handles, so 
   let's just settle for handles. Not for widget-handles :-) */
   
DEFINE NEW GLOBAL SHARED VARIABLE shared_a AS HANDLE NO-UNDO.
DEFINE            SHARED VARIABLE shared_c AS HANDLE NO-UNDO.
DEFINE NEW        SHARED VARIABLE shared_d AS HANDLE NO-UNDO.
{&_proparse_ prolint-nowarn(shared)}
DEFINE NEW GLOBAL SHARED VARIABLE shared_e AS INTEGER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE shared_f AS INTEGER NO-UNDO.
   
/* shared temp-tables are not good */   
DEFINE NEW SHARED TEMP-TABLE tt_shared NO-UNDO 
   FIELD test1 AS CHARACTER.
   
DEFINE SHARED TEMP-TABLE tt_shared_b NO-UNDO 
   FIELD test1 AS CHARACTER.
                                   
/* shared streams are allowed, because you can't get a stream handle */                                     
DEFINE NEW SHARED STREAM shared_g.
DEFINE SHARED STREAM shared_h.

/* shared buffers are not allowed */
DEFINE SHARED BUFFER shared_cust FOR customer.
                               
                               
