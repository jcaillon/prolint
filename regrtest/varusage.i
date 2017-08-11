/* ---------------------------------------------------------------------------------------------
   file    : prolint/regrtest/varusage.i
   purpose : testcases for:
               a. variable/parameter xxx is never accessed
               b. variable/parameter yyy in procedure|function|trigger hides object in program scope
   --------------------------------------------------------------------------------------------- */             
RETURN.  /* do not run this program. It may do annoying things on your system */

DEFINE VARIABLE v_varusage_1  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v_varusage_1b AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v_varusage_2  AS DECIMAL   NO-UNDO INITIAL 32.
DEFINE VARIABLE v_varusage_3  AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE v_varusage_4  AS CHARACTER NO-UNDO INITIAL "hello":U.
DEFINE VARIABLE v_varusage_5  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v_varusage_8  AS HANDLE    NO-UNDO.
DEFINE VARIABLE v_varusage_9a AS INTEGER   NO-UNDO.
DEFINE VARIABLE v_varusage_9b AS INTEGER   NO-UNDO INITIAL 10.
DEFINE VARIABLE v_varusage_10 AS INTEGER   NO-UNDO.
DEFINE VARIABLE v_varusage_11 AS HANDLE    NO-UNDO.
DEFINE VARIABLE v_varusage_12 AS HANDLE    NO-UNDO.
DEFINE VARIABLE v_varusage_14 AS DECIMAL   NO-UNDO INITIAL 0.0.

                                
   /* v_varusage_1 is assigned, but not used (it appears only on left-hand side of "=" ) */
   v_varusage_1  = SQRT(v_varusage_2) + v_varusage_3.
   v_varusage_1b = SQRT(v_varusage_2  + v_varusage_3).
             
   /* "IF {expression} THEN" is a use, even though it doesn't appear on right-hand side of assignment */             
   IF v_varusage_1b > 0.0 THEN 
      DISPLAY v_varusage_4.                
      
   /* same, but simpler: */
   IF v_varusage_5 THEN 
      DISPLAY v_varusage_4.                
                                            
   /* disclaimer: names are fictional */
   FOR EACH customer EXCLUSIVE-LOCK WHERE customer.balance < 0 :
      ASSIGN customer.balance = v_varusage_14.
   END.
                
   /* v_varusage_8 is definitely accessed here: */
   RUN myproc.p ON SERVER v_varusage_8.
   
   DO v_varusage_9a=1 TO v_varusage_9b :
      DISPLAY COLOR-TABLE:GET-BLUE-VALUE(v_varusage_10). /* access v_varusage_10 */
   END.

/* event handlers have their own variables, so they must be treated like procedures */
ON CLOSE OF THIS-PROCEDURE DO:
   DEFINE VARIABLE v_varusage_12 AS INTEGER NO-UNDO.
   DEFINE VARIABLE v_varusage_13 AS INTEGER NO-UNDO.
   DISPLAY v_varusage_12.
END.                                               

            
ON WRITE OF customer NEW new-cust OLD old-cust
  DO:
     DEFINE VARIABLE v_varusage_newcity AS CHARACTER NO-UNDO.
     DEFINE VARIABLE v_varusage_oldcity AS CHARACTER NO-UNDO.
     /* not only do we override the name of this variable, we also change the datatype! Bad! */
     DEFINE VARIABLE v_varusage_1       AS LOGICAL   NO-UNDO.
      
     ASSIGN v_varusage_newcity = new-cust.city
            v_varusage_oldcity = old-cust.city
            v_varusage_1       = v_varusage_newcity NE old-cust.city.
     IF v_varusage_1 THEN 
        RETURN ERROR "no, changing the city is not allowed":T.
  END.

/* following procedures _pp1 to _pp4 test how procedure handles are accessed */

PROCEDURE ip_varusage_pp1 :
   /* set v_varusage_ph assigns a value, but does not access it */
   DEFINE VARIABLE v_varusage_ph1 AS HANDLE NO-UNDO.
   RUN myproc.p PERSISTENT SET v_varusage_ph1.
END PROCEDURE.            


PROCEDURE ip_varusage_pp2 :
   /* delete does access the value */
   DEFINE VARIABLE v_varusage_ph2 AS HANDLE NO-UNDO.
   RUN myproc.p PERSISTENT SET v_varusage_ph2.
   DELETE PROCEDURE v_varusage_ph2.  
END PROCEDURE.            


PROCEDURE ip_varusage_pp3 :
   /* set v_varusage_async, but don't actually access it */
   DEFINE VARIABLE v_varusage_ph3   AS HANDLE NO-UNDO.
   DEFINE VARIABLE v_varusage_async AS HANDLE NO-UNDO.
   RUN myproc.p PERSISTENT SET v_varusage_ph3.
   /* run in.. accesses the handle */
   RUN something IN v_varusage_ph3. 

   RUN somethingelse IN v_varusage_ph3 ASYNCHRONOUS SET v_varusage_async.
END PROCEDURE.            
                            
                            
PROCEDURE ip_varusage_pp4 :
   /* see if prolint detects handle in dynamic function (yep, it does) */
   DEFINE VARIABLE v_varusage_ph4 AS HANDLE NO-UNDO.
   RUN myproc.p PERSISTENT SET v_varusage_ph4. 
   DYNAMIC-FUNCTION("somefunction":U IN v_varusage_ph4).
END PROCEDURE.            
                            
                                                            
PROCEDURE ip_varusage_1 IN SUPER:
   /* these params are not used, but that doesn't matter because this proc is a prototype (in super) */
   DEFINE INPUT  PARAMETER p_varusage_a AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER p_varusage_b AS LOGICAL NO-UNDO.
END PROCEDURE.

/* handle v_varusage_11 will be accessed when function udf_varusage_2 is called */
FUNCTION udf_varusage_2 RETURNS LOGICAL (something AS LOGICAL, something_else AS INTEGER) IN v_varusage_11.
              
PROCEDURE ip_varusage_2 :
   /* one of these params are used. Notice the name conflict with variable on larger scope */
   DEFINE INPUT  PARAMETER v_varusage_1 AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER p_varusage_b AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER p_varusage_c AS DECIMAL NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER p_varusage_d AS DECIMAL NO-UNDO.

   DEFINE VARIABLE v_varusage_ip AS INTEGER NO-UNDO.

   DISPLAY v_varusage_1. /* this is the local parameter, not the large-scoped variable */
END PROCEDURE.  

/* some more experiments:
    1. do we notice that 'whocares' is not used?
    2. do we notice that v_varusage_6 is used?
    3. do we see there is NO name conflict with v_varusage_ip in ip_varusage_2 */
        
FUNCTION udf_varusage_1 RETURNS LOGICAL (whocares AS LOGICAL, nobody AS INTEGER) :
   /*  var is defined in an ip and in an udf, but doesn't override a program-scoped var */
   DEFINE VARIABLE v_varusage_6 AS LOGICAL.
   DEFINE VARIABLE v_varusage_ip AS INTEGER NO-UNDO.
   
   RETURN v_varusage_6.
END FUNCTION.                                                           


PROCEDURE ip_varusage_browse :
  /* notice that hColumn is always on the left-hand side of assign statements. 
     Still, Prolint shouldn't say that hColumn is never accessed. */

  DEFINE VARIABLE hColumn AS HANDLE NO-UNDO.
  DEFINE VARIABLE hbCust  AS HANDLE NO-UNDO.

  OPEN QUERY hQuery FOR EACH customer NO-LOCK WHERE customer.balance=0.
  CREATE BROWSE hbCust
     ASSIGN QUERY = QUERY hQuery:HANDLE.
             
  hColumn = hbCust:ADD-LIKE-COLUMN (customer.city).
  hColumn:READ-ONLY = FALSE.
                                             
  /* by the way: hbCust is created and not deleted, so that's a resource loss...  */
END PROCEDURE.

  /* hMenuItem appears to be only created, but it will be accessed on 'choose' */
  DEFINE VARIABLE hMenuItem AS HANDLE NO-UNDO.
  CREATE MENU-ITEM hMenuItem 
       ASSIGN 
          LABEL  = "Open source in external editor":T
          TRIGGERS:
             ON 'choose':U PERSISTENT RUN BrowseDoubleClick IN THIS-PROCEDURE. 
          END TRIGGERS.


/* test the IMPORT statement */
DEFINE VARIABLE v_varusage2_1a  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v_varusage2_2a  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v_varusage2_3a  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v_varusage2_4a  AS INTEGER   NO-UNDO.
DEFINE VARIABLE v_varusage2_5a  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v_varusage2_1b  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v_varusage2_2b  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v_varusage2_3b  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v_varusage2_4b  AS INTEGER   NO-UNDO.
DEFINE VARIABLE v_varusage2_5b  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v_varusage2_1c  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v_varusage2_4c  AS INTEGER   NO-UNDO.
DEFINE VARIABLE v_varusage2_5c  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v_varusage2_2d  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v_varusage2_4d  AS INTEGER   NO-UNDO.
DEFINE VARIABLE v_varusage2_5d  AS CHARACTER NO-UNDO.
                               
DEFINE STREAM s_varusage2.                               

IMPORT 
   v_varusage2_1a
   v_varusage2_2a
   v_varusage2_3a
   v_varusage2_4a
   v_varusage2_5a.
   
IMPORT STREAM s_varusage2
   v_varusage2_1b
   v_varusage2_2b
   v_varusage2_3b
   v_varusage2_4b
   v_varusage2_5b.
   
IMPORT 
   v_varusage2_1c
   ^
   ^
   v_varusage2_4c
   v_varusage2_5c.
   
IMPORT STREAM s_varusage2
   ^
   v_varusage2_2d
   ^
   v_varusage2_4d
   v_varusage2_5d.


