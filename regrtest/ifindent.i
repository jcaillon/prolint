/* test rule "ifindent" */

DEFINE VARIABLE c1    AS CHARACTER                NO-UNDO.      
DEFINE VARIABLE vd1   AS CHARACTER                NO-UNDO.      
DEFINE VARIABLE vd2   AS CHARACTER INITIAL "Bye"  NO-UNDO.      
DEFINE VARIABLE x1    AS CHARACTER                NO-UNDO.      
DEFINE VARIABLE y1    AS CHARACTER INITIAL "oops" NO-UNDO.      
DEFINE VARIABLE i1    AS INTEGER                  NO-UNDO.      
DEFINE VARIABLE i2    AS INTEGER                  NO-UNDO.      
DEFINE VARIABLE ok    AS LOGICAL   INITIAL TRUE   NO-UNDO.      
DEFINE VARIABLE other AS LOGICAL   INITIAL FALSE  NO-UNDO.      
DEFINE VARIABLE pay-stat AS INTEGER INITIAL 1.
DEF VAR vlog AS LOGICAL INITIAL TRUE NO-UNDO.
DEF VAR a    AS CHAR. 
DEF VAR c    AS CHAR NO-UNDO.
DEF VAR c2   AS CHAR NO-UNDO.
DEF VAR log1 AS LOG  NO-UNDO.
DEF VAR log2 AS LOG  NO-UNDO.
DEF VAR ii   AS INT  NO-UNDO.
DEF VAR h as HANDLE.
DEF VAR flat AS LOGICAL NO-UNDO.
DEF VAR proc AS CHARACTER NO-UNDO.


IF ok = TRUE THEN 
  ASSIGN c1 = "correct". 
  IF other = TRUE THEN RETURN.   /* warn */
ELSE        /* warn: Should be for "ok = false", actually is for "other = false" */
  ASSIGN c1 = "oops". 


IF ok = TRUE THEN DO:         /* warn */
  ASSIGN c1 = "correct". 
  IF other = TRUE THEN RETURN.
END.
ELSE       
  ASSIGN c1 = "oops". 


/* if the indenting for statement2 is >= statement1, then fire the rule 
 * make sure the ASSIGN is a statehead, and so is the vd2 assignment */
IF other THEN
  ASSIGN vd1 = "Hello".
    vd2 = "oops".             /* warn */


IF other THEN
  ASSIGN 
    x1 = "Red".
    y1 = "Green".             /* warn */


IF ok THEN DO:
   i1 = i1 + 1.
   i2 = i2 * 2.
END.


MESSAGE c1 vd2 y1 VIEW-AS ALERT-BOX. 


IF other THEN
  ASSIGN vd1 = "Hello".
ELSE IF ok THEN 
  ASSIGN vd1 = "Goodbye".


IF other THEN
  ASSIGN vd1 = "Hello".
ELSE DO: IF ok THEN 
  ASSIGN vd1 = "Red".                   /* warn */
END. 


IF ok THEN
/* some silly two-line
 * comment */  i1 = i1 + 4.
               i2 = i2 + 5.             /* warn */

  IF ok THEN
  /* comment */  i1 = i1 + 7.
  i2 = i2 + 8.


  IF ok THEN
  /* comment */ i1  = i1 + 7.
                 i2 = i2 + 8.             /* warn */

  /* If's comment */  IF ok THEN
  /* more comment */  i1 = i1 + 7.




UPDATE pay-stat VIEW-AS RADIO-SET        /* warn */
  RADIO-BUTTONS "Unpaid", 1, 
                "Part", 2, 
                "Paid in full", 3          
  vlog.

IF vlog THEN
  CASE pay-stat:
    WHEN 1 THEN
      MESSAGE "This account is unpaid.".
    WHEN 2 THEN  
      MESSAGE "This account is partially paid.".
    WHEN 3 THEN  
      MESSAGE "This account is paid in full.".
  END CASE.
ELSE 
  MESSAGE "vlog = FALSE.".



IF TRUE THEN DO: 
  IF TRUE THEN DO: 
    IF FALSE THEN  
      MESSAGE "oops" VIEW-AS ALERT-BOX.
  END. 
END. 
IF FALSE THEN RETURN.


        IF TRUE THEN             /* warn */
	  assign a = "blue".


IF c = "W" AND log1 THEN
       IF c2 = 'Update':U THEN
             log2 = NO.
       ELSE
             log2 = YES.
ELSE log1 = FALSE.



/* TEST OF #202 WARNINGS: false positives and bad indenting.
 * Warnings should be at: #3,6,7,8,10,29,30
 */
FOR EACH customer NO-LOCK: 
  IF TRUE THEN MESSAGE "Customer: " customer.name. 
END. /* 1. */

IF TRUE THEN DO: 
  ii = 1.
END. 
ELSE DO: 
  IF TRUE THEN ii = 1.
END. /* 2. */

  IF AVAILABLE customer THEN DO:  /* 3. #101 */
    IF customer.name = "" THEN
      ASSIGN 
        c = "blank".
    ELSE
      ASSIGN 
        c = customer.name.
  END.    /* 4. */
  ELSE DO: 
    IF c = "" THEN
      ASSIGN 
        c = "blank".
    ELSE
      ASSIGN 
        c = "not blank".  
  END.   /* 5. */ 


 IF ii = 1 THEN DO:  /* 6. #202 */ 
  IF TRUE THEN DO: 
    ii = 1.
  END. 
END. /* 7. #102 */

IF customer.name = "" THEN  /* 8. #202 */
   ASSIGN c = customer.city.
ELSE
   IF customer.name <> "" THEN DO:
      ASSIGN c = customer.name.
   END.
   ELSE
      ASSIGN c = "". 

ASSIGN c = c + "END".  /* 9. */


  if not available(customer)
     then do:
       create customer. 
       assign customer.balance  = 54321
              customer.name     = "Test".
     end.
 find first customer no-lock.  /* 10. #202 */


REPEAT:
    ASSIGN c = ""
           ii = 0.
    IF c MATCHES "*" THEN
        log1 = TRUE.
    IF ii > 10 THEN   /* 11. Ok. */
       ii = ii + 1.
END.  /* 12. */

REPEAT:
    PROMPT-FOR customer.name.
    FIND customer USING customer.name NO-ERROR.
    IF NOT AVAILABLE customer
    THEN
    DO:
	CREATE customer.
	ASSIGN customer.name.
    END.
    UPDATE customer EXCEPT comments WITH 2 COLUMNS. /* 13. Ok. */
END.


FOR EACH customer, EACH order OF customer:
    UPDATE customer.balance
    EDITING:
	DISPLAY " Editing customer: ". 
	IF LASTKEY = KEYCODE("RETURN") THEN
	    MESSAGE "Pressed Return".
    END. /* 14. */
    HIDE FRAME custfrm.
END.



REPEAT:
    IF proc = "L" THEN TESTBLOCK: DO:
       IF OPSYS = "UNIX" THEN UNIX ls.
       ELSE IF OPSYS = "MSDOS" THEN DOS dir.
       ELSE IF OPSYS = "os2" THEN OS2 dir.
       ELSE VMS directory.
    END.  /* 15. */

    ELSE DO:   
	c = "blue". 
    END.
END. /* 16. */


REPEAT:
    IF proc = "L" THEN
       IF OPSYS = "UNIX" THEN UNIX ls.
       ELSE IF OPSYS = "MSDOS" THEN DOS dir.
       ELSE IF OPSYS = "os2" THEN OS2 dir.
       ELSE VMS directory.

    ELSE DO:  /* 17. */
	c = "blue". 
    END.
END.  /* 18. */


CASE ii:
  WHEN 1 THEN 
    IF log1 THEN
      log2 = YES.
  WHEN 2 THEN       /* 19. */
    IF log2 THEN
      log1 = YES.
  WHEN 3 THEN       /* 20. */
    IF TRUE THEN
      log2 = NO.
END CASE.           /* 21. */


CASE ii:
  WHEN 1 THEN 
    IF log1 THEN DO:
      log2 = YES.
      LEAVE. 
    END. 
  WHEN 2 THEN       /* 22. */
    IF log2 THEN DO:
      log1 = YES.
      LEAVE. 
    END. 
  WHEN 3 THEN       /* 23. */
    IF TRUE THEN DO:
      log2 = NO.
      LEAVE. 
    END. 
END CASE.           /* 24. */


IF NOT AVAILABLE customer THEN 
DO: 
  c = "". 
END.
ELSE IF TRUE THEN 
DO: 
  c = "". 
END.
ELSE IF NOT CAN-FIND(FIRST customer) THEN
    IF NOT CAN-FIND(LAST customer) THEN
    DO:
        ASSIGN c = "red".
        RETURN.
    END.
                       
IF NOT CAN-FIND(FIRST order) THEN   /* 25. */
DO:
  ASSIGN c = "green".
  RETURN.
END.


CASE c:                     /* 26. */
  WHEN "Blue" THEN
      IF FALSE THEN 
          RUN test13.p. 
  WHEN "Red" THEN DO:       /* 27. */
       IF TRUE THEN
          RUN test12.p.
  END.                      /* 28. */
  WHEN "Green" THEN IF FALSE THEN   /* 29. */
       RUN test10.p.                /* 30. */
  WHEN "Orange" THEN IF TRUE THEN   /* 31. */ 
       RUN test15.p.                /* 32. */
END.
 

IF c = "W" AND log1 THEN
       IF c2 = 'Update':U THEN
             log2 = NO.
       ELSE
             log2 = YES.
ELSE log1 = FALSE.          /* 33. */


/*********** end of #202 testing section **********/


/* Test tab replacement. If it isn't working, or tabSize is 4 or less, 
 * this will give 101 or 102 for the CREATE customer and ASSIGN name.
 * (also various other test cases use tabs, so there will be an increase 
 * in the number of warnings by 10 more.)
 */
REPEAT:
    PROMPT-FOR customer.name.
    FIND customer USING customer.name NO-ERROR.
    IF NOT AVAILABLE customer
    THEN
    DO:
	CREATE customer.
	ASSIGN customer.name.
    END.
    UPDATE customer EXCEPT comments WITH 2 COLUMNS.
END.



FOR EACH customer, EACH order OF customer:
    DISPLAY carrier WITH CENTERED ROW 2 FRAME carrier.
    UPDATE
    customer.balance AT 5 customer.name AT 30 SKIP
	WITH FRAME custfrm WITH CENTERED 1 DOWN
    EDITING:
	IF LASTKEY = KEYCODE("RETURN") THEN
	    MESSAGE " Press the space bar to edit order shipdate".
    END.
 END.

FOR EACH customer, EACH order OF customer:
    DISPLAY carrier WITH CENTERED ROW 2 FRAME carrier2.
    UPDATE
    customer.balance AT 5 customer.name AT 30 SKIP
	WITH FRAME custfrm2 WITH CENTERED 1 DOWN.
    DO:
	IF LASTKEY = KEYCODE("RETURN") THEN
	    MESSAGE " Press the space bar to edit order shipdate".
    END. 
 END.


 def var i3 as int no-undo. 
 
 do while true: 
          if  i1 - 
                                  (i2 + i3) = 0 
          then next.                                                                 
 end. 



  IF VALID-HANDLE (h) THEN  
     RUN test1.p.


&scop PART1 FOR EACH order: DISPLAY order.terms. END.
&scop PART2 IF AVAILABLE order THEN MESSAGE "avail order" VIEW-AS ALERT-BOX.
&scop PART3 IF TRUE THEN FIND FIRST order NO-LOCK NO-ERROR.

&scop ALLPARTS {&PART3} {&PART2} {&PART1}
IF TRUE THEN DO:                  /* warn */
  MESSAGE "TRUE" VIEW-AS ALERT-BOX. 
END.
ELSE DO:
  {&ALLPARTS}                     /* warn */
END.

 IF FALSE THEN MESSAGE "oops".          /* warn */


FOR EACH customer NO-LOCK:              /* warn */
  IF LENGTH(customer.name) < 14 
  OR SUBSTRING(customer.name,LENGTH(customer.name) - 1,2) = 'XX' 
    THEN PUT customer.name customer.state SKIP. END.

for each customer NO-LOCK break by customer.state: 
  if first-of(customer.state) then 
     find first order of customer no-lock no-error. 

  if not available order then do: 
     message "n/a order" view-as alert-box. 
  end. 
end. 


FIND FIRST order. 
IF AVAILABLE order
   THEN IF (order.terms >= "20" OR order.terms <= "40")
      THEN flat = TRUE.                      /* warn */
      ELSE flat = FALSE.                     /* warn */
   ELSE IF flat THEN MESSAGE "boo".          /* warn */




/* This code tests the old problem with certain preprocess tokens
 * in the hidden tokens. (Used to give two #103 warnings in the include).
 * This also reproduced some false positives when going in and out of includes.
 * (the two #102s)   These problems are now fixed. 
 */
FIND FIRST customer.
FIND FIRST invoice.
IF AVAILABLE invoice THEN DO:
    {prolint/regrtest/ifindentpreproc.i   &invoice="invoice"  
                                          &customer="customer"}
END. 



/* This code reproduces the current problem with linking together 
 * certain comments and other hidden tokens. (gives two #103 warnings now).
 */
FIND FIRST customer. 
FIND FIRST invoice. 

     IF TRUE THEN
       ASSIGN 
         customer.balance = invoice.amount
        /******/ .               /* problem with hidden tokens here */

     IF TRUE THEN
       ASSIGN 
         a = "Test"
        /******/ . 

     IF TRUE THEN
       ASSIGN 
         a = customer.country
       /****** 
         customer.name = CAPS(customer.name)
        ******/ .                /* problem with hidden tokens here */

 

IF c1 = "Green" THEN                      /* warn */
    c = "Orange". 
ELSE IF (c2 = "Red") THEN 
         IF c1 = "Yellow" THEN RETURN.
ELSE DO:                                  /* warn */
  /* other stuff */
END.                                      /* warn */




/* Test of problem with getColumn() calculating indents for preprocessors. */
DEFINE QUERY BROWSE-office FOR 
      customer SCROLLING.

&Scoped-define OPEN-QUERY-BROWSE-office OPEN QUERY Q1 FOR EACH customer WHERE customer.balance >= 10000 NO-LOCK.

  IF RETURN-VALUE <> "OK" THEN 
    RETURN NO-APPLY. 

  {&OPEN-QUERY-BROWSE-office}        /* Gives #202 now, but shouldn't. */
 


{prolint/regrtest/ifindentextra.i}

