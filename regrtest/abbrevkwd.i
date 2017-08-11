/* file abbrevkwd.i : test-cases for rule "abbrevkwd"
   looks for abbreviated keywords, ignores DEF VAR AS INT|CHAR  */

DEFINE VARIABLE abbrev1 AS CHARACTER NO-UNDO FORMAT "x(10)":U.
DEF VAR abbrev2 AS CHAR NO-UNDO FORM "x(10)":U.
DEF VAR abbrev3 AS INT NO-UNDO.   

/* you will get a warning on INIT. You would also want to get a warning on LOG */
DEF VAR abbrev4 AS LOG NO-UNDO INIT YES.
                      
/* substring or substitute? */
abbrev2 = SUBST(abbrev1, 1,4).   
DISP abbrev3.

/* custome is not a keyword, but it is an abbreviation. No warning */
OPEN query abbrev5 FOR EACH custome WHERE custome.cust > 100.
      
/* test for abbreviated widget property names, like width-p and height-c */
DEFINE BUTTON btn_abbrevkwd. /* won't compile if it isn't in a frame, so here is the bogus frame: */
DEFINE FRAME FRAME-Abbrevkwd
    btn_abbrevkwd AT ROW 1.48 COL 2 COLON-ALIGNED NO-LABELS
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 39 ROW 1.48
         SIZE 20 BY 3.33
         TITLE "Frame A":U.
ASSIGN 
   btn_abbrevkwd:width-p  = 20
   btn_abbrevkwd:height-c = 20.  
/* this would be ok: */                                                                                 
ASSIGN 
   btn_abbrevkwd:width-pixels  = 20
   btn_abbrevkwd:height-chars = 20.                                                                                
              

/* column is an abbreviation for columns, according to the KEYWORD-ALL() function */              
ASSIGN btn_abbrevkwd:COLUMN = 3.

/* "ASC" function versus "ASCENDING" option */
abbrev3 = ASC("A":U).
DEFINE TEMP-TABLE ttabbrev NO-UNDO
   FIELD nonsense AS CHARACTER
   INDEX idx1 AS PRIMARY UNIQUE nonsense ASC.

/* "FORM" statement versus "FORMAT" option */
DEFINE VARIABLE abbrev5 AS CHARACTER NO-UNDO FORM "x(3)":U.
FORM HEADER "This is a form, not format":U.

/* "PROMPT" versus "PROMPT-FOR" */
FOR EACH customer WHERE customer.custnum<1 NO-LOCK:
    COLOR /* DISPLAY NORMAL */ PROMPT INPUT customer.name.
END.

/* ignore methods and properties in com objects */
DEFINE VARIABLE chAbbrevkwd AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hbAbbrevkwd AS HANDLE NO-UNDO.
chAbbrevkwd:BGCOL = 13.  /* expect no warning */
hbAbbrevkwd:BGCOL = 13. /* expect warning */

/* "EXCLUSIVE" is a valid keyword, but should be "EXCLUSIVE-LOCK"  */
for each customer exclusive
  where customer.NAME = "jurjen":U :
END.



