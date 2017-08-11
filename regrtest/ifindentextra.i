IF FALSE THEN MESSAGE "firstline".
RETURN.

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
IF TRUE THEN FALSE.

DEF VAR i AS INTEGER.

  IF FALSE THEN
     IF TRUE THEN
        IF FALSE THEN
           MESSAGE "hi".

/* want to locate the second and third IF too: */
IF FALSE THEN IF TRUE THEN IF FALSE THEN MESSAGE "yo".
         
   CASE i :
       WHEN 1 THEN IF i=1 THEN MESSAGE "thats right".
                          ELSE MESSAGE "duh".
   END.

      {prolint/regrtest/nocomment.i}
      IF FALSE THEN MESSAGE "gotcha!".

  &IF DEFINED(anything)=0 &THEN IF TRUE THEN i=1. &ENDIF.

  &IF DEFINED(anything)<>0 &THEN IF TRUE THEN i=1. &ELSE IF FALSE THEN i=2. &ENDIF.

  {&anything}  IF TRUE THEN i=2.

  {&anything else}
 IF TRUE THEN i=2.

{&anything again}
IF TRUE THEN i=2.

  /* yada yada blah blah blah */ IF FALSE THEN i=5.

  IF /* lets add some more confusing comments */ TRUE /* yeah */ THEN /* hihi */ IF TRUE THEN i=1.


  /* these are too complicated for me and return foundNewline=FALSE.
     problem is: prevSibling contains a nodehead, not all its children may be on the
     same line  */
  IF 2 + 3 = 5 THEN IF FALSE THEN i = 8.
  IF (2 + 3 = 5) THEN IF FALSE THEN i = 8.
  IF 2.1 + 3.2 = 5.4 THEN IF i=2 THEN IF TRUE=FALSE THEN i=9.
  IF 9
     +
     8
     =
     0 THEN IF FALSE THEN i = 8.
  IF 9
     +
     8
     = 0 THEN IF FALSE THEN i = 8.


