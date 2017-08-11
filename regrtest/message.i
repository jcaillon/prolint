/* file  message.i : test-cases for rule "message"
   message .. view-as alert-box should only be used in debug mode.
   The challenge here is to suppress AB-generated messages */


MESSAGE "testing1":U VIEW-AS ALERT-BOX.

MESSAGE "testing2":U.

/* this is a typical message created by AB */   
MESSAGE "message.wrx":U SKIP(1)
        "The binary control file could not be found. The controls cannot be loaded."
        VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".


/* see if messages in ADM includes are suppressed: */
&Scoped-define FRAME-NAME MESSAGEFRAME
DEFINE FRAME MESSAGEFRAME
     "Text 1":U VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.95 COL 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 66 BY 12.48.
{src/adm/template/dialogmn.i}
                                    

