/* tests for rule "dotcomment":
   a statement beginning with a PERIOD is actually a comment. */

define variable dotcomment as decimal no-undo.

.message "test1":U view-as alert-box.

{&_proparse_ prolint-nowarn(dotcomment)}
.message "test2":U view-as alert-box.

. message "test3":U view-as alert-box.

message "test4":U view-as alert-box. .
message "test5":U view-as alert-box.

   .message "test6":U
            view-as alert-box.

   .dotcomment = dotcomment
                 + 3.14.

IF FALSE THEN
.blablabla.
   MESSAGE "Test1":U
   VIEW-AS ALERT-BOX INFORMATION.

                 

