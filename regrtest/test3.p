
/* some tests for rule "unquoted" */

DEFINE VARIABLE RADIO-SET-2 AS INTEGER
VIEW-AS RADIO-SET VERTICAL
RADIO-BUTTONS w1, 0, w2, 1, "w3", 2
SIZE 19 BY 2 NO-UNDO.

DEFINE VARIABLE myfile AS CHARACTER NO-UNDO.
OUTPUT TO myfile.
OUTPUT CLOSE.
OUTPUT TO VALUE(myfile).
OUTPUT CLOSE.
OUTPUT TO "myfile".
OUTPUT CLOSE.

/* run statements use unquoted filenames, but should not raise unquoted warning */
RUN test2.p.
ON CLOSE OF THIS-PROCEDURE PERSISTENT RUN test2.p.


