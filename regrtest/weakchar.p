/* tests for the weakchar rule */

DEFINE VARIABLE a AS CHARACTER NO-UNDO.

IF a="" THEN .

IF a<>"" THEN.

IF a>"" THEN .

IF a<>"" AND a<>? THEN .

IF NOT (a="" OR a=?) THEN .

IF NOT (a LE "") THEN.


