/* testcases for rule colon-t */

DEFINE VARIABLE colont AS CHARACTER NO-UNDO.

colont = "this is a ":T.
colont = colont + " test ":T.
colont = colont + ". ":T.


