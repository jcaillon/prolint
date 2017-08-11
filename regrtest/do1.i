/* noeffect.i:  testcases for rule "do1" */

define variable do1_1 as integer no-undo.
define variable do1_2 as integer no-undo.

if true then
   do1_1 = 5.

if true then
   do1_1 = 5.
else
   do1_1 = 7.

if true then do:
   do1_1 = 5.
end.

if true then
   do1_1 = 5.
else do:
   do1_1 = 7.
end.

if true then do:
   do1_1 = 5.
   do1_2 = 8.
end.

if true then
   do1_1 = 5.
else do:
   do1_1 = 7.
   do1_2 = 8.
end.

if true then do do1_1=1 to 5:
   do1_2 = 8.
end.

if true then
   do1_1 = 5.
else do do1_2=3 to 7:
   do1_1 = 7.
end.

/* suppress warning if the one statement is an IF statement */
if true then do:
   if false then
      do1_1 = 2.
end.
else do:
   if false then
      do1_1 = 2.
   else do:
      do1_2 = do1_1 + 7.
   end.
end.

do:
  do1_1 = 8.
end.

