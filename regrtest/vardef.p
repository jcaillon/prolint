/*
A grabbag of variable definitions
*/

def input  parameter InputHandle as handle no-undo.
def param buffer bCustomer for Customer.

def var Name like Customer.Name no-undo.

message "Question"
    view-as alert-box info buttons ok-cancel update answer as log.

procedure proc1:
/*
Purpose : 
*/
    def input  parameter MyChar as char no-undo.
    def var currentFocus as widget no-undo.

end procedure. /* proc1 */

function func1 returns logical
    (input funcparchar as char,
     buffer bfCustomer for Customer,
     output errmsg as char).
end function.
