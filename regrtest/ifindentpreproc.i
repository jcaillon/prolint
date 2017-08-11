IF TRUE THEN DO:
  ASSIGN
          {&customer}.balance = {&invoice}.amount * ({&invoice}.adjustment +
                                                           {&customer}.balance)
          {&customer}.discount = {&invoice}.amount * {&customer}.balance
          {&invoice}.amount = {&invoice}.amount * {&customer}.balance
          {&customer}.balance = {&invoice}.amount * {&customer}.balance.
END.

/** 
- customer.balance and .discount assigns (lines 3 and 5) have a WS token 
  before their PREPROCESSTOKENs. 
- last 2 don't (lines 6 and 7) - invoice.amount & customer.balance don't 
  have any more hidden tokens before the PREPROCESSTOKEN. 
*/
