/* blocklabel.i : find LEAVE/NEXT statements where no label is specified */
   
block_nolabel:           
FOR EACH customer WHERE customer.balance >50 NO-LOCK:
   IF customer.balance > 100 THEN 
      LEAVE.
END.

block_nolabel_2:
FOR EACH customer WHERE customer.balance >50 NO-LOCK:
   IF customer.balance > 100 THEN 
      LEAVE block_nolabel_2.
END.

block_nolabel_2:
FOR EACH customer WHERE customer.balance>50 NO-LOCK:
   IF customer.balance < 100 THEN 
      NEXT.
END.

block_nolabel_3:
FOR EACH customer WHERE customer.balance>50 NO-LOCK:
   IF customer.balance < 100 THEN 
      NEXT block_nolabel_3.
END.

block_nolabel_4:
FOR EACH customer WHERE customer.balance>50 NO-LOCK:
   {&_proparse_ prolint-nowarn(blocklabel)}
   IF customer.balance < 100 THEN 
      NEXT.
END.
                           
/* LEAVE is not always a statement, it can also be an event: */
DEFINE VARIABLE hwBlocklabel AS WIDGET-HANDLE NO-UNDO.                   
ON LEAVE OF hwBlocklabel DO:
END.               