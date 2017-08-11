/* file sortaccess.i : test-cases for rule "sortaccess"
   looks for SORT-ACCESS in the XREF file */


FOR EACH customer NO-LOCK 
                  WHERE customer.cust > 2
                  BY customer.comments :
END.

                  
         
             