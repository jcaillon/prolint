/* strattrib.i:  testcases for rule "strattrib"
   warn for string literals with string attributes other than :U and :T  */
   
DISPLAY "".   /* an empty string is always untranslatable, prolint doesnt need to warn */
DISPLAY "hello".
DISPLAY "hello":U.
DISPLAY "hello":T.
DISPLAY "hello":U5.
DISPLAY "hello":T5.
DISPLAY 'Hello prolint user'.                  
            
{&_proparse_ prolint-nowarn(strattrib)}
DISPLAY SUBSTITUTE("&1 &2 &3", "hello", "prolint", "user").
DISPLAY SUBSTITUTE("&1 &2 &3", "hello", "prolint", "user").
    
/* see how line numbers affect pragma's. 
   A pragma is supposed to suppress warnings for the statement that begins on the next line.
   a blanc line directly followng the pragma is ignored, as you can see */    
{&_proparse_ prolint-nowarn(strattrib)}

DISPLAY "hello" + 'prolint' + "user".

{&_proparse_ prolint-nowarn(strattrib)}
DISPLAY "hello" + 'prolint' + "user".

DISPLAY "hello" + 'prolint' + "user".

/* problem: (string + string) is a subnode, and it's not on 
   the same line where the statement begins */
{&_proparse_ prolint-nowarn(strattrib)}
DISPLAY "hello" + 
        ('prolint' + "user").
   
/* variant of the same problem */        
{&_proparse_ prolint-nowarn(strattrib)}
DISPLAY "hello" + 
        'prolint' + "user".
        
/* another variant of the same problem */        
{&_proparse_ prolint-nowarn(strattrib)}
DISPLAY "hello" + 
        'prolint' + 
        "user".

{&_proparse_ prolint-nowarn(strattrib)}
DISPLAY "hello":U + 'prolint' + "user":T.
DISPLAY "hello":U + 'prolint' + "user":T.


{&_proparse_ prolint-nowarn(strattrib)}
MESSAGE "msgtxt"
        VIEW-AS ALERT-BOX INFORMATION 
        TITLE ("msg" + "title").
        
MESSAGE "msgtxt":U 
        VIEW-AS ALERT-BOX INFORMATION 
        TITLE "title".        
        
{&_proparse_ prolint-nowarn(strattrib)}
MESSAGE "msgtxt"
        VIEW-AS ALERT-BOX INFORMATION 
        TITLE "title".                
        
        