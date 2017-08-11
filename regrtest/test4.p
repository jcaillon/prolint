

DEFINE VARIABLE longtext AS CHARACTER NO-UNDO.

ASSIGN
    longtext = "this is a test for rule maxchar. It triggers when the string is longer than 188 character because the Translation Manager cannot handle those long strings. Of course, if the string ends in a :U attribute there is not supposed to be a problem because Tranman will ignore the string"
    longtext = "this is a test for rule maxchar. It triggers when the string is longer than 188 character because the Translation Manager cannot handle those long strings. Of course, if the string ends in a :U attribute there is not supposed to be a problem because Tranman will ignore the string":U.
    


