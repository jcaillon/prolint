/* file i18nlength: test-cases for rule "i18nlength"
   look for LEGNTH function with only one parameter */


DEFINE VARIABLE i18nlength_1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE i18nlength_2 AS INTEGER   NO-UNDO.

{&_proparse_ prolint-nowarn(i18nlength)}
i18nlength_2 = LENGTH(i18nlength_1).

i18nlength_2 = LENGTH(i18nlength_1).
i18nlength_2 = LENGTH(i18nlength_1, "CHARACTER":U).
i18nlength_2 = LENGTH(i18nlength_1, "RAW":U).
i18nlength_2 = LENGTH(i18nlength_1, "COLUMN":U).


{&_proparse_ prolint-nowarn(i18nlength)}
i18nlength_2 =    LENGTH(i18nlength_1, "RAW":U) 
               +  LENGTH(i18nlength_1).

i18nlength_2 =    LENGTH(i18nlength_1, "RAW":U) 
               +  LENGTH(i18nlength_1).


