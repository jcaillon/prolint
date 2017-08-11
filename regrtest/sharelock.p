FUNCTION GetBinaryFileFromResponse RETURNS LONGCHAR PRIVATE
        (  ):
/*------------------------------------------------------------------------------
    Purpose: Returns a base64 encoded longchar of a binary file that is held
    within the HTTP response from the Actuate server between a MIME boundary
    Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE mBinaryFile                     AS MEMPTR     NO-UNDO.
    DEFINE VARIABLE iStartByte                      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iEndByte                        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE gmResponse                      AS MEMPTR     NO-UNDO.
        
    COPY-LOB FROM gmResponse STARTING AT iStartByte FOR (iEndByte - iStartByte) TO mBinaryFile.
        
    RETURN BASE64-ENCODE(mBinaryFile).

END FUNCTION.
