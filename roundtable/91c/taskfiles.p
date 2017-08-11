/* --------------------------------------------------------------------------
   file    : prolint/roundtable/91c/taskfiles.p
   by      : Jurjen Dijkstra
             with lots of help, from Gerry Winning and others
   purpose : lint all sourcefiles in the current Roundtable task.
             Actually this procedure doesn't run Prolint, it just finds and returns
             the list of files belonging to the Roundtable task.
   note    : calling program must check if database rtb is connected
   -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER hCaller AS HANDLE NO-UNDO.  /* handle of calling program, like selectfiles.w */

{rtb/g/rtbglobl.i}  
     
DEFINE VARIABLE vFullpathname AS CHARACTER NO-UNDO.

/* perhaps you should first check if the Roundtable desktop is running,
   but I hope that Grtb-task-num simply contains garbage if it isn't running */

FOR EACH rtb.rtb_ver NO-LOCK WHERE rtb.rtb_ver.task-num = Grtb-task-num : 
        
    /* get the name of the object and its fully qualified pathname.
       well, a relative pathname will also be good enough, as long as Progress can find it */
       
    FIND rtb.rtb_object NO-LOCK 
                        WHERE rtb.rtb_object.wspace-id = Grtb-wspace-id
                          AND rtb.rtb_object.obj-type  = "PCODE":U
                          AND rtb.rtb_object.object    = rtb.rtb_ver.object
                        NO-ERROR.
    IF AVAILABLE rtb.rtb_object THEN DO:
        ASSIGN vFullpathname = DYNAMIC-FUNCTION("fnRtbGetLintFile":U, 
                                       RECID(rtb.rtb_object)).
        RUN AddFile IN hCaller (vFullpathname) NO-ERROR.
    END.
                       
    /* if the object is a 4GL includefile, it might be interesting to also 
       run AddFile for each of the compilation-units where the includefile is used */
    
END. 

