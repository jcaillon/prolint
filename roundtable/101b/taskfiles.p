/* --------------------------------------------------------------------------
   file    : prolint/roundtable/101b/taskfiles.p
   by      : Jurjen Dijkstra
             with lots of help, from Gerry Winning and others
   purpose : lint all sourcefiles in the current Roundtable task.
             Actually this procedure doesn't run Prolint, it just finds and returns
             the list of files belonging to the Roundtable task.
   note    : calling program must check if database rtb is connected
   -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER hCaller            AS HANDLE NO-UNDO.  /* handle of calling program, like selectfiles.w */

DEFINE VARIABLE vFullpathname             AS CHARACTER NO-UNDO.
DEFINE VARIABLE giRTBCurrentTaskNum       AS INTEGER     NO-UNDO.
DEFINE VARIABLE gcRtbCurrentWorkspace     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcRtbCurrentWorkspacePath AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcRtbCurrentWorkspaceRoot AS CHARACTER   NO-UNDO.

DEFINE BUFFER Brtb_object FOR rtb.rtb_object.
DEFINE BUFFER Brtb_ver    FOR rtb.rtb_ver.

/* perhaps you should first check if the Roundtable desktop is running,
   but I hope that Grtb-task-num simply contains garbage if it isn't running */

  /* Get curent workdpace details */
  PUBLISH "evRtbGetCurrentWorkspace":U (OUTPUT gcRtbCurrentWorkspace).

  /* Get current task details */
  PUBLISH "evRtbGetCurrentTask":U (OUTPUT giRTBCurrentTaskNum).

FOR EACH Brtb_ver NO-LOCK WHERE Brtb_ver.task-num = giRTBCurrentTaskNum : 
        
    /* get the name of the object and its fully qualified pathname.
       well, a relative pathname will also be good enough, as long as Progress can find it */
       
    FIND Brtb_object NO-LOCK 
                        WHERE Brtb_object.wspace-id = gcRtbCurrentWorkspace
                          AND Brtb_object.obj-type  = "PCODE":U
                          AND Brtb_object.object    = Brtb_ver.object
                        NO-ERROR.
    IF AVAILABLE Brtb_object THEN DO:
        ASSIGN vFullpathname = DYNAMIC-FUNCTION("fnRtbGetLintFile":U, 
                                       ROWID(Brtb_object)).
        RUN AddFile IN hCaller (vFullpathname) NO-ERROR.
    END.
                       
    /* if the object is a 4GL includefile, it might be interesting to also 
       run AddFile for each of the compilation-units where the includefile is used */
    
END. 

