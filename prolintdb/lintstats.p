/*------------------------------------------------------------------------
   File        : lintstats.p
   Purpose     : Show and compare lint statistics for selected sessions

   Syntax      :

   Description : Statistical Analysis of Lint Warnings by Session

   Author(s)   : westgl
   Created     : Mon Jul 16 11:31:11 EDT 2007
   Notes       : invoked by prolint/outputhandlers/logwin.w
 ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hTTStats AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery   AS HANDLE NO-UNDO.
DEFINE VARIABLE hBrowse  AS HANDLE NO-UNDO.
DEFINE QUERY qSessions FOR lint_session SCROLLING.
DEFINE BROWSE brSessions QUERY qSessions
 DISPLAY lint_session.sessionid
         lint_session.sessionuser
         lint_session.sessionname
         lint_session.sessionStartDate
         STRING(lint_session.sessionStartTime,"HH:MM:SS") FORMAT "x(9)" COLUMN-LABEL "Time"
         lint_session.sessionLastDate COLUMN-LABEL "Last Use!Date"
         STRING(lint_session.sessionLastTime,"HH:MM:SS") FORMAT "x(9)" COLUMN-LABEL "Time"
         lint_session.sessionparms
         WITH SEPARATORS 9 DOWN.
DEFINE VARIABLE cUser     AS CHARACTER  NO-UNDO LABEL "User (bg)"  FORMAT "x(12)".
DEFINE VARIABLE dtStart   AS DATE       NO-UNDO LABEL "Start (>=)".
DEFINE VARIABLE cName     AS CHARACTER  NO-UNDO LABEL "Name (ma)"  FORMAT "x(40)".
DEFINE VARIABLE cBaseDt   AS CHARACTER  NO-UNDO LABEL "Started"    FORMAT "x(20)".
DEFINE VARIABLE cCurrDt   AS CHARACTER  NO-UNDO                    FORMAT "x(20)".
DEFINE VARIABLE cCompUnit AS CHARACTER  NO-UNDO LABEL "Prog (ma)".
DEFINE VARIABLE cRule     AS CHARACTER  NO-UNDO LABEL "Rule (bg)".
DEFINE BUTTON btn_filtSes LABEL "Filter"  SIZE-CHARS 15 BY 1.
DEFINE BUTTON btn_filtRul LABEL "Filter"  SIZE-CHARS 15 BY 1.
DEFINE BUTTON btn_Analyze LABEL "Analyze" SIZE-CHARS 15 BY 1.
DEFINE BUTTON btn_base    LABEL "Base"    SIZE-CHARS 15 BY 1.
DEFINE BUTTON btn_curr    LABEL "Curr"    SIZE-CHARS 15 BY 1.
DEFINE BUTTON btn_help    LABEL "Help"    SIZE-CHARS 15 BY 1.
DEFINE BUTTON btn_cancel  LABEL "Cancel"  SIZE-CHARS 15 BY 1 AUTO-ENDKEY.

DEFINE TEMP-TABLE ttStats NO-UNDO
 FIELD compunit  AS CHARACTER COLUMN-LABEL "Program" FORMAT "x(60)"
 FIELD rulename  AS CHARACTER COLUMN-LABEL "Rule" FORMAT "x(15)"
 FIELD basecount AS INTEGER   COLUMN-LABEL "Base"
 FIELD currcount AS INTEGER   COLUMN-LABEL "Curr"
 FIELD rulesev   AS INTEGER   COLUMN-LABEL "Sev"  FORMAT "9"
 FIELD diffcount AS INTEGER   COLUMN-LABEL "Diff"
 INDEX KEY AS PRIMARY UNIQUE
   compunit
   rulename
 INDEX sev
   compunit
   rulesev  DESCENDING
   rulename.

DEFINE QUERY qStats FOR ttStats SCROLLING.
DEFINE BROWSE brStats QUERY qStats
 DISPLAY compunit WIDTH-CHARS 30
         rulename rulesev diffcount basecount currcount
         WITH SEPARATORS 9 DOWN .

DEFINE BUFFER bSession FOR lint_session.
DEFINE BUFFER cSession FOR lint_session.

DEFINE FRAME Frame1
 cUser       AT ROW 1  COLUMN 15 COLON-ALIGN VIEW-AS FILL-IN SIZE-CHARS 12 by 1
 dtStart     AT ROW 1  COLUMN 45 COLON-ALIGN
 cName       AT ROW 1  COLUMN 75 COLON-ALIGN VIEW-AS FILL-IN SIZE-CHARS 12 by 1
 btn_FiltSes AT ROW 1  COLUMN 103
 brSessions  AT ROW 2  COLUMN 1
 cCompUnit   AT ROW 11 COLUMN 15 COLON-ALIGN VIEW-AS FILL-IN SIZE-CHARS 12 by 1
 cRule       AT ROW 11 COLUMN 75 COLON-ALIGN VIEW-AS FILL-IN SIZE-CHARS 12 by 1
 btn_FiltRul AT ROW 11 COLUMN 103
 brStats     AT ROW 12 COLUMN 1
 btn_base    AT ROW 12 COLUMN 103
 btn_curr    AT ROW 12 COLUMN 130
 bSession.sessionid    AT ROW 13 COLUMN 101 COLON-ALIGN VIEW-AS TEXT
 cSession.sessionid    AT ROW 13 COLUMN 130    NO-LABEL VIEW-AS TEXT
 bSession.sessionname  AT ROW 14 COLUMN 101 COLON-ALIGN VIEW-AS TEXT FORMAT "x(15)" LABEL "Name"
 cSession.sessionname  AT ROW 14 COLUMN 130   NO-LABEL VIEW-AS TEXT FORMAT "x(15)"
 bSession.sessionuser  AT ROW 15 COLUMN 101 COLON-ALIGN VIEW-AS TEXT
 cSession.sessionuser  AT ROW 15 COLUMN 130   NO-LABEL VIEW-AS TEXT
 cBaseDt               AT ROW 16 COLUMN 101 COLON-ALIGN VIEW-AS TEXT
 cCurrDt               AT ROW 16 COLUMN 130   NO-LABEL VIEW-AS TEXT
 btn_analyze           AT ROW 19 COLUMN  95
 btn_cancel            AT ROW 19 COLUMN 115
 btn_help              AT ROW 19 COLUMN 135
 WITH OVERLAY THREE-D SIDE-LABELS VIEW-AS DIALOG-BOX WIDTH 155.

/* UI Triggers */
ON CHOOSE OF btn_help IN FRAME frame1 DO:
   RUN prolint/core/openhtml.p( "http://oehive.org/node/945":U ).
END.

ON CHOOSE OF btn_FiltSes IN FRAME frame1 DO:
 DO WITH FRAME frame1:
   ASSIGN cUser dtStart cName.
   RUN openqSessions IN THIS-PROCEDURE.
 END.
END.

ON CHOOSE OF btn_base IN FRAME frame1 DO:
   CLOSE QUERY qStats.
   IF NOT AVAILABLE lint_session THEN DO:
      MESSAGE "No Session Record Selected, Choose a row and try again." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FIND bSession NO-LOCK WHERE ROWID(bSession) = ROWID(lint_session) NO-ERROR.
   IF AVAILABLE bSession THEN DO:
      ASSIGN cBaseDt = STRING(lint_session.sessionStartDate) + " "
                     + STRING(lint_session.sessionStartTime,"HH:MM:SS").
      DISPLAY bSession.SessionID bSession.sessionName bSession.SessionUser cBaseDt
              WITH FRAME frame1.
   END.
END.

ON CHOOSE OF btn_curr IN FRAME frame1 DO:
   CLOSE QUERY qStats.
   IF NOT AVAILABLE lint_session THEN DO:
      MESSAGE "No Session Record Selected, Choose a row and try again." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FIND cSession NO-LOCK WHERE ROWID(cSession) = ROWID(lint_session) NO-ERROR.
   IF AVAILABLE cSession THEN DO:
      ASSIGN cCurrDt = STRING(lint_session.sessionStartDate) + " "
                     + STRING(lint_session.sessionStartTime,"HH:MM:SS").
      DISPLAY cSession.SessionID cSession.sessionName cSession.SessionUser cCurrDt
              WITH FRAME frame1.
   END.
END.

ON CHOOSE OF btn_analyze IN FRAME frame1
OR CHOOSE OF btn_filtRul IN FRAME frame1 DO:
  DO WITH FRAME frame1:
     ASSIGN cCompUnit cRule.
  END.
  CLOSE QUERY qStats.
  IF NOT AVAILABLE bSession THEN DO:
     MESSAGE 'No BASE for comparison.  Select a session in the browse and click the Base button.' VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  IF NOT AVAILABLE cSession THEN DO:
     MESSAGE 'No CURRENT session for comparison.  Select a session in the browse and click the Curr button.' VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  RUN GenStats IN THIS-PROCEDURE (bSession.sessionID, cSession.SessionID).
  RUN openqStats IN THIS-PROCEDURE.
END.



/* ***************************  Main Block  *************************** */
ASSIGN cUser   = USERID("prolintdb")
      dtStart = TODAY.

DISPLAY cUser dtStart cName WITH FRAME frame1.

ENABLE ALL WITH FRAME frame1.
ASSIGN BROWSE brStats:COLUMN-RESIZABLE = TRUE.
IF NOT THIS-PROCEDURE:PERSISTENT THEN WAIT-FOR CLOSE OF THIS-PROCEDURE.

PROCEDURE openqSessions:
   CLOSE QUERY qSessions.
   OPEN QUERY qSessions FOR EACH lint_session NO-LOCK
        WHERE  lint_session.sessionuser BEGINS cUser
          AND  lint_session.sessionname MATCHES "*" + cName + "*"
          AND (lint_session.sessionStartDate >= dtStart
           OR dtStart = ?)
           BY lint_session.sessionid DESCENDING.
END PROCEDURE. /* openqSessions */

PROCEDURE openqStats:
   OPEN QUERY qStats FOR EACH ttStats
                        WHERE ttStats.compunit MATCHES ("*" + cCompUnit + "*")
                          AND ttStats.rulename BEGINS cRule
                         BY ttStats.compunit
                         BY ttStats.rulesev DESCENDING
                         BY ttStats.rulename.
END.

PROCEDURE GenStats:
   DEFINE INPUT PARAMETER bSessionID AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER cSessionID AS INTEGER NO-UNDO.

   DEFINE BUFFER lint_warning FOR prolintdb.lint_warning.

   EMPTY TEMP-TABLE ttStats.

   /* start with base */
   FOR EACH prolintdb.lint_warning NO-LOCK
      WHERE prolintdb.lint_warning.sessionid    = bSessionID
      BY prolintdb.lint_warning.ruleid:
          RUN createTT IN THIS-PROCEDURE (BUFFER prolintdb.lint_warning,"b").
   END. /* each lint_warning. */
   FOR EACH prolintdb.lint_warning NO-LOCK
      WHERE prolintdb.lint_warning.sessionid    = cSessionID
      BY prolintdb.lint_warning.ruleid:
          RUN createTT IN THIS-PROCEDURE (BUFFER prolintdb.lint_warning,"c").
   END. /* each lint_warning. */

   /* set the 'diff' column */
   FOR EACH ttStats:
       ASSIGN ttStats.diffcount = ttStats.currcount - ttStats.basecount.
   END.
END PROCEDURE. /* GenStats */

PROCEDURE createTT:
   DEFINE PARAMETER BUFFER lintWarn FOR prolintdb.lint_warning.
   DEFINE INPUT PARAMETER pType AS CHARACTER NO-UNDO.

   FIND ttStats WHERE ttStats.compunit = lintWarn.compunit
                  AND ttStats.rulename = lintWarn.ruleid NO-ERROR.
   IF NOT AVAILABLE ttStats THEN DO:
      CREATE ttStats.
      ASSIGN ttStats.compunit = lintWarn.compunit
             ttStats.rulename = lintWarn.ruleid.
   END.
   ASSIGN ttStats.rulesev = lintWarn.severity.
   CASE pType:
       WHEN 'b' THEN ASSIGN ttStats.basecount = ttStats.basecount + 1.
       WHEN 'c' THEN ASSIGN ttStats.currcount = ttStats.currcount + 1.
   END CASE.
END.


