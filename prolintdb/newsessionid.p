/* =======================================================================================
    file    : prolint/prolintdb/newsessionid.p
    Purpose : to get a sequence value, and publish it to propsuper as the new session ID.
    by      : Glen West
    -----------------------------------------------------------------

    Copyright (C) 2007 Glen West

    This file is part of Prolint.

    Prolint is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    Prolint is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Prolint; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   ======================================================================================= */


/* ***************************  Definitions  ************************** */
DEFINE VARIABLE NewSessionID AS INTEGER    NO-UNDO INITIAL ?.
DEFINE VARIABLE PropsRunning AS LOGICAL    NO-UNDO INITIAL FALSE.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
IF NOT CONNECTED ("prolintdb") THEN DO:
    /* should be impossible AS ALL callers would have already CONNECTED it */
    MESSAGE "Database prolintdb is not connected." SKIP
            "See help-topic prolintdb to learn about this feature."
            VIEW-AS ALERT-BOX.
    RETURN ERROR "No Database":U.
END.

PUBLISH "IsProlintPropertiesRunning" (OUTPUT PropsRunning).
IF NOT PropsRunning THEN DO:
    MESSAGE "The Properties Persistent Service is not running.  This program may"
            "have been called incorrectly.  This procedure cannot function without"
            "the Properties Persistent Service (core/propsuper.p)."
            VIEW-AS ALERT-BOX.
    RETURN ERROR "No Properties Service":U.
END.

ASSIGN NewSessionID = NEXT-VALUE(sessionid,prolintdb) NO-ERROR.

IF NewSessionID <> ? THEN DO:
    RUN SetProlintProperty ("SessionID", STRING(NewSessionID,">>>>>>>>>9")).
END.
