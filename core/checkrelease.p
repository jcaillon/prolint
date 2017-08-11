/* ===========================================================================
   file    : prolint/core/checkrelease.p
   purpose : check on internet if a new Prolint release is available
   ---------------------------------------------------------------------------

   Copyright (C) 2001,2002 Jurjen Dijkstra

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
   =========================================================================== */

{prolint/core/dlc-version.i}

DEFINE VARIABLE RemoteRelease AS INTEGER NO-UNDO.
DEFINE VARIABLE LocalRelease  AS INTEGER NO-UNDO.
DEFINE VARIABLE RemoteAvail   AS LOGICAL NO-UNDO INITIAL YES.

IF NOT (OPSYS MATCHES "win*") THEN DO:
   MESSAGE "Sorry, need Windows for this feature."
           VIEW-AS ALERT-BOX.
   RETURN.
END.

RUN GetRemoteRelease.
IF NOT RemoteAvail THEN
   RETURN.

RUN GetLocalRelease.

IF RemoteRelease <= LocalRelease THEN
   MESSAGE "Your release of Prolint is up to date."
           VIEW-AS ALERT-BOX.
ELSE DO:
   MESSAGE "You don't have the latest Prolint release:" SKIP
           "release " RemoteRelease " is available,"
           "you are using release " LocalRelease SKIP(2)
           "You will now be redirected to the Prolint website..."
           VIEW-AS ALERT-BOX.
   RUN prolint/core/openhtml.p("http://oehive.org/prolint/download":U).
END.

RETURN.

/* ================ internal procs ========================== */

DEFINE VARIABLE remote_ini AS CHARACTER NO-UNDO.

PROCEDURE GetRemoteRelease :

   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.

   RUN HTTPGet.

   DO i=1 TO NUM-ENTRIES(remote_ini, CHR(13)) :
      cLine = TRIM(ENTRY(i, remote_ini, CHR(13)), CHR(10)).

      IF ENTRY(1, cLine, "=") = "prolint" THEN DO:
         RemoteRelease = INTEGER(ENTRY(2, cLine, "=")).
         RETURN.
      END.
   END.

END PROCEDURE.


PROCEDURE GetLocalRelease :

   DEFINE VARIABLE cLine  AS CHARACTER NO-UNDO.

   FILE-INFO:FILE-NAME = "prolint/core/release.ini".

   INPUT FROM VALUE(file-info:FULL-PATHNAME).
   REPEAT:
      IMPORT UNFORMATTED cLine NO-ERROR.
      IF ENTRY(1, cLine, "=") = "prolint" THEN
         LocalRelease = INTEGER(ENTRY(2, cLine, "=")).
   END.
   INPUT CLOSE.

END PROCEDURE.


/* ============ the HTTP Get part : ============================== */
/* (mostly copied from knowledgebase article 20011 and simplified) */

DEFINE VARIABLE vSocket AS HANDLE NO-UNDO.
DEFINE VARIABLE wloop   AS LOGICAL NO-UNDO.
DEFINE VARIABLE vStr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE vBuffer AS MEMPTR NO-UNDO.

PROCEDURE HTTPGet:

   DEFINE VARIABLE wstatus AS LOGICAL NO-UNDO.

   wloop = YES.
   CREATE SOCKET vSocket.
   vSocket:SET-READ-RESPONSE-PROCEDURE ("readHandler", THIS-PROCEDURE).
   wstatus = vSocket:CONNECT("-H www.oehive.org -S 80")
             NO-ERROR.
   IF wstatus = NO THEN DO:
      MESSAGE "Connection to http server is unavailable."
              "There is perhaps a firewall or proxy problem.".
      DELETE OBJECT vSocket.
      RemoteAvail = FALSE.
      RETURN.
   END.

   vStr = "GET /files/prolintrelease.txt HTTP/1.0~n~n~n".

   SET-SIZE(vBuffer) = LENGTH(vStr) + 1.
   PUT-STRING(vBuffer,1) = vStr.
   vSocket:WRITE(vBuffer, 1, LENGTH(vStr)).
   SET-SIZE(vBuffer) = 0.
   DO WHILE wloop:
      WAIT-FOR READ-RESPONSE OF vSocket.
   END.
   vSocket:DISCONNECT().
   DELETE OBJECT vSocket.

   /* remove HHTP header */
   remote_ini = SUBSTRING (remote_ini,
                           INDEX(remote_ini, CHR(13) + CHR(10) + CHR(13) + CHR(10))).
   
END PROCEDURE.


PROCEDURE readHandler:
   DEFINE VARIABLE l AS INTEGER NO-UNDO.
   DEFINE VARIABLE b AS MEMPTR NO-UNDO.

   l = vSocket:GET-BYTES-AVAILABLE().
   IF l > 0 THEN DO:
      SET-SIZE(b) = l + 1.
      vSocket:READ(b, 1, l, 1).
      remote_ini = remote_ini + GET-STRING(b,1).
      SET-SIZE(b) = 0.
      wloop = YES.
   END.
   ELSE DO:
      wloop = NO.
      vSocket:DISCONNECT().
   END.
END.


