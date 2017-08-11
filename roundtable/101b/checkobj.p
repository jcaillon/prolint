/* ============================================================================
   file    : prolint/roundtable/101b/checkobj.p
   by      : Jurjen Dijkstra
   purpose : run check-in validation, without actually checking-in the object.
   
             Thomas Hansen, appSolutions 02/2008:
             Updated code to use RTB event model and variables from RTB 10.1B
    -------------------------------------------------------------------------

    Copyright (C) 2003 Jurjen Dijkstra

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
   ============================================================================ */
   DEFINE VARIABLE rRtbVer                AS ROWID NO-UNDO.
   DEFINE VARIABLE gcRtbCurrentWorkspace  AS CHARACTER  NO-UNDO.
   
   DEFINE BUFFER Brtb_object     FOR rtb.rtb_object.
   DEFINE BUFFER Brtb_ver        FOR rtb.rtb_ver.
   DEFINE VARIABLE p_ok          AS LOGICAL NO-UNDO.

   /* Get he current workspace and current object verison record */
   PUBLISH "evRtbGetCurrentWorkspace":U (OUTPUT gcRtbCurrentWorkspace).
   PUBLISH "evRtbGetCurrentVersionRowid":U (OUTPUT rRtbVer).

   FIND Brtb_ver NO-LOCK WHERE ROWID(Brtb_ver) = rRtbVer NO-ERROR.
   
   IF AVAILABLE Brtb_ver THEN 
   DO:
        FIND Brtb_object NO-LOCK
                            WHERE Brtb_object.wspace-id = gcRtbCurrentWorkspace
                              AND Brtb_object.obj-type  = "PCODE":U
                              AND Brtb_object.object    = Brtb_ver.object
                            NO-ERROR.
        IF NOT AVAILABLE Brtb_object THEN
            MESSAGE "Brtb_object not found" VIEW-AS ALERT-BOX.
        ELSE
            RUN prolint/roundtable/101b/checkin-event.p (YES, ?, ROWID(Brtb_object), OUTPUT p_ok).
   END.

        
