/* ============================================================================
   file    : protools/roundtable/91c/checkobj.p
   by      : Jurjen Dijkstra
   purpose : run check-in validation, without actually checking-in the object.
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
              
{rtb/g/rtbglobl.i}                         

DEFINE VARIABLE ver_recid     AS RECID NO-UNDO.
DEFINE BUFFER brtb_object     FOR rtb.rtb_object.
DEFINE BUFFER brtb_ver        FOR rtb.rtb_ver.
DEFINE VARIABLE p_ok          AS LOGICAL NO-UNDO.

IF VALID-HANDLE(Grtb-proc-handle) THEN 
   RUN choose_object IN Grtb-proc-handle ("dummy window title", 
                                          FALSE, /* not interactive */
                                          OUTPUT ver_recid).

FIND brtb_ver NO-LOCK WHERE RECID(brtb_ver)=ver_recid NO-ERROR.
IF AVAILABLE brtb_ver THEN DO:
     FIND brtb_object NO-LOCK
                         WHERE brtb_object.wspace-id = Grtb-wspace-id
                           AND brtb_object.obj-type  = "PCODE":U
                           AND brtb_object.object    = brtb_ver.object
                         NO-ERROR.
     IF NOT AVAILABLE brtb_object THEN
         MESSAGE "brtb_object not found" VIEW-AS ALERT-BOX.
     ELSE
         RUN prolint/roundtable/91c/checkin-event.p (YES, ?, RECID(brtb_object), OUTPUT p_ok).
END.

        
