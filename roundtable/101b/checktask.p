/*  ========================================================================
    file    : prolint/roundtable/101b/checktask.p
    purpose : run check-in validation, without actually completing the task
    
             Thomas Hansen, appSolutions 02/2008:
             Updated code to use RTB event model and variables from RTB 10.1B    
    ------------------------------------------------------------------------

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
    ========================================================================= */
    
    DEFINE VARIABLE giRTBCurrentTaskNum       AS INTEGER     NO-UNDO.
    
    PUBLISH "evRtbGetCurrentTask":U (OUTPUT giRTBCurrentTaskNum).
    
   {&_proparse_ prolint-nowarn(varusage)}
   DEFINE VARIABLE p_ok AS LOGICAL NO-UNDO.

   RUN prolint/roundtable/101b/checkin-event.p (YES, giRTBCurrentTaskNum, ?, OUTPUT p_ok).

                   
