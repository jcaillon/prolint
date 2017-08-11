/* =============================================================================
   file    : prolint/ab/custom_evnt.i
   purpose : (to be included in adecomm/_adeevnt.p)
             setup integration between Prolint and AppBuilder/UIB
   -----------------------------------------------------------------------------
   Copyright (C) 2005 Carl Verbiest
   Credits to Jurjen Dijkstra, Gerry Winning, Ildefonzo Arocha 
   for the RTB integration which served as a basis for this

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
   ========================================================================== */

DEFINE NEW GLOBAL SHARED VARIABLE vhAbCustFunc AS HANDLE NO-UNDO.
/* save search result for performance */
DEFINE NEW GLOBAL SHARED VARIABLE vProlintPath AS CHAR NO-UNDO.
&IF (DEFINED(prolint_ab_custom_evnt_i)=0) &THEN
  &GLOBAL-DEFINE prolint_ab_custom_evnt_i FOO

  IF vProlintPath = "" 
  THEN DO:
      vProlintPath = SEARCH("prolint/ab/ab_event.p").
  END.
  IF vProlintPath <> ? AND NOT VALID-HANDLE(vhAbCustFunc) 
  THEN DO:
      RUN VALUE(vProlintPath) PERSISTENT SET vhAbCustFunc.
  END.
  IF VALID-HANDLE(vhAbCustFunc) 
  THEN RUN AppBuilderEvent IN vhAbCustFunc (
    INPUT  p_product,
    INPUT  p_event,  
    INPUT  p_context,
    INPUT  p_other,  
    OUTPUT p_ok    
    ).
&ENDIF
                                   
