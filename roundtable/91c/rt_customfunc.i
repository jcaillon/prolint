/* ============================================================================
   file    : prolint/roundtable/91c/rt_customfunc.i
   by      : Gerry Winning
   purpose : load Roundtable utilities for Prolint
   ============================================================================ */

  DEFINE NEW GLOBAL SHARED VARIABLE viRtbCustFuncId AS INTEGER NO-UNDO.
  IF NOT (VALID-HANDLE(vhRtbCustFunc) AND vhRtbCustFunc:UNIQUE-ID=viRtbCustFuncId) THEN
     DO:
       RUN prolint/roundtable/91c/rt_customfunc.p PERSISTENT SET vhRtbCustFunc.
       viRtbCustFuncId = vhRtbCustFunc:UNIQUE-ID.
     END.
  SESSION:ADD-SUPER-PROCEDURE(vhRtbCustFunc).

