/* -------------------------------------------------------------
   file    : prolint/launch/lintCurrent.p
   by      : Martijn Voncken 
   purpose : Lints current file in UIB. (no procedure editor files)
   ------------------------------------------------------------- */
DEF VAR cInfo AS CHAR NO-UNDO.

RUN adeuib/_uibinfo.p(?,?,"FILE-NAME",output cInfo).

RUN prolint/launch/lintfileList.p(cInfo).
