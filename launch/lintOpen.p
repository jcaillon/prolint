/* -------------------------------------------------------------
   file    : prolint/launch/lintCurrent.p
   by      : Martijn Voncken 
   purpose : Lints all open files in UIB. (no procedure editor files)
   ------------------------------------------------------------- */
   
DEF VAR cInfo AS CHAR NO-UNDO.

RUN adeuib/_uibinfo.p(?,"SESSION","PROCEDURES RETURN NAME",output cInfo).

RUN prolint/launch/lintfileList.p(cInfo).
