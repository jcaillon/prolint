/* -------------------------------------------------------------
   file    : prolint/launch/lintCurrent.p
   by      : Martijn Voncken
   purpose : Lints all open files in UIB. (no procedure editor files)
   ------------------------------------------------------------- */

DEFINE VARIABLE cInfo AS CHAR NO-UNDO.


RUN adeuib/_uibinfo.p(?,"SESSION","PROCEDURES RETURN NAME", OUTPUT cInfo).
RUN prolint/launch/lintfileList.p(cInfo).
