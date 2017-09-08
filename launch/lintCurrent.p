/* -------------------------------------------------------------
   file    : prolint/launch/lintCurrent.p
   by      : Martijn Voncken
   purpose : Lints current file in UIB. (no procedure editor files)
   ------------------------------------------------------------- */
&IF INTEGER(ENTRY(1, PROVERSION, '.')) < 11 &THEN
    MESSAGE "FATAL ERROR - This version of ProLint can only run using Progress | OpenEdge 10.2A or higher." VIEW-AS ALERT-BOX ERROR.
    RETURN "".
&ELSE
		DEFINE VARIABLE cInfo AS CHAR NO-UNDO.

		RUN adeuib/_uibinfo.p(?,?,"FILE-NAME",output cInfo).
		RUN prolint/launch/lintfileList.p(cInfo).
&ENDIF
