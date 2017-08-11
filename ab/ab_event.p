&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/* =============================================================================
   file    : prolint/ab/ab_event.p
   purpose : Integration between Prolint and AppBuilder/UIB
   -----------------------------------------------------------------------------
   Copyright (C) 2005 Carl Verbiest

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
   
/* ***************************  Definitions  ************************** */

def var vABWindow as widget-handle no-undo.
def var vABMenu as widget-handle no-undo.
def var vABProcedure as handle no-undo.

/*

{ src/adeuib/triggers.i }
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ABLint_Current) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ABLint_Current Procedure 
PROCEDURE ABLint_Current :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var vProcContext as char no-undo init ?.
    def var vFileName as char no-undo init ?.
    run adeuib/_uibinfo.p(?, ?, "procedure", output vProcContext).
    run adeuib/_uibinfo.p(vProcContext, ?, "file-name", output vFileName).
    if vFileName <> ? then run prolint/ab/ablint.p(vFileName).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ABLint_ExpandMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ABLint_ExpandMenu Procedure 
PROCEDURE ABLint_ExpandMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var vhMenu as widget-handle no-undo.
    def var vhMenuItem as widget-handle no-undo.
    def var count as int no-undo.
           
    assign vhMenu = vABMenu:first-child.
    do while valid-handle(vhMenu) and replace(vhMenu:label, "&", "") <> "Compile":
        vhMenu = vhMenu:next-sibling.
    end.    
    if valid-handle(vhMenu)
    then do:
        create menu-item vhMenuItem
            assign
                label = "Pro&lint ..."
                parent = vhMenu
            triggers:
                on choose persistent run ABLint_Current in this-procedure.
            end triggers.
    end.
    do while valid-handle(vhMenu) and replace(vhMenu:label, "&", "") <> "Tools":
        vhMenu = vhMenu:next-sibling.
    end.    
    if valid-handle(vhMenu)
    then do:
    end.
    do while valid-handle(vhMenu) and replace(vhMenu:label, "&", "") <> "Options":
        vhMenu = vhMenu:next-sibling.
    end.    
    if valid-handle(vhMenu)
    then do:
        create menu-item vhMenuItem
            assign
                label = "Pro&lint ..."
                parent = vhMenu
            triggers:
                on choose persistent run ABLint_Options in this-procedure.
            end triggers.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ABLint_Options) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ABLint_Options Procedure 
PROCEDURE ABLint_Options :
/*------------------------------------------------------------------------------
  Purpose:     Start Prolint config
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* lintcfg has a conditional parameter, try it both with & without */
    RUN prolint/core/lintcfg.w PERSISTENT NO-ERROR.
    IF ERROR-STATUS:ERROR AND ERROR-STATUS:GET-NUMBER(1) = 3234 /* mismatched number of parameters */
    THEN RUN prolint/core/lintcfg.w PERSISTENT ("AppBuilder") NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ABLint_Startup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ABLint_Startup Procedure 
PROCEDURE ABLint_Startup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p_product  AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER p_event    AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER p_context  AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER p_other    AS CHAR    NO-UNDO.
DEFINE OUTPUT PARAMETER p_ok       AS LOGICAL NO-UNDO INITIAL TRUE.

    def var proclist as char no-undo.
    def var count as int no-undo.
    def var UIBWinX as int no-undo.
    def var UIBWinY as int no-undo.
    def var WZ-CHAR as char no-undo.
    def var WIND-HND as handle no-undo.
    def var PROC-HND as handle no-undo.

    assign 
        WIND-HND = widget-handle(p_Other)
        PROC-HND = widget-handle(p_Context)
        no-error.
        
    if valid-handle(vABWindow) then return. /* already running */
    assign 
        vABWindow = widget-handle(p_Other)
        vABProcedure = widget-handle(p_Context).
    if valid-handle(vABWindow)
    then do:
        assign
            vABMenu = vABWindow:menu-bar
            vABWindow:title = substitute("&1+&2", vABWindow:title, "Prolint":U).
        run ABLint_ExpandMenu in this-procedure.
    end.           

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AppBuilderEvent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AppBuilderEvent Procedure 
PROCEDURE AppBuilderEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p_product  AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER p_event    AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER p_context  AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER p_other    AS CHAR    NO-UNDO.
DEFINE OUTPUT PARAMETER p_ok       AS LOGICAL NO-UNDO INITIAL TRUE.

CASE p_event:
    WHEN "Startup" THEN RUN ABLint_Startup
        (p_product, p_event, p_context, p_other, output p_ok).
END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

