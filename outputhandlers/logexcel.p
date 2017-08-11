/* =======================================================================
   file    : prolint/logexcel.p
   purpose : write results (found by rules) to a Excel xml file and open it.
   by      : Carl Verbiest
    -----------------------------------------------------------------

    Copyright (C) 2001,2002 Jurjen Dijkstra

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
   ====================================================================== */
{prolint/core/dlc-version.i}

DEFINE VARIABLE logfile AS CHAR NO-UNDO INITIAL "prolint.xml":U.
DEFINE VARIABLE chExcel AS COM-HANDLE.
                                            
SUBSCRIBE TO "Prolint_InitializeResults" ANYWHERE.
SUBSCRIBE TO "Prolint_AddResult" ANYWHERE.
SUBSCRIBE TO "Prolint_FinalizeResults" ANYWHERE.
   
RETURN.

                  
PROCEDURE Prolint_InitializeResults :  
   /* purpose : start with an empty logfile. If one exists make it empty */
   DEFINE INPUT PARAMETER pClearOutput AS LOGICAL NO-UNDO.

   CREATE "Excel.Application.11":U chExcel NO-ERROR.
   IF chExcel=? THEN DO:
      MESSAGE "Sorry, 'Prolint output to Excel' only works with Excel version 11":T VIEW-AS ALERT-BOX.
      DELETE PROCEDURE THIS-PROCEDURE.
   END.
   ELSE DO:
      chExcel:VISIBLE = TRUE.
      logfile = substitute("&1prolint&2.xml", DYNAMIC-FUNCTION("ProlintProperty", "outputhandlers.outputdirectory"),
                                              replace(string(time, "hh:mm:ss"), ":", "")).

      OUTPUT TO VALUE(logfile).
      PUT UNFORMATTED
			'<?xml version="1.0"?>' skip
			'<?mso-application progid="Excel.Sheet"?>' skip
			'<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"' skip
			' xmlns:o="urn:schemas-microsoft-com:office:office"' skip
			' xmlns:x="urn:schemas-microsoft-com:office:excel"' skip
			' xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"' skip
			' xmlns:html="http://www.w3.org/TR/REC-html40">' skip
			' <DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">' skip
			'  <LastAuthor>prolint</LastAuthor>' skip
			' </DocumentProperties>' skip
			' <ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel">' skip
			'  <RefModeR1C1/>' skip
			'  <ProtectStructure>False</ProtectStructure>' skip
			'  <ProtectWindows>False</ProtectWindows>' skip
			' </ExcelWorkbook>' skip
			' <Styles>' skip
			'  <Style ss:ID="Default" ss:Name="Normal">' skip
			'   <Alignment ss:Vertical="Bottom"/>' skip
			'   <Borders/>' skip
			'   <Font/>' skip
			'   <Interior/>' skip
			'   <NumberFormat/>' skip
			'   <Protection/>' skip
			'  </Style>' skip
			'  <Style ss:ID="s20">' skip
			'   <Font x:Family="Swiss" ss:Bold="1"/>' skip
			'  </Style>' skip
			'  <Style ss:ID="s21">' skip
			'  </Style>' skip
			'  <Style ss:ID="s22">' skip
			'  </Style>' skip
			'  <Style ss:ID="s23">' skip
			'  </Style>' skip
			'  <Style ss:ID="s24">' skip
			'  </Style>' skip
			'  <Style ss:ID="s25">' skip
			'  </Style>' skip
			'  <Style ss:ID="s26">' skip
			'  </Style>' skip
			'  <Style ss:ID="s27">' skip
			'  </Style>' skip
			'  <Style ss:ID="s28">' skip
			'   <Font ss:Color="#FF6600"/>' skip
			'  </Style>' skip
			'  <Style ss:ID="s29">' skip
			'   <Font ss:Color="#FF0000"/>' skip
			'  </Style>' skip
			' </Styles>' skip
			' <Worksheet ss:Name="prolint">' skip
			'  <Names>' skip
			'   <NamedRange ss:Name="_FilterDatabase" ss:RefersTo="=prolint!R1C1:R1C6"' skip
			'    ss:Hidden="1"/>' skip
			'  </Names>' skip
			'  <Table>' skip
			'   <Column ss:Width="87"/>' skip
			'   <Column ss:Width="96"/>' skip
			'   <Column ss:Width="57"/>' skip
			'   <Column ss:Index="5" ss:Width="437.25"/>' skip
			'   <Row ss:StyleID="s20">' skip
			'    <Cell><Data ss:Type="String">CompilationUnit</Data><NamedCell ss:Name="_FilterDatabase"/></Cell>' skip
			'    <Cell><Data ss:Type="String">SourceFile</Data><NamedCell ss:Name="_FilterDatabase"/></Cell>' skip
			'    <Cell><Data ss:Type="String">LineNumber</Data><NamedCell ss:Name="_FilterDatabase"/></Cell>' skip
			'    <Cell><Data ss:Type="String">Severity</Data><NamedCell ss:Name="_FilterDatabase"/></Cell>' skip
			'    <Cell><Data ss:Type="String">Description</Data><NamedCell ss:Name="_FilterDatabase"/></Cell>' skip
			'    <Cell><Data ss:Type="String">RuleID</Data><NamedCell ss:Name="_FilterDatabase"/></Cell>' skip
			'   </Row>' skip
            .
      OUTPUT CLOSE.
   END.
 
END PROCEDURE.              
   
   
PROCEDURE Prolint_AddResult :              
   /* purpose: add one result from a 'rule' to the logfile, 
               using the format of your choice.
               The format in this example looks pretty useless to me */
   DEFINE INPUT PARAMETER pCompilationUnit  AS CHAR    NO-UNDO.  /* the sourcefile we're parsing          */
   DEFINE INPUT PARAMETER pSourcefile       AS CHAR    NO-UNDO.  /* may be an includefile                 */
   DEFINE INPUT PARAMETER pLineNumber       AS INTEGER NO-UNDO.  /* line number in pSourceFile            */
   DEFINE INPUT PARAMETER pDescription      AS CHAR    NO-UNDO.  /* human-readable hint                   */
   DEFINE INPUT PARAMETER pRuleID           AS CHAR    NO-UNDO.  /* defines rule-program and maps to help */
   DEFINE INPUT PARAMETER pSeverity         AS INTEGER NO-UNDO.  /* importance of this rule, scale 0-9    */

   IF chExcel<>? THEN DO:
       OUTPUT TO VALUE (logfile) APPEND.
       PUT UNFORMATTED SUBSTITUTE(
		    '   <Row ss:StyleID="s2&6">~n' +
		    '    <Cell><Data ss:Type="String">&5</Data></Cell>~n' +
		    '    <Cell><Data ss:Type="String">&1</Data></Cell>~n' +
		    '    <Cell><Data ss:Type="Number">&2</Data></Cell>~n' +
		    '    <Cell><Data ss:Type="Number">&6</Data></Cell>~n' +
		    '    <Cell><Data ss:Type="String">&3</Data></Cell>~n' +
		    '    <Cell><Data ss:Type="String">&4</Data></Cell>~n' +
		    '   </Row>~n',
            pSourceFile,
            pLineNumber,
            replace(replace(pDescription, ">", "&gt"), "<", "&gt"),
            pRuleID,
            pCompilationUnit,
            pSeverity)
            .
       OUTPUT CLOSE.
   END.

END PROCEDURE.

   
PROCEDURE Prolint_FinalizeResults :                                    
   /* purpose: close the logfile and/or show it. Free resources  */
   
   /* This procedure will not be invoked again, so it can exit */

   IF chExcel<>? THEN DO:
       OUTPUT TO VALUE (logfile) APPEND.
       PUT UNFORMATTED
		    '  </Table>' skip
		    '  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">' skip
		    '   <Selected/>' skip
		    '   <ProtectObjects>False</ProtectObjects>' skip
		    '   <ProtectScenarios>False</ProtectScenarios>' skip
		    '  </WorksheetOptions>' skip
		    '  <AutoFilter x:Range="R1C1:R1C6" xmlns="urn:schemas-microsoft-com:office:excel">' skip
		    '  </AutoFilter>' skip
		    '  <Sorting xmlns="urn:schemas-microsoft-com:office:excel">' skip
		    '   <Sort>CompilationUnit</Sort>' skip
		    '   <Sort>SourceFile</Sort>' skip
		    '   <Sort>LineNumber</Sort>' skip
		    '   <Descending/>' skip
		    '  </Sorting>' skip
		    ' </Worksheet>' skip
		    '</Workbook>' skip
		    .
       OUTPUT CLOSE.

       file-info:file-name = Logfile.
       chExcel:Workbooks:OPEN(file-info:full-pathname).

       /*  I would like to release the com-handle, but the Excel application closes when I try  :-(
           2007-07-03 tim townsend: it appears that RELEASE OBJECT works as advertized now, perhaps it was a Progress version/patch issue.  */
       RELEASE OBJECT chExcel.

   END.
   
   DELETE PROCEDURE THIS-PROCEDURE.                          
   
END PROCEDURE.

