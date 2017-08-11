/* -------------------------------------------------------------
   file    : lintproglistprof.p
   by      : Niek Knijnenburg
   purpose : Lint all files listed a text file to the profile also in the textfile per file
   input   : The name of the text file containing the list of files and their profiles to lint.

   The columns in the textfile should be: filename Profilename
   prolint/core/prolint.p strict
   prolint/launch/lintproglistprof.p relaxed
   etc...
   ------------------------------------------------------------- */

define input parameter pcListfile as character no-undo.

define variable gcProfilename   as character no-undo.
define variable lClearOutput    as logical   no-undo initial yes.
define variable iProfileCounter as integer   no-undo.
define variable iStarttime      as integer   no-undo.
define variable iMaxSeverity    as integer   no-undo.

define temp-table ttSourcefile no-undo
   field Sourcefile   as character  format "x(64)":U
   field Profilename  as character  format "x(15)":U
   field Processed    as logical
   index Sourcefile is primary unique Sourcefile.

assign iStartTime = etime(no).

run FillTempTable.
if not can-find(first ttSourcefile)
then do:
  message 'no files found to lint':T view-as alert-box error.
  return.
end.
for each ttSourcefile no-lock break by ttSourcefile.Profilename:
  if first-of(ttSourcefile.Profilename)
  then do:
    assign gcProfilename = ttSourcefile.Profilename
           iProfilecounter = iProfilecounter + 1.
    run prolint/core/prolint.p ("",
                                this-procedure:handle,
                                gcProfilename,
                                lClearOutput).
    assign iMaxSeverity = max(iMaxSeverity,int(return-value)) no-error.
    assign lClearOutput = no.
  end.
end.

publish "Prolint_Status_Progress":U ("done":T).
publish "Prolint_Status_FileStart":U (substitute("total time: &1 msec":T, trim(string(etime(no) - iStarttime, ">>>,>>>,>>>,>>9":U)))).

if iProfileCounter > 1
then
  publish "Prolint_Status_Profile":U (substitute('MULTIPLE(&1)':U,string(iProfileCounter))).

return string(iMaxSeverity).

procedure FillTempTable :
/*------------------------------------------------------------------------------
  reads the contents of the file named by the cListFile input parameter.
  adds a new entry to the temp table for each valid filename read.
------------------------------------------------------------------------------*/
  define variable cSourcefile  as character no-undo.
  define variable cProfilename as character no-undo.
  define variable cListfile    as character no-undo.

  assign cListFile = search(pcListfile).
  if cListfile = ?
  then do:
    message 'Listfile not found':T view-as alert-box error.
    return.
  end.
  input from value(cListFile).
  repeat:
    import cSourcefile cProfilename.
    if cSourcefile > ""
    and not can-find(ttSourcefile where ttSourcefile.Sourcefile = cSourcefile)
    then do:
      create ttSourcefile.
      assign ttSourcefile.Sourcefile  = cSourcefile
             ttSourcefile.Profilename = cProfilename.
    end.
  end.
  input close.
end procedure. /* FillTempTable */

procedure getfirstLintsource :
  /* purpose: prolint.p calls this ip to ask for the first Sourcefile to analyze.
              return ? if you don't have any Sourcefiles. */
  define output parameter pSourcefile as character no-undo.

  run getnextLintsource(output pSourcefile).

end procedure. /* getfirstLintsource */

procedure getnextLintsource :
  /* purpose: prolint.p calls this ip to ask for the next Sourcefile to analyze.
              return ? if you don't have any Sourcefiles. */
  define output parameter pSourcefile as character no-undo.

  find first ttSourcefile where ttSourcefile.Profilename = gcProfilename
                            and not ttSourcefile.Processed exclusive-lock no-error.
  if available ttSourcefile
  then
    assign pSourcefile            = ttSourcefile.Sourcefile
           ttSourcefile.Processed = yes.
  else
    assign pSourcefile = ?.

end procedure. /* getnextLintsource */
