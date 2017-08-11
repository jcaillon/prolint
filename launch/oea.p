/* -------------------------------------------------------------
   file    : prolint/launch/eclipse.p
   by      : Jurjen Dijkstra
   purpose : launch prolint.p from OpenEdge Architect (=the Eclipse plugin)
   ------------------------------------------------------------- */

   define input parameter OAparameters as character no-undo.
   define variable activefilename as character no-undo.
   define variable isrunning as logical no-undo initial false.
   
   if num-entries(OAParameters, chr(3)) > 1 then do:
      activefilename = entry(2, OAParameters, chr(3)).
   
      /* show the results window */
      PUBLISH "Prolint_outputhandler_oea":U (OUTPUT isrunning).
      IF NOT isrunning THEN
         RUN prolint/outputhandlers/oea_results.w PERSISTENT.


      /* start parsing */
      RUN prolint/core/prolint.p  (activefilename,
                                   ?,
                                   "oea-ide",
                                   true).    
								   
   end.								   
                          
						  
                          
           
           
