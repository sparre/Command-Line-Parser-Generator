with Bad_Declares_Function;

package Good_Procedure_Renamings is
   procedure Run (Help : in Boolean := False)
     renames Bad_Declares_Function.Run;

   procedure Batch_Mode (Help : in     Boolean := False;
                         A, B : in     Natural)
     renames Bad_Declares_Function.Batch_Mode;
end Good_Procedure_Renamings;
