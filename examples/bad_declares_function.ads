package Bad_Declares_Function is
   procedure Run (Help : in Boolean := False);
   procedure Batch_Mode (Help : in     Boolean := False;
                         A, B : in     Natural);
   function Help_Text return String;
end Bad_Declares_Function;
