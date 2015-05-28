with Ada.Text_IO;

with Simple_Package_Syntax;

procedure Parse_Package is
   use Ada.Text_IO;
begin
   Put_Line ("Parsing standard input ...");

   declare
      use Simple_Package_Syntax;

      Parser : LALR_Parsers.Instance :=
        LALR_Parsers.Initialize
          (Analyzer => Analyzer,
           Table    => LALR_Generators.Generate
                         (Grammar              => Grammar,
                          Ignore_Unused_Tokens => True));
   begin
      LALR_Parsers.Parse (Parser);
   end;

   Put_Line ("Passed.");
end Parse_Package;
