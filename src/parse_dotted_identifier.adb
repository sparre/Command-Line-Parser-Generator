with Ada.Text_IO;

with Dotted_Identifier_Syntax;

procedure Parse_Dotted_Identifier is
   use Ada.Text_IO;
begin
   Put_Line ("Parsing standard input ...");

   declare
      use Dotted_Identifier_Syntax;

      Parser : LALR_Parsers.Instance :=
        LALR_Parsers.Initialize
          (Analyzer => Analyzer,
           Table    => LALR_Generators.Generate
                         (Grammar              => Grammar,
                          Trace                => False,
                          Put_Parse_Table      => False,
                          Ignore_Unused_Tokens => True));
   begin
      LALR_Parsers.Parse (Parser);
   end;

   Put_Line ("Passed.");
end Parse_Dotted_Identifier;
