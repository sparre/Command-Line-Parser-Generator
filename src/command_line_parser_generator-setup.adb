with Asis.Ada_Environments,
     Asis.Implementation;

procedure Command_Line_Parser_Generator.Setup (Context :    out Asis.Context)
is
begin
   Asis.Implementation.Initialize ("");
   Asis.Ada_Environments.Associate (The_Context => Context,
                                    Name        => "CLPG",
                                    Parameters  => "-CA -FM -Isrc -Iexamples");
   Asis.Ada_Environments.Open (The_Context => Context);
end Command_Line_Parser_Generator.Setup;
