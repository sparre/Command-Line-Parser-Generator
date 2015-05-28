--  Copyright (C) 1999, 2008 Christoph Karl Walter Grein (based on an OpenToken example)
--  Copyright (C) 2015 Jacob Sparre Andersen

pragma License (Modified_GPL);

with Ada.Command_Line,
     Ada.Text_IO;

with Ada_Lexer;

with Simple_Package_Syntax; pragma Unreferenced (Simple_Package_Syntax);

procedure Parse_Package is
   use type Ada_Lexer.Ada_Token;

   File : Ada.Text_IO.File_Type;
begin
   Ada.Text_IO.Open (File => File,
                     Mode => Ada.Text_IO.In_File,
                     Name => Ada.Command_Line.Argument (1));

   Ada_Lexer.Set_Input_Feeder (File);
   Ada_Lexer.Bad_Token_on_Syntax_Error;

   loop
      Ada_Lexer.Find_Next;

      Ada.Text_IO.Put_Line
        (Item => Ada_Lexer.Ada_Token'Image (Ada_Lexer.Token_ID) & ' ' &
                 Ada_Lexer.Lexeme);

      exit when Ada_Lexer.Token_ID = Ada_Lexer.End_of_File_T;
   end loop;
end Parse_Package;
