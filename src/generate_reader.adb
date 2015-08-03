with Ada.Wide_Text_IO;

with Full_Defining_Name,
     Source_Name;

procedure Generate_Reader (For_Type : in     Asis.Declaration) is
   Package_Name : constant Wide_String := Source_Name (For_Type);
   Type_Name    : constant Wide_String := Full_Defining_Name (For_Type);

   use Ada.Wide_Text_IO;
begin
   New_Line;
   Put_Line ("----------------------");
   Put_Line ("with Ada.Command_Line,");
   Put_Line ("     Ada.Text_IO;");
   Put_Line ("with " & Package_Name & ";");
   Put_Line ("procedure Reader is");
   Put_Line ("   use Ada.Command_Line, Ada.Text_IO;");
   Put_Line ("   O : constant " & Type_Name & " := " & Type_Name &
               "'Value (Argument (1));");
   Put_Line ("begin");
   Put_Line ("   Put_Line (""" & Type_Name & ": "" & " & Type_Name &
               "'Image (O));");
   Put_Line ("end Reader;");
   Put_Line ("----------------------");
end Generate_Reader;
