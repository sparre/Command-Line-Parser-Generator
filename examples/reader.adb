with Ada.Command_Line,
     Ada.Text_IO;
with Subtype_Chains_2;
procedure Reader is
   use Ada.Command_Line, Ada.Text_IO;
   O : constant Subtype_Chains_2.Two := Subtype_Chains_2.Two'Value (Argument (1));
begin
   Put_Line ("Subtype_Chains_2.Two: " & Subtype_Chains_2.Two'Image (O));
end Reader;
