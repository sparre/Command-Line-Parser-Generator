with Ada.Text_IO;

package body Dotted_Type_Name is
   procedure Trim (Sides : Ada.Strings.Trim_End := Ada.Strings.Both) is
      use Ada.Strings, Ada.Text_IO;
   begin
      Put_Line ("   Trim (Sides => " & Trim_End'Image (Sides) & ");");
   end Trim;

   procedure Trim_More (Prefix : String;
                        Side   : Ada.Strings.Trim_End := Ada.Strings.Both) is
      use Ada.Strings, Ada.Text_IO;
   begin
      Put_Line ("   Trim_More (Prefix => """ & Prefix & """);");
      Put_Line ("   Trim_More (Side   => " & Trim_End'Image (Side) & ");");
   end Trim_More;
end Dotted_Type_Name;
