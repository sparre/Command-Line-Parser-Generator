with Ada.Strings;

package Dotted_Type_Name is
   procedure Trim (Sides : Ada.Strings.Trim_End := Ada.Strings.Both);

   use all type Ada.Strings.Trim_End;

   procedure Trim_More (Prefix : String;
                        Side   : Ada.Strings.Trim_End := Both);
end Dotted_Type_Name;
