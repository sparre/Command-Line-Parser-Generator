with Ada.Strings.Wide_Fixed;

function Trim (Item : in Wide_String) return Wide_String is
   use Ada.Strings;
begin
   return Wide_Fixed.Trim (Item, Both);
end Trim;
