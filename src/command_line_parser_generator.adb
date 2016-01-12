--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

with Ada.Strings.Wide_Fixed;

package body Command_Line_Parser_Generator is
   function Trim (Item : in Wide_String) return Wide_String is
      use Ada.Strings;
   begin
      return Wide_Fixed.Trim (Item, Both);
   end Trim;
end Command_Line_Parser_Generator;
