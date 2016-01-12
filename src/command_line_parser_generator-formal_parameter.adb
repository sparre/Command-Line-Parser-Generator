--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

with Ada.Strings.Wide_Unbounded;

package body Command_Line_Parser_Generator.Formal_Parameter is
   function Has_Default_Value (Item : in Instance) return Boolean is
      use Ada.Strings.Wide_Unbounded;
   begin
      return Item.Default_Value /= Null_Unbounded_Wide_String;
   end Has_Default_Value;

   function Image (Item : in Instance) return Wide_String is
   begin
      if Item.Has_Default_Value then
         return +Item.Name & " : in     " & (+Item.Type_Name) &
           " := " & (+Item.Default_Value);
      else
         return +Item.Name & " : in     " & (+Item.Type_Name);
      end if;
   end Image;
end Command_Line_Parser_Generator.Formal_Parameter;
