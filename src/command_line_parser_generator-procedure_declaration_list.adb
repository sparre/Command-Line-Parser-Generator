with Ada.Characters.Conversions,
     Ada.Characters.Latin_1;

package body Command_Line_Parser_Generator.Procedure_Declaration_List is
   function Image (Item : in Instance) return Wide_String is
      Buffer : Source_Text;

      use type Ada.Containers.Count_Type;
      use all type Source_Text;
   begin
      for Element of Item loop
         Append (Source   => Buffer,
                 New_Item => Element.Image);
         Append (Source   => Buffer,
                 New_Item => Ada.Characters.Conversions.To_Wide_Character
                               (Ada.Characters.Latin_1.LF));
      end loop;

      return To_Wide_String (Buffer);
   end Image;
end Command_Line_Parser_Generator.Procedure_Declaration_List;
