package body Command_Line_Parser_Generator.Formal_Parameter_List is
   function Image (Item : in Instance) return Wide_String is
      Buffer : Source_Text;

      use type Ada.Containers.Count_Type;
      use all type Source_Text;
   begin
      if Item.Length = 0 then
         return "";
      else
         Append (Source   => Buffer,
                 New_Item => Wide_String'(" ("));
         Append (Source   => Buffer,
                 New_Item => Item.First_Element.Image);

         for Index in Item.First_Index + 1 .. Item.Last_Index loop
            Append (Source   => Buffer,
                    New_Item => "; ");
            Append (Source   => Buffer,
                    New_Item => Item.Element (Index).Image);
         end loop;

         Append (Source   => Buffer,
                 New_Item => ")");

         return To_Wide_String (Buffer);
      end if;
   end Image;
end Command_Line_Parser_Generator.Formal_Parameter_List;
