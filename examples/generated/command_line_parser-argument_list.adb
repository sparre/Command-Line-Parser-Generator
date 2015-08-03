with Ada.Strings.Unbounded;

package body Command_Line_Parser.Argument_List is
   function Image (Item : in Instance) return String is
      use Ada.Strings.Unbounded;

      Buffer : Unbounded_String;
   begin
      for Element of Item loop
         Append (Source   => Buffer,
                 New_Item => " " & Element.Image);
      end loop;

      return To_String (Buffer);
   end Image;
end Command_Line_Parser.Argument_List;
