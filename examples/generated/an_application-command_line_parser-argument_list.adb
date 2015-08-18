with Ada.Strings.Unbounded;

package body An_Application.Command_Line_Parser.Argument_List is
   function Image (Container : in     Instance) return String is
      use Ada.Strings.Unbounded;

      Buffer : Unbounded_String;
      Index  : Maps.Cursor := Container.First;

      use type Maps.Cursor;
   begin
      while Index /= Maps.No_Element loop
         Append
           (Source   => Buffer,
            New_Item => " " & Argument.Compose
                                (Key   => Maps.Key (Index),
                                 Value => Maps.Element (Index)).Image);

         Maps.Next (Index);
      end loop;

      return To_String (Buffer);
   end Image;

   procedure Insert (Container : in out Instance;
                     New_Item  : in     Argument.Instance) is
      use Ada.Strings.Unbounded;
   begin
      Container.Insert (Key      => To_String (New_Item.Key),
                        New_Item => To_String (New_Item.Value));
   end Insert;
end An_Application.Command_Line_Parser.Argument_List;
