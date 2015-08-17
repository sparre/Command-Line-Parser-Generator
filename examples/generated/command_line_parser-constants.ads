with Ada.Containers.Hashed_Sets;

with Command_Line_Parser.Argument_List;

private
package Command_Line_Parser.Constants is
   package Sets is new Ada.Containers.Hashed_Sets
                         (Element_Type        => Argument.Instance,
                          Equivalent_Elements => Argument.Equal_Keys,
                          "="                 => Argument."=",
                          Hash                => Argument.Key_Hash);

   type Instance is new Sets.Set with null record;

   function Image (Item : in Instance) return String;
end Command_Line_Parser.Argument_List;
