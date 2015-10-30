with Ada.Containers.Indefinite_Hashed_Sets,
     Ada.Strings.Fixed.Equal_Case_Insensitive,
     Ada.Strings.Fixed.Hash_Case_Insensitive;

with An_Application.Command_Line_Parser.Argument_List;

private
package An_Application.Command_Line_Parser.Key_List is
   package Sets is
      new Ada.Containers.Indefinite_Hashed_Sets
            (Element_Type        => String,
             Hash                => Ada.Strings.Fixed.Hash_Case_Insensitive,
             Equivalent_Elements => Ada.Strings.Fixed.Equal_Case_Insensitive);

   type Instance is new Sets.Set with null record;

   function "+" (Left  : in     Instance;
                 Right : in     String) return Instance;

   function "+" (Right : in     String) return Instance;

   function "=" (Left  : in     Argument_List.Instance;
                 Right : in     Instance) return Boolean;

end An_Application.Command_Line_Parser.Key_List;
