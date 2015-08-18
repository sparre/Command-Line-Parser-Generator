with Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Strings.Fixed.Equal_Case_Insensitive,
     Ada.Strings.Fixed.Hash_Case_Insensitive;

with An_Application.Command_Line_Parser.Argument;

private
package An_Application.Command_Line_Parser.Argument_List is
   package Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
            (Element_Type    => String,
             Key_Type        => String,
             Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive,
             Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive);

   type Instance is new Maps.Map with null record;

   function Image (Container : in     Instance) return String;

   procedure Insert (Container : in out Instance;
                     New_Item  : in     Argument.Instance);
end An_Application.Command_Line_Parser.Argument_List;
