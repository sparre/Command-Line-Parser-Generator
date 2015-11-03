with
  Ada.Containers.Hashed_Maps;

with
  Command_Line_Parser_Generator.Identifier_Set,
  Wide_Unbounded_Equal_Case_Insensitive,
  Wide_Unbounded_Hash_Case_Insensitive;

package Command_Line_Parser_Generator.Identifier_Matrix is
   use type Source_Text;
   use type Identifier_Set.Instance;

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Source_Text,
      Element_Type    => Identifier_Set.Instance,
      Hash            => Wide_Unbounded_Hash_Case_Insensitive,
      Equivalent_Keys => Wide_Unbounded_Equal_Case_Insensitive);

   type Instance is new Maps.Map with null record;

   procedure Append (Container : in out Instance;
                     Key       : in     Source_Text;
                     Value     : in     Source_Text);
end Command_Line_Parser_Generator.Identifier_Matrix;
