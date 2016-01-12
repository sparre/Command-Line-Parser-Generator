--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

with
  Ada.Containers.Hashed_Sets;

with
  Wide_Unbounded_Equal_Case_Insensitive,
  Wide_Unbounded_Hash_Case_Insensitive;

package Command_Line_Parser_Generator.Identifier_Set is
   use type Source_Text;

   package Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Source_Text,
      Hash                => Wide_Unbounded_Hash_Case_Insensitive,
      Equivalent_Elements => Wide_Unbounded_Equal_Case_Insensitive);

   type Instance is new Sets.Set with null record;

   procedure Append (Container : in out Instance;
                     Value     : in     Source_Text);
end Command_Line_Parser_Generator.Identifier_Set;
