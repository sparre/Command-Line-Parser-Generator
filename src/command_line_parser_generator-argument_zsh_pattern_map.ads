--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

with
  Ada.Containers.Hashed_Maps;

with
  Command_Line_Parser_Generator.Zsh_Argument_Pattern,
  Wide_Unbounded_Equal_Case_Insensitive,
  Wide_Unbounded_Hash_Case_Insensitive;

package Command_Line_Parser_Generator.Argument_Zsh_Pattern_Map is
   use type Source_Text;
   use type Zsh_Argument_Pattern.Instance;

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Source_Text,
      Element_Type    => Zsh_Argument_Pattern.Instance,
      Hash            => Wide_Unbounded_Hash_Case_Insensitive,
      Equivalent_Keys => Wide_Unbounded_Equal_Case_Insensitive);

   type Instance is new Maps.Map with null record;

   procedure Append (Container : in out Instance;
                     Key       : in     Source_Text;
                     Pattern   : in     Zsh_Argument_Pattern.Instance);
end Command_Line_Parser_Generator.Argument_Zsh_Pattern_Map;
