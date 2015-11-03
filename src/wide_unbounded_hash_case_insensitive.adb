with
  Ada.Characters.Conversions,
  Ada.Strings.Hash_Case_Insensitive;

function Wide_Unbounded_Hash_Case_Insensitive
  (Key : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
  return Ada.Containers.Hash_Type is
begin
   return Ada.Strings.Hash_Case_Insensitive
            (Key => Ada.Characters.Conversions.To_String
                      (Ada.Strings.Wide_Unbounded.To_Wide_String (Key)));
end Wide_Unbounded_Hash_Case_Insensitive;
