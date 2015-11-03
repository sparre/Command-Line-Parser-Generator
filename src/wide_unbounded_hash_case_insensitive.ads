with
  Ada.Containers,
  Ada.Strings.Wide_Unbounded;

function Wide_Unbounded_Hash_Case_Insensitive
  (Key : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
  return Ada.Containers.Hash_Type;
