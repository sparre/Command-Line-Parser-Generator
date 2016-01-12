--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

with
  Ada.Characters.Conversions,
  Ada.Strings.Equal_Case_Insensitive;

function Wide_Unbounded_Equal_Case_Insensitive
  (Left, Right : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
  return Boolean
is
begin
   return Ada.Strings.Equal_Case_Insensitive
            (Left  => Ada.Characters.Conversions.To_String
                        (Ada.Strings.Wide_Unbounded.To_Wide_String (Left)),
             Right => Ada.Characters.Conversions.To_String
                        (Ada.Strings.Wide_Unbounded.To_Wide_String (Right)));
end Wide_Unbounded_Equal_Case_Insensitive;
