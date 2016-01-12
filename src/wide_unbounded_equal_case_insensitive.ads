--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

with Ada.Strings.Wide_Unbounded;

function Wide_Unbounded_Equal_Case_Insensitive
  (Left, Right : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
  return Boolean;
