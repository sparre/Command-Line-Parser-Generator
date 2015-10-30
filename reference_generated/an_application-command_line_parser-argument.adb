with Ada.Strings.Fixed,
     Ada.Strings.Unbounded.Equal_Case_Insensitive,
     Ada.Strings.Unbounded.Hash_Case_Insensitive;

package body An_Application.Command_Line_Parser.Argument is
   function "+" (Item : in String)
                return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   overriding
   function "=" (Left, Right : in Instance) return Boolean is
      use Ada.Strings.Unbounded;
   begin
      return Equal_Keys (Left, Right) and Left.Value = Right.Value;
   end "=";

   function Compose (Key   : in String;
                     Value : in String) return Instance is
   begin
      return (Key   => +Key,
              Value => +Value);
   end Compose;

   function Equal_Keys (Left, Right : in Instance) return Boolean is
      use Ada.Strings.Unbounded;
   begin
      return Equal_Case_Insensitive (Left.Key, Right.Key);
   end Equal_Keys;

   function Image (Item : in Instance) return String is
      use Ada.Strings.Unbounded;
   begin
      return "--" & To_String (Item.Key) & "=" & To_String (Item.Value);
   end Image;

   function Key_Hash (Item : in Instance) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Unbounded.Hash_Case_Insensitive (Item.Key);
   end Key_Hash;

   function Value (Item : in String) return Instance is
      use Ada.Strings.Fixed;
      Key_Value_Separator : Natural;
   begin
      if Head (Item, 2) = "--" then
         Key_Value_Separator := Index (Item, "=");

         if Key_Value_Separator = 0 then
            return (Key   => +Item (Item'First + 2 .. Item'Last),
                    Value => +"True");
         else
            return (Key   => +Item (Item'First + 2 .. Key_Value_Separator - 1),
                    Value => +Item (Key_Value_Separator + 1 .. Item'Last));
         end if;
      else
         raise Constraint_Error
           with "Incorrect argument format.  Expected: --<key>=<value>";
      end if;
   end Value;
end An_Application.Command_Line_Parser.Argument;
