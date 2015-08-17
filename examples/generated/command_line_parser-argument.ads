with Ada.Containers,
     Ada.Strings.Unbounded;

private
package Command_Line_Parser.Argument is
   type Instance is tagged
      record
         Key   : Ada.Strings.Unbounded.Unbounded_String;
         Value : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding
   function "=" (Left, Right : in Instance) return Boolean;

   function Compose (Key   : in String;
                     Value : in String) return Instance;

   function Equal_Keys (Left, Right : in Instance) return Boolean;
   function Key_Hash (Item : in Instance) return Ada.Containers.Hash_Type;

   function Image (Item : in Instance) return String;
   function Value (Item : in String) return Instance;
end Command_Line_Parser.Argument;
