with Ada.Strings.Wide_Unbounded;

package Command_Line_Parser_Generator is
   subtype Source_Text is Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   function "+" (Item : in Source_Text) return Wide_String
     renames Ada.Strings.Wide_Unbounded.To_Wide_String;

   function "+" (Item : in Wide_String) return Source_Text
     renames Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String;

   function Trim (Item : in Wide_String) return Wide_String;
end Command_Line_Parser_Generator;
