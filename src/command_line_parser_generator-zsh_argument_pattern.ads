--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

with Command_Line_Parser_Generator.Identifier_Set;

package Command_Line_Parser_Generator.Zsh_Argument_Pattern is
   type Kinds is (Files, Directories, Enumeration, Anything, Flag);
   subtype Simple_Kinds is Kinds
     with Static_Predicate => Simple_Kinds /= Enumeration;

   type Instance is tagged private;

   function Create (Kind : in Simple_Kinds) return Instance;
   function Create_Enumeration (Values : in Identifier_Set.Instance)
                               return Instance;

   function "+" (Kind : in Simple_Kinds) return Instance renames Create;

   function Kind   (Item : in Instance) return Kinds;
   function Values (Item : in Instance) return Identifier_Set.Instance;

   function "or" (Left, Right : Instance) return Instance;

   function Image (Item : in Instance) return Wide_String;
private
   type Instance is tagged
      record
         Kind   : Kinds := Anything;
         Values : Identifier_Set.Instance;
      end record;
end Command_Line_Parser_Generator.Zsh_Argument_Pattern;
