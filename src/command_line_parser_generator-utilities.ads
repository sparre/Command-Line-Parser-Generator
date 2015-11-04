with Asis;

with Command_Line_Parser_Generator.Identifier_Set;

package Command_Line_Parser_Generator.Utilities is
   function Defining_Name      (Item : Asis.Declaration) return Wide_String;
   function Source_Name        (Item : Asis.Declaration) return Wide_String;
   function Full_Defining_Name (Item : Asis.Declaration) return Wide_String;

   function Name_Definition (Item : Asis.Declaration) return Asis.Element;

   procedure Generate_Reader (For_Type : in     Asis.Declaration);

   function Is_Enumeration (Type_Declaration : in Asis.Declaration)
                           return Boolean;
   function Enumeration_Values (Type_Declaration : in Asis.Declaration)
                               return Identifier_Set.Instance;
end Command_Line_Parser_Generator.Utilities;
