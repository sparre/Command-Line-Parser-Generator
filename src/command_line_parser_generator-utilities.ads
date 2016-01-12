--  Copyright: JSA Research & Innovation <info@jacob-sparre.dk>
--  License:   Beer Ware
pragma License (Unrestricted);

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

   function Is_String_Compatible (Type_Declaration : in Asis.Declaration)
                                 return Boolean;
   function Is_Integer_Compatible (The_Subtypes : in Asis.Element_List)
                                  return Boolean;
   function Is_Integer_Compatible (The_Subtype : in Asis.Element)
                                  return Boolean;

   function Static_Match_With_Character
     (The_Subtype : in Asis.Declaration) return Boolean;

   function To_Ada_Source (Item : in Source_Text) return Wide_String;
end Command_Line_Parser_Generator.Utilities;
