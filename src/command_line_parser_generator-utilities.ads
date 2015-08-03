with Asis;

package Command_Line_Parser_Generator.Utilities is
   function Defining_Name      (Item : Asis.Declaration) return Wide_String;
   function Source_Name        (Item : Asis.Declaration) return Wide_String;
   function Full_Defining_Name (Item : Asis.Declaration) return Wide_String;

   procedure Generate_Reader (For_Type : in     Asis.Declaration);
end Command_Line_Parser_Generator.Utilities;
