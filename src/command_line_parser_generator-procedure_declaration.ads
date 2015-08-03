with Command_Line_Parser_Generator.Formal_Parameter_List;

package Command_Line_Parser_Generator.Procedure_Declaration is
   type Instance is tagged
      record
         Name              : Source_Text;
         Formal_Parameters : Formal_Parameter_List.Instance;
      end record;

   function Image (Item : in Instance) return Wide_String;
end Command_Line_Parser_Generator.Procedure_Declaration;
