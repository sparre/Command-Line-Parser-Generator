package body Command_Line_Parser_Generator.Procedure_Declaration is
   function Image (Item : in Instance) return Wide_String is
   begin
      return +Item.Name & Item.Formal_Parameters.Image & ";";
   end Image;
end Command_Line_Parser_Generator.Procedure_Declaration;
