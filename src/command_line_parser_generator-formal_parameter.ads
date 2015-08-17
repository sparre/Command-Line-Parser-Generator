package Command_Line_Parser_Generator.Formal_Parameter is
   type Instance is tagged
      record
         Name           : Source_Text;
         Image_Function : Source_Text;
         Value_Function : Source_Text;
         Default_Value  : Source_Text;
         Type_Name      : Source_Text;
      end record;

   function Has_Default_Value (Item : in Instance) return Boolean;

   function Image (Item : in Instance) return Wide_String;
end Command_Line_Parser_Generator.Formal_Parameter;