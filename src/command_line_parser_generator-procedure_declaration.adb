package body Command_Line_Parser_Generator.Procedure_Declaration is
   function Image (Item : in Instance) return Wide_String is
   begin
      return
        "   procedure " & (+Item.Name) & Item.Formal_Parameters.Image & ";";
   end Image;

   function Number_Of_Optional_Parameters (Item : in Instance) return Natural
   is
      Result : Natural := 0;
   begin
      for Parameter of Item.Formal_Parameters loop
         if Parameter.Has_Default_Value then
            Result := Result + 1;
         end if;
      end loop;

      return Result;
   end Number_Of_Optional_Parameters;
end Command_Line_Parser_Generator.Procedure_Declaration;
